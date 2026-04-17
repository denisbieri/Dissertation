from __future__ import annotations

import json
import sqlite3
from collections import defaultdict
from contextlib import closing
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile

import altair as alt
import pandas as pd
import streamlit as st
from dateutil.relativedelta import relativedelta

DB_PATH = Path(__file__).with_name("med_tracker.db")
APP_DIR = Path(__file__).resolve().parent
ZIP_PATH = APP_DIR.parent / "medbestand_app.zip"

WEEKDAY_LABELS = {
    0: "Mo",
    1: "Di",
    2: "Mi",
    3: "Do",
    4: "Fr",
    5: "Sa",
    6: "So",
}

SCHEDULE_MODE_LABELS = {
    "weekly": "Feste Wochentage",
    "cycle": "Zyklus (z. B. 5 Tage Einnahme / 2 Tage Pause)",
}


@dataclass
class PausePeriod:
    id: int
    medication_id: int
    start_date: date
    end_date: date
    note: str


@dataclass
class StockMovement:
    id: int
    medication_id: int
    movement_date: date
    quantity: float
    note: str
    created_at: str


@dataclass
class ScheduleChange:
    id: int
    medication_id: int
    start_date: date
    schedule_mode: str
    intake_weekdays: list[int]
    cycle_take_days: int
    cycle_pause_days: int
    cycle_start_date: date | None
    created_at: str


# ---------- Datenbank ----------
def get_conn() -> sqlite3.Connection:
    conn = sqlite3.connect(DB_PATH, check_same_thread=False)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA foreign_keys = ON")
    return conn


def _ensure_column(conn: sqlite3.Connection, table: str, column: str, ddl: str) -> None:
    existing_columns = {
        row["name"] for row in conn.execute(f"PRAGMA table_info({table})").fetchall()
    }
    if column not in existing_columns:
        conn.execute(f"ALTER TABLE {table} ADD COLUMN {column} {ddl}")


def _touch_medication(conn: sqlite3.Connection, medication_id: int) -> None:
    now_iso = datetime.now().isoformat(timespec="seconds")
    conn.execute(
        "UPDATE medications SET last_checked = ? WHERE id = ?",
        (now_iso, medication_id),
    )


def _touch_master_data(conn: sqlite3.Connection, medication_id: int) -> None:
    now_iso = datetime.now().isoformat(timespec="seconds")
    conn.execute(
        "UPDATE medications SET master_data_updated_at = ?, last_checked = ? WHERE id = ?",
        (now_iso, now_iso, medication_id),
    )


def _migrate_legacy_incoming_stock(conn: sqlite3.Connection) -> None:
    columns = {row["name"] for row in conn.execute("PRAGMA table_info(medications)").fetchall()}
    if "incoming_tablets" not in columns:
        return

    rows = conn.execute(
        "SELECT id, initial_tablets, incoming_tablets FROM medications WHERE COALESCE(incoming_tablets, 0) != 0"
    ).fetchall()
    for row in rows:
        merged_stock = round(float(row["initial_tablets"] or 0) + float(row["incoming_tablets"] or 0), 2)
        conn.execute(
            "UPDATE medications SET initial_tablets = ?, incoming_tablets = 0 WHERE id = ?",
            (merged_stock, row["id"]),
        )


def init_db() -> None:
    with closing(get_conn()) as conn:
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS medications (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                person_name TEXT NOT NULL,
                birth_date TEXT NOT NULL,
                medication_name TEXT NOT NULL,
                prescription_issue_date TEXT NOT NULL,
                validity_months INTEGER NOT NULL DEFAULT 0,
                pharmacy_reserved_packs INTEGER NOT NULL DEFAULT 0,
                tablets_at_home REAL NOT NULL DEFAULT 0,
                incoming_tablets REAL NOT NULL DEFAULT 0,
                initial_tablets REAL NOT NULL DEFAULT 0,
                tablets_per_day REAL NOT NULL DEFAULT 0,
                intake_weekdays TEXT NOT NULL DEFAULT '[]',
                extra_pause_days INTEGER NOT NULL DEFAULT 0,
                schedule_mode TEXT NOT NULL DEFAULT 'weekly',
                cycle_take_days INTEGER NOT NULL DEFAULT 0,
                cycle_pause_days INTEGER NOT NULL DEFAULT 0,
                cycle_start_date TEXT,
                notes TEXT NOT NULL DEFAULT '',
                last_checked TEXT NOT NULL,
                master_data_updated_at TEXT NOT NULL DEFAULT '',
                created_at TEXT NOT NULL
            )
            """
        )
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS pause_periods (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                medication_id INTEGER NOT NULL,
                start_date TEXT NOT NULL,
                end_date TEXT NOT NULL,
                note TEXT NOT NULL DEFAULT '',
                FOREIGN KEY (medication_id) REFERENCES medications (id) ON DELETE CASCADE
            )
            """
        )
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS stock_movements (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                medication_id INTEGER NOT NULL,
                movement_date TEXT NOT NULL,
                quantity REAL NOT NULL,
                note TEXT NOT NULL DEFAULT '',
                created_at TEXT NOT NULL,
                FOREIGN KEY (medication_id) REFERENCES medications (id) ON DELETE CASCADE
            )
            """
        )
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS schedule_changes (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                medication_id INTEGER NOT NULL,
                start_date TEXT NOT NULL,
                schedule_mode TEXT NOT NULL DEFAULT 'weekly',
                intake_weekdays TEXT NOT NULL DEFAULT '[]',
                cycle_take_days INTEGER NOT NULL DEFAULT 0,
                cycle_pause_days INTEGER NOT NULL DEFAULT 0,
                cycle_start_date TEXT,
                created_at TEXT NOT NULL,
                FOREIGN KEY (medication_id) REFERENCES medications (id) ON DELETE CASCADE
            )
            """
        )

        _ensure_column(conn, "medications", "schedule_mode", "TEXT NOT NULL DEFAULT 'weekly'")
        _ensure_column(conn, "medications", "cycle_take_days", "INTEGER NOT NULL DEFAULT 0")
        _ensure_column(conn, "medications", "cycle_pause_days", "INTEGER NOT NULL DEFAULT 0")
        _ensure_column(conn, "medications", "cycle_start_date", "TEXT")
        _ensure_column(conn, "medications", "incoming_tablets", "REAL NOT NULL DEFAULT 0")
        _ensure_column(conn, "medications", "extra_pause_days", "INTEGER NOT NULL DEFAULT 0")
        _ensure_column(conn, "medications", "initial_tablets", "REAL NOT NULL DEFAULT 0")
        _ensure_column(conn, "medications", "master_data_updated_at", "TEXT NOT NULL DEFAULT ''")

        _migrate_legacy_incoming_stock(conn)

        rows = conn.execute(
            "SELECT id, created_at, last_checked, tablets_at_home, initial_tablets FROM medications"
        ).fetchall()
        for row in rows:
            if float(row["initial_tablets"] or 0) == 0 and float(row["tablets_at_home"] or 0) != 0:
                conn.execute(
                    "UPDATE medications SET initial_tablets = ? WHERE id = ?",
                    (round(float(row["tablets_at_home"] or 0), 2), row["id"]),
                )
            needs_master = not str(row["last_checked"] or "").strip()
            fallback = row["last_checked"] or row["created_at"] or datetime.now().isoformat(timespec="seconds")
            current = conn.execute(
                "SELECT master_data_updated_at FROM medications WHERE id = ?",
                (row["id"],),
            ).fetchone()
            if current and not str(current["master_data_updated_at"] or "").strip():
                conn.execute(
                    "UPDATE medications SET master_data_updated_at = ? WHERE id = ?",
                    (fallback, row["id"]),
                )
            elif needs_master:
                conn.execute(
                    "UPDATE medications SET last_checked = ?, master_data_updated_at = ? WHERE id = ?",
                    (fallback, fallback, row["id"]),
                )

        conn.commit()


def ensure_initial_schedule_history() -> None:
    with closing(get_conn()) as conn:
        rows = conn.execute("SELECT * FROM medications").fetchall()
        for row in rows:
            existing = conn.execute(
                "SELECT 1 FROM schedule_changes WHERE medication_id = ? LIMIT 1",
                (row["id"],),
            ).fetchone()
            if existing:
                continue

            prescription_issue_date = row["prescription_issue_date"]
            if (row["schedule_mode"] or "weekly") == "cycle":
                cycle_start_date = row["cycle_start_date"] or prescription_issue_date
            else:
                cycle_start_date = None
            now_iso = datetime.now().isoformat(timespec="seconds")

            conn.execute(
                """
                INSERT INTO schedule_changes (
                    medication_id, start_date, schedule_mode, intake_weekdays,
                    cycle_take_days, cycle_pause_days, cycle_start_date, created_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    row["id"],
                    prescription_issue_date,
                    row["schedule_mode"] or "weekly",
                    row["intake_weekdays"] or "[]",
                    int(row["cycle_take_days"] or 0),
                    int(row["cycle_pause_days"] or 0),
                    cycle_start_date,
                    now_iso,
                ),
            )

        conn.commit()


def list_medications() -> list[sqlite3.Row]:
    with closing(get_conn()) as conn:
        rows = conn.execute(
            "SELECT * FROM medications ORDER BY person_name COLLATE NOCASE, medication_name COLLATE NOCASE"
        ).fetchall()
    return rows


def get_medication(medication_id: int) -> sqlite3.Row | None:
    with closing(get_conn()) as conn:
        row = conn.execute("SELECT * FROM medications WHERE id = ?", (medication_id,)).fetchone()
    return row


def _normalized_medication_payload(data: dict) -> dict:
    schedule_mode = data["schedule_mode"]
    prescription_issue_date = data["prescription_issue_date"]
    raw_cycle_start_date = data["cycle_start_date"] or None

    if schedule_mode == "cycle":
        cycle_start_date = raw_cycle_start_date or prescription_issue_date
    else:
        cycle_start_date = None

    return {
        "person_name": data["person_name"].strip(),
        "birth_date": data["birth_date"],
        "medication_name": data["medication_name"].strip(),
        "prescription_issue_date": prescription_issue_date,
        "validity_months": int(data["validity_months"]),
        "pharmacy_reserved_packs": int(data["pharmacy_reserved_packs"]),
        "initial_tablets": round(float(data["initial_tablets"]), 2),
        "tablets_per_day": round(float(data["tablets_per_day"]), 2),
        "intake_weekdays": sorted(int(x) for x in data["intake_weekdays"]),
        "schedule_mode": schedule_mode,
        "cycle_take_days": int(data["cycle_take_days"]),
        "cycle_pause_days": int(data["cycle_pause_days"]),
        "cycle_start_date": cycle_start_date,
        "notes": data["notes"].strip(),
    }


def _row_as_normalized_payload(row: sqlite3.Row) -> dict:
    schedule_mode = row["schedule_mode"] or "weekly"
    if schedule_mode == "cycle":
        cycle_start_date = row["cycle_start_date"] or row["prescription_issue_date"]
    else:
        cycle_start_date = None

    return {
        "person_name": (row["person_name"] or "").strip(),
        "birth_date": row["birth_date"],
        "medication_name": (row["medication_name"] or "").strip(),
        "prescription_issue_date": row["prescription_issue_date"],
        "validity_months": int(row["validity_months"] or 0),
        "pharmacy_reserved_packs": int(row["pharmacy_reserved_packs"] or 0),
        "initial_tablets": round(float(row["initial_tablets"] or 0), 2),
        "tablets_per_day": round(float(row["tablets_per_day"] or 0), 2),
        "intake_weekdays": sorted(int(x) for x in json.loads(row["intake_weekdays"] or "[]")),
        "schedule_mode": schedule_mode,
        "cycle_take_days": int(row["cycle_take_days"] or 0),
        "cycle_pause_days": int(row["cycle_pause_days"] or 0),
        "cycle_start_date": cycle_start_date,
        "notes": (row["notes"] or "").strip(),
    }


def save_medication(data: dict, medication_id: int | None = None) -> bool:
    payload = _normalized_medication_payload(data)
    now_iso = datetime.now().isoformat(timespec="seconds")

    with closing(get_conn()) as conn:
        if medication_id is None:
            conn.execute(
                """
                INSERT INTO medications (
                    person_name, birth_date, medication_name,
                    prescription_issue_date, validity_months,
                    pharmacy_reserved_packs, tablets_at_home, incoming_tablets,
                    initial_tablets, tablets_per_day, intake_weekdays, extra_pause_days,
                    schedule_mode, cycle_take_days, cycle_pause_days, cycle_start_date,
                    notes, last_checked, master_data_updated_at, created_at
                ) VALUES (?, ?, ?, ?, ?, ?, 0, 0, ?, ?, ?, 0, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    payload["person_name"],
                    payload["birth_date"],
                    payload["medication_name"],
                    payload["prescription_issue_date"],
                    payload["validity_months"],
                    payload["pharmacy_reserved_packs"],
                    payload["initial_tablets"],
                    payload["tablets_per_day"],
                    json.dumps(payload["intake_weekdays"]),
                    payload["schedule_mode"],
                    payload["cycle_take_days"],
                    payload["cycle_pause_days"],
                    payload["cycle_start_date"],
                    payload["notes"],
                    now_iso,
                    now_iso,
                    now_iso,
                ),
            )
            new_id = conn.execute("SELECT last_insert_rowid()").fetchone()[0]
            conn.execute(
                """
                INSERT INTO schedule_changes (
                    medication_id, start_date, schedule_mode, intake_weekdays,
                    cycle_take_days, cycle_pause_days, cycle_start_date, created_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    new_id,
                    payload["prescription_issue_date"],
                    payload["schedule_mode"],
                    json.dumps(payload["intake_weekdays"]),
                    payload["cycle_take_days"],
                    payload["cycle_pause_days"],
                    payload["cycle_start_date"] or payload["prescription_issue_date"],
                    now_iso,
                ),
            )
            conn.commit()
            return True

        existing = conn.execute("SELECT * FROM medications WHERE id = ?", (medication_id,)).fetchone()
        if existing is None:
            raise ValueError("Der Eintrag wurde nicht gefunden.")

        if payload == _row_as_normalized_payload(existing):
            return False

        conn.execute(
            """
            UPDATE medications
            SET person_name = ?,
                birth_date = ?,
                medication_name = ?,
                prescription_issue_date = ?,
                validity_months = ?,
                pharmacy_reserved_packs = ?,
                initial_tablets = ?,
                tablets_per_day = ?,
                intake_weekdays = ?,
                schedule_mode = ?,
                cycle_take_days = ?,
                cycle_pause_days = ?,
                cycle_start_date = ?,
                notes = ?,
                master_data_updated_at = ?,
                last_checked = ?
            WHERE id = ?
            """,
            (
                payload["person_name"],
                payload["birth_date"],
                payload["medication_name"],
                payload["prescription_issue_date"],
                payload["validity_months"],
                payload["pharmacy_reserved_packs"],
                payload["initial_tablets"],
                payload["tablets_per_day"],
                json.dumps(payload["intake_weekdays"]),
                payload["schedule_mode"],
                payload["cycle_take_days"],
                payload["cycle_pause_days"],
                payload["cycle_start_date"],
                payload["notes"],
                now_iso,
                now_iso,
                medication_id,
            ),
        )
        conn.commit()
        return True


def delete_medication(medication_id: int) -> None:
    with closing(get_conn()) as conn:
        conn.execute("DELETE FROM medications WHERE id = ?", (medication_id,))
        conn.commit()


# ---------- Hilfsfunktionen ----------
def parse_iso_date(value: str) -> date:
    return datetime.strptime(value, "%Y-%m-%d").date()


def parse_iso_datetime(value: str) -> datetime:
    return datetime.fromisoformat(value)


def format_date_de(value: date | None) -> str:
    if not value:
        return "-"
    return value.strftime("%d.%m.%Y")


def format_datetime_de(value: str | None) -> str:
    if not value:
        return "-"
    return parse_iso_datetime(value).strftime("%d.%m.%Y %H:%M")


def clean_text(value: object) -> str:
    if value is None:
        return ""
    return str(value).strip()


def add_months(start: date, months: int) -> date:
    return start + relativedelta(months=months)


def weekdays_to_text(weekdays: list[int]) -> str:
    weekdays = sorted(set(int(day) for day in weekdays))
    if not weekdays:
        return "-"
    return ", ".join(WEEKDAY_LABELS[day] for day in weekdays)


def cycle_to_text(take_days: int, pause_days: int, cycle_start_date: date | None) -> str:
    if take_days <= 0:
        return "-"
    text = f"{take_days} Tage Einnahme / {pause_days} Tage Pause"
    if cycle_start_date:
        text += f" ab {format_date_de(cycle_start_date)}"
    return text


def schedule_to_text(
    schedule_mode: str,
    intake_weekdays: list[int],
    cycle_take_days: int,
    cycle_pause_days: int,
    cycle_start_date: date | None,
) -> str:
    if schedule_mode == "cycle":
        return cycle_to_text(cycle_take_days, cycle_pause_days, cycle_start_date)
    return weekdays_to_text(intake_weekdays)


def list_pause_periods(medication_id: int) -> list[PausePeriod]:
    with closing(get_conn()) as conn:
        rows = conn.execute(
            """
            SELECT * FROM pause_periods
            WHERE medication_id = ?
            ORDER BY start_date, end_date, id
            """,
            (medication_id,),
        ).fetchall()
    return [
        PausePeriod(
            id=row["id"],
            medication_id=row["medication_id"],
            start_date=parse_iso_date(row["start_date"]),
            end_date=parse_iso_date(row["end_date"]),
            note=row["note"],
        )
        for row in rows
    ]


def add_pause_period(medication_id: int, start_date: date, end_date: date, note: str) -> None:
    with closing(get_conn()) as conn:
        overlap = conn.execute(
            """
            SELECT 1
            FROM pause_periods
            WHERE medication_id = ?
              AND start_date <= ?
              AND end_date >= ?
            LIMIT 1
            """,
            (medication_id, end_date.isoformat(), start_date.isoformat()),
        ).fetchone()
        if overlap:
            raise ValueError("Die Pause überschneidet sich mit einer bereits erfassten Pause.")

        conn.execute(
            """
            INSERT INTO pause_periods (medication_id, start_date, end_date, note)
            VALUES (?, ?, ?, ?)
            """,
            (medication_id, start_date.isoformat(), end_date.isoformat(), note.strip()),
        )
        _touch_medication(conn, medication_id)
        conn.commit()


def delete_pause_period(pause_id: int, medication_id: int) -> None:
    with closing(get_conn()) as conn:
        conn.execute("DELETE FROM pause_periods WHERE id = ?", (pause_id,))
        _touch_medication(conn, medication_id)
        conn.commit()


def list_stock_movements(medication_id: int) -> list[StockMovement]:
    with closing(get_conn()) as conn:
        rows = conn.execute(
            """
            SELECT * FROM stock_movements
            WHERE medication_id = ?
            ORDER BY movement_date DESC, id DESC
            """,
            (medication_id,),
        ).fetchall()
    return [
        StockMovement(
            id=row["id"],
            medication_id=row["medication_id"],
            movement_date=parse_iso_date(row["movement_date"]),
            quantity=float(row["quantity"]),
            note=row["note"],
            created_at=row["created_at"],
        )
        for row in rows
    ]


def list_schedule_changes(medication_id: int) -> list[ScheduleChange]:
    with closing(get_conn()) as conn:
        rows = conn.execute(
            """
            SELECT * FROM schedule_changes
            WHERE medication_id = ?
            ORDER BY start_date ASC, id ASC
            """,
            (medication_id,),
        ).fetchall()

    return [
        ScheduleChange(
            id=row["id"],
            medication_id=row["medication_id"],
            start_date=parse_iso_date(row["start_date"]),
            schedule_mode=row["schedule_mode"],
            intake_weekdays=sorted(int(x) for x in json.loads(row["intake_weekdays"] or "[]")),
            cycle_take_days=int(row["cycle_take_days"] or 0),
            cycle_pause_days=int(row["cycle_pause_days"] or 0),
            cycle_start_date=parse_iso_date(row["cycle_start_date"]) if row["cycle_start_date"] else None,
            created_at=row["created_at"],
        )
        for row in rows
    ]


def add_schedule_change(
    medication_id: int,
    start_date: date,
    schedule_mode: str,
    intake_weekdays: list[int],
    cycle_take_days: int,
    cycle_pause_days: int,
    cycle_start_date: date | None,
) -> None:
    now_iso = datetime.now().isoformat(timespec="seconds")
    if schedule_mode == "cycle":
        cycle_start_date = cycle_start_date or start_date
    else:
        cycle_start_date = None

    with closing(get_conn()) as conn:
        latest_change = conn.execute(
            "SELECT start_date FROM schedule_changes WHERE medication_id = ? ORDER BY start_date DESC, id DESC LIMIT 1",
            (medication_id,),
        ).fetchone()
        if latest_change and start_date <= parse_iso_date(latest_change["start_date"]):
            raise ValueError("Rhythmuswechsel müssen nach dem zuletzt gespeicherten Wechsel liegen.")

        conn.execute(
            """
            INSERT INTO schedule_changes (
                medication_id, start_date, schedule_mode, intake_weekdays,
                cycle_take_days, cycle_pause_days, cycle_start_date, created_at
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                medication_id,
                start_date.isoformat(),
                schedule_mode,
                json.dumps(sorted(int(x) for x in intake_weekdays)),
                int(cycle_take_days),
                int(cycle_pause_days),
                cycle_start_date.isoformat() if cycle_start_date else None,
                now_iso,
            ),
        )
        conn.execute(
            """
            UPDATE medications
            SET schedule_mode = ?,
                intake_weekdays = ?,
                cycle_take_days = ?,
                cycle_pause_days = ?,
                cycle_start_date = ?,
                last_checked = ?
            WHERE id = ?
            """,
            (
                schedule_mode,
                json.dumps(sorted(int(x) for x in intake_weekdays)),
                int(cycle_take_days),
                int(cycle_pause_days),
                cycle_start_date.isoformat() if cycle_start_date else None,
                now_iso,
                medication_id,
            ),
        )
        conn.commit()


def is_pause_day(day: date, pauses: list[PausePeriod]) -> bool:
    return any(pause.start_date <= day <= pause.end_date for pause in pauses)


def is_scheduled_intake_day(
    day: date,
    schedule_mode: str,
    intake_weekdays: list[int],
    cycle_take_days: int,
    cycle_pause_days: int,
    cycle_start_date: date | None,
) -> bool:
    if schedule_mode == "cycle":
        if cycle_take_days <= 0 or cycle_start_date is None or day < cycle_start_date:
            return False
        cycle_length = cycle_take_days + max(0, cycle_pause_days)
        if cycle_length <= 0:
            return False
        delta_days = (day - cycle_start_date).days
        return (delta_days % cycle_length) < cycle_take_days

    weekdays = set(int(day_idx) for day_idx in intake_weekdays)
    return bool(weekdays) and day.weekday() in weekdays


def daterange(start_date: date, end_date: date):
    current = start_date
    while current <= end_date:
        yield current
        current += timedelta(days=1)


def get_schedule_for_day(day: date, schedule_changes: list[ScheduleChange]) -> ScheduleChange | None:
    valid_changes = [change for change in schedule_changes if change.start_date <= day]
    if not valid_changes:
        return None
    return valid_changes[-1]


def daily_consumption_for_day(
    day: date,
    tablets_per_day: float,
    pauses: list[PausePeriod],
    schedule_changes: list[ScheduleChange],
) -> float:
    if is_pause_day(day, pauses):
        return 0.0

    schedule = get_schedule_for_day(day, schedule_changes)
    if schedule is None:
        return 0.0

    if is_scheduled_intake_day(
        day,
        schedule.schedule_mode,
        schedule.intake_weekdays,
        schedule.cycle_take_days,
        schedule.cycle_pause_days,
        schedule.cycle_start_date,
    ):
        return round(float(tablets_per_day), 2)
    return 0.0


def consumption_between(
    start_date: date,
    end_date: date,
    tablets_per_day: float,
    pauses: list[PausePeriod],
    schedule_changes: list[ScheduleChange],
) -> float:
    if end_date < start_date:
        return 0.0

    total = 0.0
    for day in daterange(start_date, end_date):
        total += daily_consumption_for_day(day, tablets_per_day, pauses, schedule_changes)
    return round(total, 2)


def calculate_stock_on_date(
    row: sqlite3.Row | dict,
    target_date: date,
    pauses: list[PausePeriod] | None = None,
    schedule_changes: list[ScheduleChange] | None = None,
    movements: list[StockMovement] | None = None,
) -> float:
    prescription_issue_date = parse_iso_date(row["prescription_issue_date"])
    if target_date < prescription_issue_date:
        return round(float(row["initial_tablets"] or 0), 2)

    medication_id = int(row["id"])
    pauses = pauses if pauses is not None else list_pause_periods(medication_id)
    schedule_changes = schedule_changes if schedule_changes is not None else list_schedule_changes(medication_id)
    movements = movements if movements is not None else list_stock_movements(medication_id)

    movement_sum = sum(float(item.quantity) for item in movements if item.movement_date <= target_date)
    consumed = consumption_between(
        start_date=prescription_issue_date,
        end_date=target_date,
        tablets_per_day=float(row["tablets_per_day"] or 0),
        pauses=pauses,
        schedule_changes=schedule_changes,
    )
    stock = float(row["initial_tablets"] or 0) + movement_sum - consumed
    return round(stock, 2)


def movement_totals_by_day(movements: list[StockMovement]) -> dict[date, float]:
    totals: dict[date, float] = defaultdict(float)
    for movement in movements:
        totals[movement.movement_date] += float(movement.quantity)
    return {day: round(quantity, 2) for day, quantity in totals.items()}


def build_stock_timeseries(
    row: sqlite3.Row,
    today: date | None = None,
    include_future_days: int = 0,
) -> pd.DataFrame:
    today = today or date.today()
    prescription_issue_date = parse_iso_date(row["prescription_issue_date"])
    pauses = list_pause_periods(row["id"])
    schedule_changes = list_schedule_changes(row["id"])
    movements = list_stock_movements(row["id"])
    movement_by_day = movement_totals_by_day(movements)

    last_pause_day = max((pause.end_date for pause in pauses), default=prescription_issue_date)
    last_movement_day = max((movement.movement_date for movement in movements), default=prescription_issue_date)
    last_schedule_day = max((change.start_date for change in schedule_changes), default=prescription_issue_date)
    end_date = max(today, last_pause_day, last_movement_day, last_schedule_day) + timedelta(days=max(0, include_future_days))

    stock = round(float(row["initial_tablets"] or 0), 2)
    records: list[dict] = []
    for day in daterange(prescription_issue_date, end_date):
        movement_delta = round(float(movement_by_day.get(day, 0.0)), 2)
        consumption_delta = daily_consumption_for_day(
            day=day,
            tablets_per_day=float(row["tablets_per_day"] or 0),
            pauses=pauses,
            schedule_changes=schedule_changes,
        )
        stock = round(stock + movement_delta - consumption_delta, 2)
        schedule = get_schedule_for_day(day, schedule_changes)
        schedule_text = (
            schedule_to_text(
                schedule.schedule_mode,
                schedule.intake_weekdays,
                schedule.cycle_take_days,
                schedule.cycle_pause_days,
                schedule.cycle_start_date,
            )
            if schedule is not None
            else "-"
        )
        records.append(
            {
                "Datum": day,
                "Bestand": stock,
                "Bestandsänderung": movement_delta,
                "Verbrauch": consumption_delta,
                "Pause": "Ja" if is_pause_day(day, pauses) else "Nein",
                "Rhythmus": schedule_text,
            }
        )

    return pd.DataFrame(records)


def build_stock_event_df(row: sqlite3.Row) -> pd.DataFrame:
    prescription_issue_date = parse_iso_date(row["prescription_issue_date"])
    movements = list_stock_movements(row["id"])
    pauses = list_pause_periods(row["id"])
    schedule_changes = list_schedule_changes(row["id"])

    events: list[dict] = [
        {
            "Datum": prescription_issue_date,
            "Typ": "Startbestand",
            "Wert": round(float(row["initial_tablets"] or 0), 2),
            "Bemerkung": "Startbestand zum Rezeptdatum",
        }
    ]

    for item in movements:
        events.append(
            {
                "Datum": item.movement_date,
                "Typ": "Bestandsänderung",
                "Wert": round(float(item.quantity), 2),
                "Bemerkung": item.note or "-",
            }
        )

    for item in schedule_changes:
        events.append(
            {
                "Datum": item.start_date,
                "Typ": "Rhythmuswechsel",
                "Wert": "-",
                "Bemerkung": schedule_to_text(
                    item.schedule_mode,
                    item.intake_weekdays,
                    item.cycle_take_days,
                    item.cycle_pause_days,
                    item.cycle_start_date,
                ),
            }
        )

    for item in pauses:
        events.append(
            {
                "Datum": item.start_date,
                "Typ": "Pause beginnt",
                "Wert": "-",
                "Bemerkung": item.note or f"Bis {format_date_de(item.end_date)}",
            }
        )
        events.append(
            {
                "Datum": item.end_date,
                "Typ": "Pause endet",
                "Wert": "-",
                "Bemerkung": item.note or "-",
            }
        )

    event_df = pd.DataFrame(events)
    if not event_df.empty:
        event_df = event_df.sort_values(by=["Datum", "Typ"]).reset_index(drop=True)
    return event_df


def build_stock_movement_history_df(row: sqlite3.Row) -> pd.DataFrame:
    history_rows = [
        {
            "Datum": format_date_de(parse_iso_date(row["prescription_issue_date"])),
            "Änderung": round(float(row["initial_tablets"] or 0), 2),
            "Bemerkung": "Startbestand",
            "Erfasst am": format_datetime_de(row["created_at"]),
        }
    ]
    movements = sorted(list_stock_movements(row["id"]), key=lambda x: (x.movement_date, x.id), reverse=True)
    for item in movements:
        history_rows.append(
            {
                "Datum": format_date_de(item.movement_date),
                "Änderung": item.quantity,
                "Bemerkung": item.note or "-",
                "Erfasst am": format_datetime_de(item.created_at),
            }
        )
    return pd.DataFrame(history_rows)


def latest_change_display(row: sqlite3.Row) -> str:
    return format_datetime_de(row["last_checked"])


def data_quality_status(current_stock: float) -> str:
    return "Daten prüfen" if current_stock < 0 else "OK"


def validate_stock_history_consistency(
    row: sqlite3.Row | dict,
    initial_tablets: float | None = None,
    tablets_per_day: float | None = None,
    prescription_issue_date: date | None = None,
    schedule_changes: list[ScheduleChange] | None = None,
    pauses: list[PausePeriod] | None = None,
    movements: list[StockMovement] | None = None,
) -> tuple[bool, str | None, float]:
    effective_row = dict(row)
    if initial_tablets is not None:
        effective_row["initial_tablets"] = round(float(initial_tablets), 2)
    if tablets_per_day is not None:
        effective_row["tablets_per_day"] = round(float(tablets_per_day), 2)
    if prescription_issue_date is not None:
        effective_row["prescription_issue_date"] = prescription_issue_date.isoformat()

    stock_today = calculate_stock_on_date(
        row=effective_row,
        target_date=date.today(),
        pauses=pauses,
        schedule_changes=schedule_changes,
        movements=movements,
    )
    if stock_today < 0:
        return (
            False,
            "Die Historie würde zu einem negativen berechneten Bestand führen. Bitte Startbestand, Rhythmus, Pausen oder Bestandsänderungen prüfen.",
            stock_today,
        )
    return True, None, stock_today


def projected_until_date(
    available_stock: float,
    tablets_per_day: float,
    pauses: list[PausePeriod],
    schedule_changes: list[ScheduleChange],
    start_date: date,
    max_years: int = 10,
) -> date | None:
    available_stock = round(float(available_stock), 2)
    if available_stock <= 0:
        return None
    if tablets_per_day <= 0:
        return None

    remaining = available_stock
    last_covered_day = start_date - timedelta(days=1)
    horizon = start_date + relativedelta(years=max_years)
    current = start_date

    while current <= horizon:
        daily_need = daily_consumption_for_day(current, tablets_per_day, pauses, schedule_changes)
        if daily_need <= 0:
            last_covered_day = current
            current += timedelta(days=1)
            continue

        if remaining >= daily_need:
            remaining = round(remaining - daily_need, 2)
            last_covered_day = current
            current += timedelta(days=1)
            continue

        return last_covered_day if last_covered_day >= start_date else None

    return last_covered_day


def status_for_prescription(valid_until: date, today: date) -> str:
    if valid_until < today:
        return "Abgelaufen"
    if valid_until <= today + timedelta(days=30):
        return "Läuft bald ab"
    return "Gültig"


def rest_days_from_covered_until(covered_until: date | None, today: date) -> int | None:
    if covered_until is None:
        return None
    return max(0, (covered_until - today).days)


def remaining_days_display(current_stock: float, rest_days: int | None) -> str:
    if current_stock <= 0:
        return "Kein Bestand"
    if rest_days is None:
        return "Nicht berechenbar"
    if rest_days == 1:
        return "1 Tag"
    return f"{rest_days} Tage"


def covered_until_display(current_stock: float, covered_until: date | None) -> str:
    if current_stock <= 0:
        return "Kein Bestand"
    if covered_until is None:
        return "Nicht berechenbar"
    return format_date_de(covered_until)


def calculate_row_metrics(row: sqlite3.Row, today: date | None = None) -> dict:
    today = today or date.today()
    pauses = list_pause_periods(row["id"])
    schedule_changes = list_schedule_changes(row["id"])
    movements = list_stock_movements(row["id"])
    prescription_issue_date = parse_iso_date(row["prescription_issue_date"])
    valid_until = add_months(prescription_issue_date, int(row["validity_months"] or 0))

    consumed_since_issue = consumption_between(
        start_date=prescription_issue_date,
        end_date=today,
        tablets_per_day=float(row["tablets_per_day"] or 0),
        pauses=pauses,
        schedule_changes=schedule_changes,
    )
    current_stock = calculate_stock_on_date(
        row=row,
        target_date=today,
        pauses=pauses,
        schedule_changes=schedule_changes,
        movements=movements,
    )
    covered_until = projected_until_date(
        available_stock=current_stock,
        tablets_per_day=float(row["tablets_per_day"] or 0),
        pauses=pauses,
        schedule_changes=schedule_changes,
        start_date=today + timedelta(days=1),
    )
    rest_days = rest_days_from_covered_until(covered_until, today)
    current_schedule = get_schedule_for_day(today, schedule_changes)

    if current_schedule:
        intake_weekdays = current_schedule.intake_weekdays
        schedule_mode = current_schedule.schedule_mode
        cycle_take_days = current_schedule.cycle_take_days
        cycle_pause_days = current_schedule.cycle_pause_days
        cycle_start_date = current_schedule.cycle_start_date
        schedule_display = schedule_to_text(
            schedule_mode,
            intake_weekdays,
            cycle_take_days,
            cycle_pause_days,
            cycle_start_date,
        )
    else:
        intake_weekdays = []
        schedule_mode = "weekly"
        cycle_take_days = 0
        cycle_pause_days = 0
        cycle_start_date = None
        schedule_display = "-"

    return {
        "prescription_issue_date": prescription_issue_date,
        "valid_until": valid_until,
        "prescription_status": status_for_prescription(valid_until, today),
        "data_quality_status": data_quality_status(current_stock),
        "intake_weekdays": intake_weekdays,
        "schedule_mode": schedule_mode,
        "cycle_take_days": cycle_take_days,
        "cycle_pause_days": cycle_pause_days,
        "cycle_start_date": cycle_start_date,
        "initial_stock": round(float(row["initial_tablets"] or 0), 2),
        "current_stock": current_stock,
        "consumed_since_issue": consumed_since_issue,
        "covered_until": covered_until,
        "rest_days": rest_days,
        "rest_days_display": remaining_days_display(current_stock, rest_days),
        "covered_until_display": covered_until_display(current_stock, covered_until),
        "schedule_display": schedule_display,
        "pause_count": len(pauses),
        "pauses": pauses,
        "schedule_changes": schedule_changes,
        "movements": movements,
        "latest_change_display": latest_change_display(row),
        "master_data_updated_display": format_datetime_de(row["master_data_updated_at"]),
    }


def build_overview_df(rows: list[sqlite3.Row]) -> pd.DataFrame:
    today = date.today()
    data: list[dict] = []

    for row in rows:
        metrics = calculate_row_metrics(row, today=today)
        data.append(
            {
                "ID": row["id"],
                "Name": row["person_name"],
                "Geburtsdatum": format_date_de(parse_iso_date(row["birth_date"])),
                "Medikament": row["medication_name"],
                "Rezeptdatum": format_date_de(metrics["prescription_issue_date"]),
                "Gültig bis": format_date_de(metrics["valid_until"]),
                "Rezeptstatus": metrics["prescription_status"],
                "Datenstatus": metrics["data_quality_status"],
                "Packungen Apotheke": int(row["pharmacy_reserved_packs"] or 0),
                "Startbestand": metrics["initial_stock"],
                "Bestand aktuell": metrics["current_stock"],
                "Tabletten pro Einnahmetag": round(float(row["tablets_per_day"] or 0), 2),
                "Einnahmerhythmus": metrics["schedule_display"],
                "Pausen erfasst": metrics["pause_count"],
                "Tage verbleibend": metrics["rest_days_display"],
                "Reicht voraussichtlich bis": metrics["covered_until_display"],
                "Verbrauch seit Rezeptdatum": metrics["consumed_since_issue"],
                "Zuletzt geändert": metrics["latest_change_display"],
                "Stammdaten geändert": metrics["master_data_updated_display"],
                "Bemerkungen": row["notes"],
                "_rest_days_numeric": -1 if metrics["current_stock"] <= 0 else metrics["rest_days"],
                "_covered_until_date": metrics["covered_until"],
            }
        )

    df = pd.DataFrame(data)
    if not df.empty:
        df = df.sort_values(by=["Name", "Medikament"]).reset_index(drop=True)
    return df


def build_low_stock_df(overview_df: pd.DataFrame, limit: int = 10) -> pd.DataFrame:
    if overview_df.empty:
        return pd.DataFrame()

    low_stock_df = overview_df[overview_df["_rest_days_numeric"].notna()].copy()
    if low_stock_df.empty:
        return pd.DataFrame()

    low_stock_df = low_stock_df[low_stock_df["_rest_days_numeric"] <= 14].copy()
    if low_stock_df.empty:
        return pd.DataFrame()

    low_stock_df = low_stock_df.sort_values(
        by=["_rest_days_numeric", "Name", "Medikament"],
        ascending=[True, True, True],
    ).head(limit)
    return low_stock_df[
        [
            "Name",
            "Medikament",
            "Datenstatus",
            "Bestand aktuell",
            "Tage verbleibend",
            "Reicht voraussichtlich bis",
            "Rezeptstatus",
            "Packungen Apotheke",
        ]
    ]


def get_medication_options(rows: list[sqlite3.Row]) -> dict[str, int]:
    options: dict[str, int] = {}
    for row in rows:
        label = f'{row["person_name"]} | {row["medication_name"]} | ID {row["id"]}'
        options[label] = row["id"]
    return options


def unique_field_values(rows: list[sqlite3.Row], field_name: str) -> list[str]:
    values = {
        clean_text(row[field_name])
        for row in rows
        if clean_text(row[field_name])
    }
    return sorted(values, key=str.casefold)


def render_text_input_with_suggestions(
    label: str,
    text_key: str,
    options: list[str],
    placeholder: str = "",
) -> str:
    normalized_options = sorted({clean_text(option) for option in options if clean_text(option)}, key=str.casefold)
    selection = st.selectbox(
        label,
        options=normalized_options,
        index=None,
        key=text_key,
        placeholder=placeholder or "Bestehenden Wert suchen oder neu eingeben",
        accept_new_options=True,
        help="Tippe, um bestehende Einträge zu suchen, oder bestätige einen neuen Wert mit Enter.",
    )
    return clean_text(selection)


def render_schedule_inputs(prefix: str, existing: sqlite3.Row | None = None):
    prescription_issue_default = (
        parse_iso_date(existing["prescription_issue_date"]) if existing else date.today()
    )
    default_schedule_mode = (existing["schedule_mode"] if existing else "weekly") or "weekly"
    radio_key = f"{prefix}_schedule_mode"
    if radio_key not in st.session_state:
        st.session_state[radio_key] = default_schedule_mode

    schedule_mode = st.radio(
        "Einnahmerhythmus",
        options=["weekly", "cycle"],
        horizontal=True,
        key=radio_key,
        format_func=lambda x: SCHEDULE_MODE_LABELS[x],
    )

    intake_weekdays: list[int] = []
    cycle_take_days = 0
    cycle_pause_days = 0
    cycle_start_date: date | None = None

    if schedule_mode == "weekly":
        default_weekdays = (
            sorted(int(x) for x in json.loads(existing["intake_weekdays"] or "[]"))
            if existing
            else list(WEEKDAY_LABELS.keys())
        )
        intake_weekdays = st.multiselect(
            "An welchen Wochentagen wird das Medikament eingenommen?",
            options=list(WEEKDAY_LABELS.keys()),
            default=default_weekdays,
            format_func=lambda x: WEEKDAY_LABELS[x],
            key=f"{prefix}_intake_weekdays",
        )
    else:
        cycle_col1, cycle_col2, cycle_col3 = st.columns(3)
        with cycle_col1:
            cycle_take_days = st.number_input(
                "Tage mit Einnahme",
                min_value=1,
                max_value=365,
                step=1,
                value=int(existing["cycle_take_days"] or 5) if existing else 5,
                key=f"{prefix}_cycle_take_days",
            )
        with cycle_col2:
            cycle_pause_days = st.number_input(
                "Tage Pause",
                min_value=0,
                max_value=365,
                step=1,
                value=int(existing["cycle_pause_days"] or 2) if existing else 2,
                key=f"{prefix}_cycle_pause_days",
            )
        with cycle_col3:
            cycle_start_default = (
                parse_iso_date(existing["cycle_start_date"])
                if existing and existing["cycle_start_date"]
                else prescription_issue_default
            )
            cycle_start_date = st.date_input(
                "Startdatum des Zyklus",
                value=cycle_start_default,
                format="DD.MM.YYYY",
                key=f"{prefix}_cycle_start_date",
            )

    return {
        "schedule_mode": schedule_mode,
        "intake_weekdays": sorted(int(x) for x in intake_weekdays),
        "cycle_take_days": int(cycle_take_days),
        "cycle_pause_days": int(cycle_pause_days),
        "cycle_start_date": cycle_start_date.isoformat() if cycle_start_date else None,
    }


def validate_medication_payload(payload: dict) -> str | None:
    if not payload["person_name"].strip():
        return "Bitte einen Namen eingeben."
    if not payload["medication_name"].strip():
        return "Bitte einen Medikamentennamen eingeben."
    if payload["schedule_mode"] == "weekly" and not payload["intake_weekdays"]:
        return "Bitte mindestens einen Wochentag auswählen."
    if payload["schedule_mode"] == "cycle" and int(payload["cycle_take_days"]) <= 0:
        return "Bitte mindestens 1 Tag mit Einnahme angeben."
    if float(payload["initial_tablets"]) < 0:
        return "Der Startbestand darf nicht negativ sein."
    return None


def validate_prescription_date_change(medication_id: int, new_prescription_issue_date: date) -> str | None:
    movements = list_stock_movements(medication_id)
    earlier_movements = [item for item in movements if item.movement_date < new_prescription_issue_date]
    if earlier_movements:
        return "Das Rezeptdatum kann nicht nach vorhandene Bestandsänderungen verschoben werden. Bitte zuerst die betroffenen Bestandsänderungen prüfen."
    return None


def validate_schedule_change_effective_date(medication_id: int, effective_date: date) -> str | None:
    changes = list_schedule_changes(medication_id)
    if not changes:
        return None
    latest_start_date = max(item.start_date for item in changes)
    if effective_date <= latest_start_date:
        return "Rhythmuswechsel müssen nach dem zuletzt gespeicherten Wechsel liegen."
    return None


def make_zip_bundle() -> None:
    with ZipFile(ZIP_PATH, "w", ZIP_DEFLATED) as zip_file:
        for filename in ["app.py", "README.md", "requirements.txt"]:
            file_path = APP_DIR / filename
            if file_path.exists():
                zip_file.write(file_path, arcname=f"medbestand_app/{filename}")


def can_apply_stock_movement(row: sqlite3.Row, movement_date: date, quantity: float) -> tuple[bool, str | None]:
    pauses = list_pause_periods(row["id"])
    schedule_changes = list_schedule_changes(row["id"])
    movements = list_stock_movements(row["id"])

    stock_without_new_movement = calculate_stock_on_date(
        row=row,
        target_date=movement_date,
        pauses=pauses,
        schedule_changes=schedule_changes,
        movements=movements,
    )
    stock_with_new_movement = round(stock_without_new_movement + float(quantity), 2)
    if stock_with_new_movement < 0:
        return False, "Der berechnete Bestand an diesem Datum würde negativ werden."
    return True, None


def add_stock_movement(medication_id: int, movement_date: date, quantity: float, note: str) -> None:
    quantity = round(float(quantity), 2)
    if quantity == 0:
        raise ValueError("Die Bestandsänderung darf nicht 0 sein.")

    row = get_medication(medication_id)
    if row is None:
        raise ValueError("Der Eintrag wurde nicht gefunden.")

    is_valid, error_message = can_apply_stock_movement(row, movement_date, quantity)
    if not is_valid:
        raise ValueError(error_message or "Ungültige Bestandsänderung.")

    with closing(get_conn()) as conn:
        now_iso = datetime.now().isoformat(timespec="seconds")
        conn.execute(
            """
            INSERT INTO stock_movements (medication_id, movement_date, quantity, note, created_at)
            VALUES (?, ?, ?, ?, ?)
            """,
            (medication_id, movement_date.isoformat(), quantity, note.strip(), now_iso),
        )
        _touch_medication(conn, medication_id)
        conn.commit()


def render_stock_chart(row: sqlite3.Row) -> None:
    chart_df = build_stock_timeseries(row)
    if chart_df.empty:
        st.caption("Für diesen Eintrag kann noch kein Bestandsverlauf dargestellt werden.")
        return

    line = (
        alt.Chart(chart_df)
        .mark_line()
        .encode(
            x=alt.X("Datum:T", title="Datum"),
            y=alt.Y("Bestand:Q", title="Berechneter Bestand"),
            tooltip=[
                alt.Tooltip("Datum:T", title="Datum"),
                alt.Tooltip("Bestand:Q", title="Bestand"),
                alt.Tooltip("Bestandsänderung:Q", title="Bestandsänderung"),
                alt.Tooltip("Verbrauch:Q", title="Verbrauch"),
                alt.Tooltip("Rhythmus:N", title="Rhythmus"),
                alt.Tooltip("Pause:N", title="Pause"),
            ],
        )
    )

    movement_points_df = chart_df[chart_df["Bestandsänderung"] != 0].copy()
    layers = [line]
    if not movement_points_df.empty:
        points = (
            alt.Chart(movement_points_df)
            .mark_point(size=80)
            .encode(
                x="Datum:T",
                y="Bestand:Q",
                tooltip=[
                    alt.Tooltip("Datum:T", title="Datum"),
                    alt.Tooltip("Bestand:Q", title="Bestand"),
                    alt.Tooltip("Bestandsänderung:Q", title="Bestandsänderung"),
                ],
            )
        )
        layers.append(points)

    st.altair_chart(alt.layer(*layers).interactive(), use_container_width=True)


# ---------- UI ----------
st.set_page_config(page_title="Medikamentenbestand", page_icon="💊", layout="wide")
init_db()
ensure_initial_schedule_history()
make_zip_bundle()

st.title("💊 Medikamentenbestand & Rezeptübersicht")
st.caption("Streamlit-App zur Medikamentenverwaltung mit Bestands- und Rezeptübersicht.")

flash_message = st.session_state.pop("flash_message", None)
if flash_message:
    st.success(flash_message)

with st.sidebar:
    st.header("Hinweise")
    st.caption(
        "Mit diesem Tool können Medikamentenbestände, Rezepte, Einnahmerhythmen sowie Pausen und Ferien übersichtlich verwaltet werden."
    )
    st.markdown(
        """
- **Bestand aktuell** wird automatisch aus **Startbestand - Verbrauch + Bestandsänderungen** berechnet.
- Neue und geänderte Einträge werden auf **negative berechnete Bestände** geprüft und bei Bedarf blockiert.
- **Startbestand** ist der Bestand **zum Rezeptdatum**.
- **In der Apotheke hinterlegte Packungen** werden separat erfasst und **nicht** in den aktuellen Bestand eingerechnet.
- **Reicht voraussichtlich bis** zeigt das Datum, bis zu dem der berechnete Bestand noch ausreicht.
- **Ferien / Pausen** können pro Medikament separat erfasst werden und fliessen in die Berechnung ein.
- Das **Zyklus-Modell** eignet sich für regelmässige Einnahmerhythmen, z. B. **5 Tage Einnahme / 2 Tage Pause**.
- Ein **Rhythmuswechsel** kann bei der Bearbeitung ab einem gewünschten Datum gespeichert werden und wird historisch berücksichtigt.
- Im Tab **Bestand / Zukauf** können **Zugänge**, aber auch **Korrekturen oder Verluste** erfasst werden.
- Die **Bestandshistorie** enthält zusätzlich eine synthetische Zeile für den **Startbestand**.
- **Zuletzt geändert** bezieht sich auf die letzte Änderung am Eintrag insgesamt. **Stammdaten geändert** nur auf Änderungen an den Stammdaten.
        """
    )

rows = list_medications()
overview_df = build_overview_df(rows)
medication_options = get_medication_options(rows)
existing_person_names = unique_field_values(rows, "person_name")
existing_medication_names = unique_field_values(rows, "medication_name")
low_stock_df = build_low_stock_df(overview_df)

summary_cols = st.columns(4)
summary_cols[0].metric("Einträge", len(rows))
summary_cols[1].metric(
    "Abgelaufene Rezepte",
    0 if overview_df.empty else int((overview_df["Rezeptstatus"] == "Abgelaufen").sum()),
)
summary_cols[2].metric(
    "Rezepte laufen bald ab (≤ 30 Tage)",
    0 if overview_df.empty else int((overview_df["Rezeptstatus"] == "Läuft bald ab").sum()),
)
summary_cols[3].metric(
    "Bestände ≤ 14 Tage",
    0 if overview_df.empty else int(overview_df["_rest_days_numeric"].fillna(999999).le(14).sum()),
)

tab1, tab2, tab3, tab4, tab5, tab6 = st.tabs(
    [
        "Übersicht",
        "Neuer Eintrag",
        "Bestand / Zukauf",
        "Pausen / Ferien",
        "Bearbeiten",
        "Export & Löschen",
    ]
)

with tab1:
    st.subheader("Aktuelle Übersicht")
    if overview_df.empty:
        st.info("Noch keine Einträge vorhanden. Lege im Tab **Neuer Eintrag** den ersten Datensatz an.")
    else:
        st.markdown("### Bestandswarnung")
        if low_stock_df.empty:
            st.success("Aktuell gibt es keine Einträge mit höchstens 14 verbleibenden Tagen.")
        else:
            st.dataframe(low_stock_df, use_container_width=True, hide_index=True)

        st.markdown("### Gesamtliste")
        search = st.text_input(
            "Suchen nach Name oder Medikament",
            placeholder="z. B. Ritalin oder Max Muster",
        )
        filtered_df = overview_df.copy()
        if search:
            mask = (
                filtered_df["Name"].str.contains(search, case=False, na=False)
                | filtered_df["Medikament"].str.contains(search, case=False, na=False)
            )
            filtered_df = filtered_df[mask]

        display_columns = [
            "Name",
            "Geburtsdatum",
            "Medikament",
            "Rezeptdatum",
            "Gültig bis",
            "Rezeptstatus",
            "Datenstatus",
            "Packungen Apotheke",
            "Startbestand",
            "Bestand aktuell",
            "Tabletten pro Einnahmetag",
            "Einnahmerhythmus",
            "Tage verbleibend",
            "Reicht voraussichtlich bis",
            "Verbrauch seit Rezeptdatum",
            "Zuletzt geändert",
            "Stammdaten geändert",
            "Bemerkungen",
        ]
        st.dataframe(filtered_df[display_columns], use_container_width=True, hide_index=True)

        st.markdown("### Detailansicht")
        selected_label = st.selectbox(
            "Eintrag auswählen",
            options=list(medication_options.keys()),
            key="detail_select",
        )
        selected_id = medication_options[selected_label]
        selected_row = get_medication(selected_id)

        if selected_row is not None:
            detail = calculate_row_metrics(selected_row)
            detail_col1, detail_col2 = st.columns(2)
            with detail_col1:
                st.markdown(f"**Name:** {selected_row['person_name']}")
                st.markdown(f"**Geburtsdatum:** {format_date_de(parse_iso_date(selected_row['birth_date']))}")
                st.markdown(f"**Medikament:** {selected_row['medication_name']}")
                st.markdown(f"**Rezeptdatum:** {format_date_de(detail['prescription_issue_date'])}")
                st.markdown(f"**Gültig bis:** {format_date_de(detail['valid_until'])}")
                st.markdown(f"**Rezeptstatus:** {detail['prescription_status']}")
                st.markdown(f"**Datenstatus:** {detail['data_quality_status']}")
                st.markdown(f"**Startbestand:** {detail['initial_stock']}")
            with detail_col2:
                st.markdown(f"**Bestand aktuell:** {detail['current_stock']}")
                st.markdown(f"**Packungen in Apotheke:** {int(selected_row['pharmacy_reserved_packs'] or 0)}")
                st.markdown(f"**Tabletten pro Einnahmetag:** {round(float(selected_row['tablets_per_day'] or 0), 2)}")
                st.markdown(f"**Einnahmerhythmus:** {detail['schedule_display']}")
                st.markdown(f"**Tage verbleibend:** {detail['rest_days_display']}")
                st.markdown(f"**Reicht voraussichtlich bis:** {detail['covered_until_display']}")
                st.markdown(f"**Zuletzt geändert:** {detail['latest_change_display']}")
                st.markdown(f"**Stammdaten geändert:** {detail['master_data_updated_display']}")

            movement_sum = round(sum(item.quantity for item in detail["movements"]), 2)
            st.markdown(f"**Verbrauch seit Rezeptdatum:** {detail['consumed_since_issue']}")
            st.markdown(f"**Summe Bestandsänderungen:** {movement_sum}")
            st.markdown(
                f"**Rechnung:** {detail['initial_stock']} + {movement_sum} - {detail['consumed_since_issue']} = {detail['current_stock']}"
            )
            st.markdown(f"**Bemerkungen:** {selected_row['notes'] or '-'}")

            if detail["current_stock"] < 0:
                st.error("Dieser Eintrag ist historisch inkonsistent. Bitte Startbestand, Rhythmus, Pausen und Bestandsänderungen prüfen.")

            movement_df = build_stock_movement_history_df(selected_row)
            if not movement_df.empty:
                st.markdown("**Historie Bestand ändern / Zukauf:**")
                st.dataframe(movement_df, use_container_width=True, hide_index=True)

            schedule_history = detail["schedule_changes"]
            if schedule_history:
                schedule_df = pd.DataFrame(
                    [
                        {
                            "Gültig ab": format_date_de(item.start_date),
                            "Rhythmus": schedule_to_text(
                                item.schedule_mode,
                                item.intake_weekdays,
                                item.cycle_take_days,
                                item.cycle_pause_days,
                                item.cycle_start_date,
                            ),
                            "Erfasst am": format_datetime_de(item.created_at),
                        }
                        for item in schedule_history
                    ]
                )
                st.markdown("**Historie Einnahmerhythmus:**")
                st.dataframe(schedule_df, use_container_width=True, hide_index=True)

            if detail["pauses"]:
                pause_df = pd.DataFrame(
                    [
                        {
                            "Von": format_date_de(pause.start_date),
                            "Bis": format_date_de(pause.end_date),
                            "Bemerkung": pause.note,
                        }
                        for pause in detail["pauses"]
                    ]
                )
                st.markdown("**Erfasste Pausen / Ferien:**")
                st.dataframe(pause_df, use_container_width=True, hide_index=True)

            st.markdown("**Berechneter Bestandsverlauf:**")
            render_stock_chart(selected_row)

            event_df = build_stock_event_df(selected_row)
            if not event_df.empty:
                event_df_display = event_df.copy()
                event_df_display["Datum"] = event_df_display["Datum"].map(format_date_de)
                st.markdown("**Ereignisse im Verlauf:**")
                st.dataframe(event_df_display, use_container_width=True, hide_index=True)

with tab2:
    st.subheader("Neuen Patienten mit Medikament anlegen")

    new_col1, new_col2 = st.columns(2)
    with new_col1:
        person_name = render_text_input_with_suggestions(
            "Name",
            text_key="new_person_name",
            options=existing_person_names,
            placeholder="Neuen Namen eingeben oder bestehende Vorschläge übernehmen",
        )
        birth_date = st.date_input(
            "Geburtsdatum",
            value=date(2000, 1, 1),
            format="DD.MM.YYYY",
            key="new_birth_date",
        )
        medication_name = render_text_input_with_suggestions(
            "Medikament (vollständiger Name + Dosierung)",
            text_key="new_medication_name",
            options=existing_medication_names,
            placeholder="z. B. Ritalin LA 40mg",
        )
        prescription_issue_date = st.date_input(
            "Ausstellungsdatum des Rezepts",
            value=date.today(),
            format="DD.MM.YYYY",
            key="new_prescription_issue_date",
        )
        validity_months = st.slider(
            "Gültigkeit des Rezepts in Monaten",
            min_value=0,
            max_value=24,
            value=6,
            key="new_validity_months",
        )
        pharmacy_reserved_packs = st.number_input(
            "Packungen in der Apotheke hinterlegt",
            min_value=0,
            step=1,
            value=0,
            key="new_pharmacy_reserved_packs",
        )
    with new_col2:
        initial_tablets = st.number_input(
            "Startbestand zum Rezeptdatum",
            min_value=0.0,
            step=0.25,
            value=0.0,
            key="new_initial_tablets",
        )
        tablets_per_day = st.number_input(
            "Wie viele Tabletten pro Einnahmetag?",
            min_value=0.25,
            max_value=20.0,
            step=0.25,
            value=1.0,
            key="new_tablets_per_day",
        )
        notes = st.text_area("Bemerkungen", height=140, key="new_notes")

    new_schedule = render_schedule_inputs(prefix="new", existing=None)

    if st.button("Neuen Eintrag speichern", use_container_width=True):
        payload = {
            "person_name": person_name,
            "birth_date": birth_date.isoformat(),
            "medication_name": medication_name,
            "prescription_issue_date": prescription_issue_date.isoformat(),
            "validity_months": int(validity_months),
            "pharmacy_reserved_packs": int(pharmacy_reserved_packs),
            "initial_tablets": float(initial_tablets),
            "tablets_per_day": float(tablets_per_day),
            "intake_weekdays": new_schedule["intake_weekdays"],
            "schedule_mode": new_schedule["schedule_mode"],
            "cycle_take_days": new_schedule["cycle_take_days"],
            "cycle_pause_days": new_schedule["cycle_pause_days"],
            "cycle_start_date": new_schedule["cycle_start_date"],
            "notes": notes,
        }
        validation_error = validate_medication_payload(payload)
        if validation_error:
            st.error(validation_error)
        else:
            temp_row = {
                "id": -1,
                "prescription_issue_date": payload["prescription_issue_date"],
                "initial_tablets": payload["initial_tablets"],
                "tablets_per_day": payload["tablets_per_day"],
            }
            initial_schedule_changes = [
                ScheduleChange(
                    id=-1,
                    medication_id=-1,
                    start_date=parse_iso_date(payload["prescription_issue_date"]),
                    schedule_mode=payload["schedule_mode"],
                    intake_weekdays=payload["intake_weekdays"],
                    cycle_take_days=payload["cycle_take_days"],
                    cycle_pause_days=payload["cycle_pause_days"],
                    cycle_start_date=parse_iso_date(payload["cycle_start_date"]) if payload["cycle_start_date"] else parse_iso_date(payload["prescription_issue_date"]),
                    created_at=datetime.now().isoformat(timespec="seconds"),
                )
            ]
            is_valid, error_message, _ = validate_stock_history_consistency(
                row=temp_row,
                schedule_changes=initial_schedule_changes,
                pauses=[],
                movements=[],
            )
            if not is_valid:
                st.error(error_message)
            else:
                save_medication(payload)
                st.session_state["flash_message"] = "Eintrag gespeichert."
                st.rerun()

with tab3:
    st.subheader("Bestand ändern / Zukauf buchen")
    if not medication_options:
        st.info("Lege zuerst mindestens einen Eintrag an.")
    else:
        stock_label = st.selectbox(
            "Für welches Medikament möchtest du den Bestand ändern?",
            options=list(medication_options.keys()),
            key="stock_select",
        )
        stock_med_id = medication_options[stock_label]
        stock_row = get_medication(stock_med_id)
        if stock_row is not None:
            detail = calculate_row_metrics(stock_row)
            st.metric("Bestand aktuell", detail["current_stock"])
            st.caption("Positiver Wert = Zukauf. Negativer Wert = Korrektur, Verlust oder Fehlbuchung.")

            movement_date = st.date_input(
                "Datum der Bestandsänderung",
                value=date.today(),
                format="DD.MM.YYYY",
                key="stock_movement_date",
            )
            quantity = st.number_input(
                "Bestandsänderung in Tabletten",
                step=0.25,
                value=0.0,
                key="stock_quantity",
            )
            stock_note = st.text_input(
                "Bemerkung",
                placeholder="z. B. Zukauf, Korrektur, Verlust",
                key="stock_note",
            )

            if st.button("Bestandsänderung speichern", use_container_width=True, disabled=(quantity == 0)):
                try:
                    add_stock_movement(stock_med_id, movement_date, quantity, stock_note)
                except ValueError as exc:
                    st.error(str(exc))
                else:
                    st.session_state["flash_message"] = "Bestandsänderung gespeichert."
                    st.rerun()

            history_df = build_stock_movement_history_df(stock_row)
            if not history_df.empty:
                st.markdown("### Historie")
                st.dataframe(history_df, use_container_width=True, hide_index=True)

with tab4:
    st.subheader("Pausen / Ferien verwalten")
    if not medication_options:
        st.info("Lege zuerst mindestens einen Eintrag an.")
    else:
        pause_label = st.selectbox(
            "Für welches Medikament möchtest du eine Pause / Ferien erfassen?",
            options=list(medication_options.keys()),
            key="pause_select",
        )
        pause_med_id = medication_options[pause_label]

        pause_col1, pause_col2 = st.columns(2)
        with pause_col1:
            pause_start = st.date_input(
                "Pause von",
                value=date.today(),
                format="DD.MM.YYYY",
                key="pause_start",
            )
        with pause_col2:
            pause_end = st.date_input(
                "Pause bis",
                value=date.today(),
                format="DD.MM.YYYY",
                key="pause_end",
            )
        pause_note = st.text_input(
            "Bemerkung",
            placeholder="z. B. Herbstferien",
            key="pause_note",
        )

        if st.button("Pause speichern", use_container_width=True):
            if pause_end < pause_start:
                st.error("Das Enddatum darf nicht vor dem Startdatum liegen.")
            else:
                try:
                    add_pause_period(pause_med_id, pause_start, pause_end, pause_note)
                except ValueError as exc:
                    st.error(str(exc))
                else:
                    st.session_state["flash_message"] = "Pause gespeichert."
                    st.rerun()

        existing_pauses = list_pause_periods(pause_med_id)
        if existing_pauses:
            st.markdown("### Bereits erfasste Pausen")
            selected_pause_row = get_medication(pause_med_id)
            pause_df = pd.DataFrame(
                [
                    {
                        "Name": selected_pause_row["person_name"] if selected_pause_row else "-",
                        "Medikament": selected_pause_row["medication_name"] if selected_pause_row else "-",
                        "Von": format_date_de(pause.start_date),
                        "Bis": format_date_de(pause.end_date),
                        "Bemerkung": pause.note or "-",
                    }
                    for pause in existing_pauses
                ]
            )
            st.dataframe(pause_df, use_container_width=True, hide_index=True)

            pause_options = {
                (
                    f"{selected_pause_row['person_name'] if selected_pause_row else '-'} | "
                    f"{selected_pause_row['medication_name'] if selected_pause_row else '-'} | "
                    f"{format_date_de(pause.start_date)} - {format_date_de(pause.end_date)}"
                ): pause.id
                for pause in existing_pauses
            }
            delete_pause_label = st.selectbox(
                "Pause löschen",
                options=list(pause_options.keys()),
                key="delete_pause_select",
            )
            if st.button("Ausgewählte Pause löschen", use_container_width=True):
                delete_pause_period(pause_options[delete_pause_label], pause_med_id)
                st.session_state["flash_message"] = "Pause gelöscht."
                st.rerun()

with tab5:
    st.subheader("Bestehenden Eintrag bearbeiten")
    if not medication_options:
        st.info("Noch keine Einträge vorhanden.")
    else:
        edit_label = st.selectbox(
            "Eintrag zum Bearbeiten",
            options=list(medication_options.keys()),
            key="edit_select",
        )
        edit_id = medication_options[edit_label]
        existing = get_medication(edit_id)

        if existing is not None:
            edit_col1, edit_col2 = st.columns(2)
            with edit_col1:
                person_name = st.text_input(
                    "Name",
                    value=existing["person_name"],
                    key=f"edit_person_name_{edit_id}",
                )
                birth_date = st.date_input(
                    "Geburtsdatum",
                    value=parse_iso_date(existing["birth_date"]),
                    format="DD.MM.YYYY",
                    key=f"edit_birth_date_{edit_id}",
                )
                medication_name = st.text_input(
                    "Medikament (vollständiger Name + Dosierung)",
                    value=existing["medication_name"],
                    key=f"edit_medication_name_{edit_id}",
                )
                prescription_issue_date = st.date_input(
                    "Ausstellungsdatum des Rezepts",
                    value=parse_iso_date(existing["prescription_issue_date"]),
                    format="DD.MM.YYYY",
                    key=f"edit_prescription_issue_date_{edit_id}",
                )
                validity_months = st.slider(
                    "Gültigkeit des Rezepts in Monaten",
                    min_value=0,
                    max_value=24,
                    value=int(existing["validity_months"] or 0),
                    key=f"edit_validity_months_{edit_id}",
                )
                pharmacy_reserved_packs = st.number_input(
                    "Packungen in der Apotheke hinterlegt",
                    min_value=0,
                    step=1,
                    value=int(existing["pharmacy_reserved_packs"] or 0),
                    key=f"edit_pharmacy_reserved_packs_{edit_id}",
                )
            with edit_col2:
                initial_tablets = st.number_input(
                    "Startbestand zum Rezeptdatum",
                    min_value=0.0,
                    step=0.25,
                    value=float(existing["initial_tablets"] or 0),
                    key=f"edit_initial_tablets_{edit_id}",
                )
                tablets_per_day = st.number_input(
                    "Wie viele Tabletten pro Einnahmetag?",
                    min_value=0.25,
                    max_value=20.0,
                    step=0.25,
                    value=float(existing["tablets_per_day"] or 1),
                    key=f"edit_tablets_per_day_{edit_id}",
                )
                notes = st.text_area(
                    "Bemerkungen",
                    value=existing["notes"],
                    height=140,
                    key=f"edit_notes_{edit_id}",
                )

            edit_schedule = render_schedule_inputs(prefix=f"edit_{edit_id}", existing=existing)
            schedule_effective_date = st.date_input(
                "Rhythmusänderung gültig ab",
                value=date.today(),
                format="DD.MM.YYYY",
                key=f"edit_schedule_effective_date_{edit_id}",
                help="Nur relevant, wenn sich der Einnahmerhythmus ändert. Standard ist heute; für rückwirkende Änderungen das Datum ausdrücklich anpassen.",
            )

            existing_schedule_changes = list_schedule_changes(edit_id)
            latest_schedule = existing_schedule_changes[-1] if existing_schedule_changes else None
            old_schedule_payload = {
                "schedule_mode": latest_schedule.schedule_mode if latest_schedule else (existing["schedule_mode"] or "weekly"),
                "intake_weekdays": latest_schedule.intake_weekdays if latest_schedule else sorted(int(x) for x in json.loads(existing["intake_weekdays"] or "[]")),
                "cycle_take_days": latest_schedule.cycle_take_days if latest_schedule else int(existing["cycle_take_days"] or 0),
                "cycle_pause_days": latest_schedule.cycle_pause_days if latest_schedule else int(existing["cycle_pause_days"] or 0),
                "cycle_start_date": latest_schedule.cycle_start_date.isoformat() if latest_schedule and latest_schedule.cycle_start_date else (existing["cycle_start_date"] or None),
            }
            new_schedule_payload = {
                "schedule_mode": edit_schedule["schedule_mode"],
                "intake_weekdays": edit_schedule["intake_weekdays"],
                "cycle_take_days": edit_schedule["cycle_take_days"],
                "cycle_pause_days": edit_schedule["cycle_pause_days"],
                "cycle_start_date": edit_schedule["cycle_start_date"],
            }

            if st.button("Änderungen speichern", use_container_width=True):
                payload = {
                    "person_name": person_name,
                    "birth_date": birth_date.isoformat(),
                    "medication_name": medication_name,
                    "prescription_issue_date": prescription_issue_date.isoformat(),
                    "validity_months": int(validity_months),
                    "pharmacy_reserved_packs": int(pharmacy_reserved_packs),
                    "initial_tablets": float(initial_tablets),
                    "tablets_per_day": float(tablets_per_day),
                    "intake_weekdays": edit_schedule["intake_weekdays"],
                    "schedule_mode": edit_schedule["schedule_mode"],
                    "cycle_take_days": edit_schedule["cycle_take_days"],
                    "cycle_pause_days": edit_schedule["cycle_pause_days"],
                    "cycle_start_date": edit_schedule["cycle_start_date"],
                    "notes": notes,
                }
                validation_error = validate_medication_payload(payload)
                current_prescription_issue_date = parse_iso_date(existing["prescription_issue_date"])
                prescription_date_error = (
                    validate_prescription_date_change(edit_id, prescription_issue_date)
                    if prescription_issue_date > current_prescription_issue_date
                    else None
                )
                schedule_date_error = (
                    validate_schedule_change_effective_date(edit_id, schedule_effective_date)
                    if old_schedule_payload != new_schedule_payload
                    else None
                )
                if validation_error:
                    st.error(validation_error)
                elif prescription_date_error:
                    st.error(prescription_date_error)
                elif schedule_date_error:
                    st.error(schedule_date_error)
                else:
                    simulated_schedule_changes = list_schedule_changes(edit_id)
                    if old_schedule_payload != new_schedule_payload:
                        simulated_schedule_changes = simulated_schedule_changes + [
                            ScheduleChange(
                                id=-1,
                                medication_id=edit_id,
                                start_date=schedule_effective_date,
                                schedule_mode=new_schedule_payload["schedule_mode"],
                                intake_weekdays=new_schedule_payload["intake_weekdays"],
                                cycle_take_days=new_schedule_payload["cycle_take_days"],
                                cycle_pause_days=new_schedule_payload["cycle_pause_days"],
                                cycle_start_date=parse_iso_date(new_schedule_payload["cycle_start_date"]) if new_schedule_payload["cycle_start_date"] else None,
                                created_at=datetime.now().isoformat(timespec="seconds"),
                            )
                        ]
                        simulated_schedule_changes = sorted(simulated_schedule_changes, key=lambda x: (x.start_date, x.id))

                    is_valid, error_message, _ = validate_stock_history_consistency(
                        row=existing,
                        initial_tablets=payload["initial_tablets"],
                        tablets_per_day=payload["tablets_per_day"],
                        prescription_issue_date=prescription_issue_date,
                        schedule_changes=simulated_schedule_changes,
                        pauses=list_pause_periods(edit_id),
                        movements=list_stock_movements(edit_id),
                    )
                    if not is_valid:
                        st.error(error_message)
                    else:
                        changed = save_medication(payload, medication_id=edit_id)
                        if old_schedule_payload != new_schedule_payload:
                            try:
                                add_schedule_change(
                                medication_id=edit_id,
                                start_date=schedule_effective_date,
                                schedule_mode=new_schedule_payload["schedule_mode"],
                                intake_weekdays=new_schedule_payload["intake_weekdays"],
                                cycle_take_days=new_schedule_payload["cycle_take_days"],
                                cycle_pause_days=new_schedule_payload["cycle_pause_days"],
                                cycle_start_date=parse_iso_date(new_schedule_payload["cycle_start_date"]) if new_schedule_payload["cycle_start_date"] else None,
                                )
                            except ValueError as exc:
                                st.error(str(exc))
                            else:
                                st.session_state["flash_message"] = "Eintrag und Rhythmusänderung gespeichert."
                        else:
                            st.session_state["flash_message"] = (
                                "Eintrag gespeichert." if changed else "Keine Änderungen erkannt."
                            )
                        st.rerun()

with tab6:
    st.subheader("Export & Löschen")
    if overview_df.empty:
        st.info("Noch keine Daten zum Exportieren vorhanden.")
    else:
        export_df = overview_df[
            [
                "Name",
                "Geburtsdatum",
                "Medikament",
                "Rezeptdatum",
                "Gültig bis",
                "Rezeptstatus",
                "Datenstatus",
                "Packungen Apotheke",
                "Startbestand",
                "Bestand aktuell",
                "Tabletten pro Einnahmetag",
                "Einnahmerhythmus",
                "Pausen erfasst",
                "Tage verbleibend",
                "Reicht voraussichtlich bis",
                "Verbrauch seit Rezeptdatum",
                "Zuletzt geändert",
                "Stammdaten geändert",
                "Bemerkungen",
            ]
        ]
        csv_data = export_df.to_csv(index=False).encode("utf-8-sig")
        st.download_button(
            "Übersicht als CSV herunterladen",
            data=csv_data,
            file_name="medikamentenbestand_uebersicht.csv",
            mime="text/csv",
            use_container_width=True,
        )

        if DB_PATH.exists():
            st.download_button(
                "SQLite-Datenbank herunterladen",
                data=DB_PATH.read_bytes(),
                file_name="med_tracker.db",
                mime="application/octet-stream",
                use_container_width=True,
            )

    st.markdown("---")
    st.markdown("### Eintrag endgültig löschen")
    if not medication_options:
        st.info("Keine Einträge zum Löschen vorhanden.")
    else:
        delete_label = st.selectbox(
            "Eintrag wählen",
            options=list(medication_options.keys()),
            key="delete_med_select",
        )
        delete_id = medication_options[delete_label]
        confirm_delete = st.checkbox("Ich möchte diesen Eintrag wirklich löschen.")
        if st.button("Eintrag löschen", use_container_width=True, disabled=not confirm_delete):
            delete_medication(delete_id)
            st.session_state["flash_message"] = "Eintrag gelöscht."
            st.rerun()
