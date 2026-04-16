# Medikamentenbestand-App

Eine lokale Streamlit-App für:

- Medikamentenbestand berechnen
- Rezeptgültigkeit im Blick behalten
- Verbrauch seit Rezeptdatum anzeigen
- Ferien / Einnahmepausen berücksichtigen
- Regelmässige Einnahmezyklen abbilden
- Kritische Bestände schnell erkennen
- Bestandsänderungen separat buchen
- Bemerkungen und Apotheke-Reserve getrennt führen

## Funktionen

### Übersicht
- Vollständige Liste aller Einträge
- **Bestandswarnung nur für Einträge mit höchstens 14 verbleibenden Tagen**
- Detailansicht mit:
  - Bestand aktuell
  - Rezeptstatus
  - Einnahmerhythmus
  - Tage verbleibend
  - **Reicht voraussichtlich bis**
  - Verbrauch seit Rezeptdatum
  - Zuletzt geprüft
  - Historie von **Bestand ändern / Zukauf**

### Neuer Eintrag
- Name mit Vorschlägen aus der bestehenden Datenbank, aber weiterhin freier Eingabe
- Geburtsdatum
- Medikamentenname inkl. Dosierung mit Vorschlägen aus der bestehenden Datenbank, aber weiterhin freier Eingabe
- Rezept-Ausstellungsdatum
- Gültigkeit in Monaten (0-24)
- Anzahl Packungen in der Apotheke (nur Anzeige, **nicht** im Bestand aktuell)
- Bestand aktuell
- Tabletten pro Einnahmetag
- **Zwei Arten von Einnahmerhythmus**:
  - feste Wochentage, bei neuen Einträgen standardmässig alle 7 Tage vorausgewählt
  - Zyklus, z. B. **5 Tage Einnahme / 2 Tage Pause**
- Bemerkungen

### Bestand / Zukauf
- Bestandsänderungen separat buchen, ohne den Stammdatensatz zu bearbeiten
- **Positive Werte = Zukauf**
- **Negative Werte = Korrektur / Verlust / Fehlbuchung**
- Negative Buchungen sind erlaubt, solange der Bestand aktuell nicht unter 0 fällt
- Verlauf aller bisherigen Bestandsänderungen pro Medikament

### Pausen / Ferien
Für jedes Medikament können beliebig viele zusätzliche Pausen mit Start- und Enddatum erfasst werden.
Diese Tage werden bei der Verbrauchs- und Reichweitenberechnung berücksichtigt.

Beispiele:
- Herbstferien
- Klinikaufenthalt
- Sonstige vorübergehende Unterbrüche

### Bearbeiten
- Bestehende Einträge können separat bearbeitet werden
- Das Feld **Zuletzt geprüft** wird **nur dann aktualisiert, wenn sich beim Speichern tatsächlich etwas geändert hat**
- Auch Pausen sowie Bestandsänderungen aktualisieren **Zuletzt geprüft**, weil sie den Eintrag effektiv verändern

### Automatische Berechnungen
- **Gültig bis**
- **Rezeptstatus** (gültig / läuft bald ab / abgelaufen)
- **Bestand aktuell**
- **Tage verbleibend**
- **Reicht voraussichtlich bis**
- **Verbrauch seit Rezeptdatum**

## Installation

```bash
python -m venv .venv
```

### Windows PowerShell
```bash
.venv\Scripts\Activate.ps1
pip install -r requirements.txt
streamlit run app.py
```

### macOS / Linux
```bash
source .venv/bin/activate
pip install -r requirements.txt
streamlit run app.py
```

## Hinweise zur Berechnung

- **Reicht voraussichtlich bis** wird ab dem heutigen Datum berechnet.
- Die **Packungen in der Apotheke** werden nicht in den Bestand aktuell eingerechnet.
- Bei **festen Wochentagen** wird die Einnahme genau über die gewählten Wochentage berechnet.
- Beim **Zyklus-Modell** wird ab dem gewählten Startdatum gerechnet, z. B. 5 Tage Einnahme / 2 Tage Pause.
- **Zusätzliche Ferien / Pausen** werden immer zusätzlich berücksichtigt.
- Wenn **Bestand aktuell = 0** ist, zeigt die App nicht mehr `None` oder `-`, sondern **Kein Bestand**.
- Bereits gespeicherte Werte aus dem früheren Feld **„Neu dazu“** werden beim Start automatisch in den Bestand aktuell übernommen.

## Datenspeicherung

Die Daten werden lokal in einer SQLite-Datei gespeichert:

- `med_tracker.db`

Zusätzlich werden Bestandsänderungen in einer separaten Historie gespeichert.
