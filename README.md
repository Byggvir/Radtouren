# Radtouren

## Auswertung der Tourdaten

E-Bikes mit *Mahle* E-Motor liefern über die App **My Smartbike** Daten über den Verlauf einer Radtour. Im (Dashboard|https://my-smartbike.com/app/dashboard) können die Daten angesehen und in verschiedenen Formaten exportiert werden. Die umfangreichsten Daten liefert der Export in ein **JSON**-File. 

Was nicht möglich ist, ist zwei oder mehr Touren miteinander zu vergleichen. Dazu dienen die R-Scripte in diesen Repository.

Für die Auswertung müssen die Touren in das Verzeichnis **data** als einzelne Files exportiert werden. Die R-Scripte lessen alle Touren ein und erstellen verschiedene Grafiken. 

Bei den Streudiagrammen (Scatterplots) besteht nicht immer eine Korrelation der Werte untereinander. Manche Korrelationen sind sehr gering.

Die folgenden Messdaten aus dem JSON-File für die Wegpunkte werden ausgewertet

- al: Unterstützungslevel
- aw: Hinweis (z.B. "PAUSE)
- cp: Trittfrequenz
- gd: Distanz
- ge: Höhe
- la: Latitude
- lo: Longitude
- mw: Motorleistung
- or: unbekannt
- rw: Fahrerleistung
- sl: Steigung
- sp: Geschwindigkeit
- tm: Drehmoment
- ts: Zeitstempel

Die R Scripte lesen die Touren im Verzeichnis *data* ein und erstellen die Diagramme im Verzeichnis *png*.
