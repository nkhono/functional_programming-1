# Variant 24 — Автоматична станція очищення води (water_filtration)

CSV journal: `testdata/variant_24_water_filtration.csv` (copy of `n04_state_event_logs/generated/...`, 500 000 rows, ~5% intentionally corrupted).

Generation:

```
cd n04_state_event_logs/scripts
python3 generate_event_logs.py --variant 24 --rows 500000
```

Events: `FILTER_BATCH`, `FLUSH_DRAIN`, `REGENERATE`, `CARTRIDGE_REPLACE` (cartridge capacity 5000 L).

- `Parser.parseLine` — CSV line -> `Either[ParseError, Event]` (smart constructors `Quantity.liters` / `Quantity.salt`, ranges from the contract); `Parser.parseAll` uses `partitionMap`.
- `Station.replay` — `foldLeft` over valid events; computes remaining resource, clean water, loss %, stop signal.
- Invariant `stop_filtration_when_cartridge_depleted`: `FILTER_BATCH` exceeding remaining resource is rejected (`ResourceExhausted`).

Run: `sbt run` / `sbt test`.
