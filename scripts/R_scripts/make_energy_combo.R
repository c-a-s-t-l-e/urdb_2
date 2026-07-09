energy_combo <- energy_sched %>%
  left_join(
    energy_strux %>%
      select(eiaId, revision_date, rateName, utilityName, sector, period, tier, unit, max, total_rate),
    by = c("eiaId", "rateName", "utilityName", "sector", "period")
  ) |>
  group_by(rateName, utilityName) |>
  fill(unit, .direction = "down") |>
  ungroup() |>
  mutate(
    unit = coalesce(unit, "kWh"),
    energyMin = coalesce(energyMin, -1),
    energyMax = coalesce(energyMax, -1)
  )
