flat_demand_combo <- flat_demand_sched %>%
  rename(
    period = flatDemandMonths
  ) |>
  mutate(
    month = rep(1:12, length.out = n())
  ) |>
  left_join(
    flat_demand_strux %>%
      select(eiaId, rateName, utilityName, sector, period, tier, flatDemandUnits, total_rate, max),
    by = c("eiaId", "rateName", "utilityName", "sector", "period")
  ) |>
  mutate(
    demandMin = coalesce(demandMin, -1),
    demandMax = coalesce(demandMax, -1)
  )
