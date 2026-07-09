demand_combo <- demand_sched %>%
  left_join(
    demand_strux %>%
      select(eiaId, rateName, utilityName, sector, period, tier, total_rate, max),
    by = c("eiaId", "rateName", "utilityName", "sector", "period")
  ) |>
  mutate(
    demandMin = coalesce(demandMin, -1),
    demandMax = coalesce(demandMax, -1)
  )
