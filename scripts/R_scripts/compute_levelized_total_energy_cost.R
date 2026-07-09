# combine energy_sched with energy_cost to get levelized cost per period
energy_total_sched <- energy_sched |>
  left_join(
    energy_cost,
    by = c("eiaId", "rateName", "utilityName", "period")
  )
