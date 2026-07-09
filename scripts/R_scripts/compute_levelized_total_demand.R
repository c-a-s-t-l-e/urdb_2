# combine demand_sched with demand_cost to get levelized cost per period
demand_level_sched <- demand_sched |>
  left_join(
    demand_cost,
    by = c("eiaId", "rateName", "utilityName", "period")
  )

# demand_level_sched with flat_demand_cost to then compute total levalized cost
demand_total_sched <- demand_level_sched |>
  left_join(
    flat_demand_cost,
    by = c("eiaId", "rateName", "utilityName", "month")
  ) |>
  mutate(
    total_flat_demand_billed = total_demand_billed,
    total_flat_demand_cost = coalesce(total_flat_demand_cost, 0),
    levelized_flat_demand_rate = coalesce(levelized_flat_demand_rate, 0)
  ) |>
  mutate(
    levelized_total_demand_cost = (total_demand_cost + total_flat_demand_cost) / total_demand_billed
  )
