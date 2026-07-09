combine_demand_schedules <- function(demand_sched, demand_cost, flat_demand_cost, peak_demand_kW = 500) {
  demand_sched |>
    # Combine demand_sched with demand_cost to get levelized cost per period
    left_join(
      demand_cost,
      by = c("eiaId", "rateName", "utilityName", "period")
    ) |>
    # Combine with flat_demand_cost to then compute total levelized cost
    left_join(
      flat_demand_cost,
      by = c("eiaId", "rateName", "utilityName", "month")
    ) |>
    # 3. Clean up missing values and calculate final levelized cost
    mutate(
      # total_flat_demand_billed = total_demand_billed,
      total_flat_demand_cost = coalesce(total_flat_demand_cost, 0),
      levelized_flat_demand_rate = coalesce(levelized_flat_demand_rate, 0),

      # Calculated using the newly coalesced total_flat_demand_cost above
      levelized_total_demand_cost = (total_demand_cost + total_flat_demand_cost) / peak_demand_kW
    )
}
