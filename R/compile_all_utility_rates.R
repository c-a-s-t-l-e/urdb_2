compile_all_utility_rates <- function(urdb_reformated, demand_total_sched, energy_total_sched) {
  # 1. Prepare fixed charges
  fixed_charges_part <- urdb_reformated |>
    select(eiaId, rateName, utilityName, fixedChargeFirstMeter) |>
    mutate(fixedChargeFirstMeter = coalesce(fixedChargeFirstMeter, 0))

  # 2. Process and prepare demand schedule components
  demand_part <- demand_total_sched |>
    select(
      eiaId, rateName, utilityName, week_part, month, hour,
      period, levelized_total_demand_cost
    ) |>
    rename(
      demand_period = period,
      total_demand_rate = levelized_total_demand_cost
    )

  # 3. Process and prepare energy schedule components
  energy_part <- energy_total_sched |>
    select(
      eiaId, rateName, utilityName, week_part, month, hour,
      period, levelized_rate
    ) |>
    rename(
      energy_period = period,
      total_energy_rate = levelized_rate
    )

  # 4. Join the variable schedules together
  variable_parts <- demand_part |>
    left_join(
      energy_part,
      by = c("eiaId", "rateName", "utilityName", "week_part", "month", "hour")
    )

  # 5. Join fixed charges and relocate the column for the final master output
  variable_parts |>
    left_join(
      fixed_charges_part,
      by = c("eiaId", "rateName", "utilityName")
    ) |>
    relocate(fixedChargeFirstMeter, .after = utilityName)
}
