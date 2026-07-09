fixed_charges_part <- urdb_r |>
  select(
    eiaId,
    rateName,
    utilityName,
    fixedChargeFirstMeter
  ) |>
  mutate(
    fixedChargeFirstMeter = coalesce(fixedChargeFirstMeter, 0)
  )

demand_total_sched <- demand_total_sched |>
  mutate(
    week_part = case_when(
      # If the string contains "Weekday", label it "weekday"
      str_detect(sched_var, "Weekday") ~ "weekday",

      # If the string contains "Weekend", label it "weekend"
      str_detect(sched_var, "Weekend") ~ "weekend",

      # A safety net just in case there are weird entries
      TRUE ~ "unknown"
    )
  )

demand_part <- demand_total_sched |>
  select(
    eiaId,
    rateName,
    utilityName,
    week_part,
    month,
    hour,
    period,
    levelized_total_demand_cost
  ) |>
  rename(
    demand_period = period,
    total_demand_rate = levelized_total_demand_cost
  )

energy_total_sched <- energy_total_sched |>
  mutate(
    week_part = case_when(
      # If the string contains "Weekday", label it "weekday"
      str_detect(sched_var, "Weekday") ~ "weekday",

      # If the string contains "Weekend", label it "weekend"
      str_detect(sched_var, "Weekend") ~ "weekend",

      # A safety net just in case there are weird entries
      TRUE ~ "unknown"
    )
  )

energy_part <- energy_total_sched |>
  select(
    eiaId,
    rateName,
    utilityName,
    week_part,
    month,
    hour,
    period,
    levelized_rate
  ) |>
  rename(
    energy_period = period,
    total_energy_rate = levelized_rate
  )

variable_parts <- demand_part |>
  left_join(
    energy_part,
    by = c("eiaId", "rateName", "utilityName", "week_part", "month", "hour")
  )

all_parts <- variable_parts |>
  left_join(
    fixed_charges_part,
    by = c("eiaId", "rateName", "utilityName")
  ) |>
  relocate(
    fixedChargeFirstMeter,
    .after = utilityName
  )
