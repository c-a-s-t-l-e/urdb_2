# 1. Define your specific customer profile here
monthly_usage_kWh <- 2000 # Total energy volume
peak_demand_kW <- 500 # Peak power draw
peak_demand_hp <- peak_demand_kW / 0.746 # Optional: Auto-converts kW to horsepower if you hit a kWh/hp plan
peak_demand_kVA <- peak_demand_kW / 0.9 # Optional: Estimates kVA assuming a 0.9 Power Factor

# 2. Run the dynamic pipeline
energy_cost <- energy_combo |>
  distinct(eiaId, rateName, utilityName, period, tier, unit, max, total_rate) |>
  arrange(eiaId, rateName, period, tier) |>
  group_by(eiaId, rateName, utilityName, period) |>
  mutate(
    # Handle Infinity (-1) limits
    raw_max = if_else(max == -1, Inf, as.numeric(max)),

    # Calculate the TRUE max kWh for the tier based on the unit type
    real_max = case_when(
      unit == "kWh" ~ raw_max,
      unit == "kWh/kW" ~ raw_max * peak_demand_kW,
      unit == "kWh/hp" ~ raw_max * peak_demand_hp,
      unit == "kWh/kVA" ~ raw_max * peak_demand_kVA,
      TRUE ~ raw_max
    ),

    # Waterfall math using the dynamic limits
    prev_max = lag(real_max, default = 0),
    bucket_size = real_max - prev_max,
    remaining_usage = pmax(monthly_usage_kWh - prev_max, 0),
    tier_usage = pmin(remaining_usage, bucket_size),
    tier_cost = tier_usage * total_rate
  ) |>
  # Roll it up into a final bill
  summarize(
    total_kWh_billed = sum(tier_usage),
    total_energy_cost = sum(tier_cost),
    .groups = "drop"
  ) |>
  # Calculate Levelized Rate (Total Cost / Usage)
  mutate(
    levelized_rate = total_energy_cost / total_kWh_billed
  )

# 3. View the results
head(energy_cost)
