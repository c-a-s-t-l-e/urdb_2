# Your base assumptions
peak_demand_kW <- 500
peak_demand_hp <- peak_demand_kW / 0.746
peak_demand_kVA <- peak_demand_kW / 0.9

flat_demand_calculated <- flat_demand_combo |>
  # 1. Fill missing units to be safe
  mutate(flatDemandUnits = coalesce(flatDemandUnits, "kW")) |>
  # 2. Isolate the unique tiers by MONTH this time
  distinct(eiaId, rateName, utilityName, month, tier, flatDemandUnits, max, total_rate) |>
  arrange(eiaId, rateName, month, tier) |>
  # 3. Group by the plan and MONTH
  group_by(eiaId, rateName, utilityName, month) |>
  mutate(
    # Handle Infinity
    real_max = if_else(max == -1, Inf, as.numeric(max)),
    prev_max = lag(real_max, default = 0),
    bucket_size = real_max - prev_max,

    # Dynamically select the correct peak demand based on the flat unit
    target_peak_demand = case_when(
      flatDemandUnits == "kW" ~ peak_demand_kW,
      flatDemandUnits == "hp" ~ peak_demand_hp,
      flatDemandUnits == "kVA" ~ peak_demand_kVA,
      TRUE ~ peak_demand_kW
    ),

    # Allocate demand into the tier buckets
    remaining_demand = pmax(target_peak_demand - prev_max, 0),
    tier_demand = pmin(remaining_demand, bucket_size),

    # Calculate the cost for the tier
    tier_cost = tier_demand * total_rate
  ) |>
  # 4. Roll it up to get the total flat cost per month!
  summarize(
    total_flat_demand_cost = sum(tier_cost),
    .groups = "drop"
  )
