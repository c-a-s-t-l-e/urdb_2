library(dplyr)
library(tidyr)

peak_demand_kW <- 500
peak_demand_hp <- peak_demand_kW / 0.746
peak_demand_kVA <- peak_demand_kW / 0.9

flat_demand_cost <- flat_demand_combo |>
  # 0. Fill in any missing units with "kW" right off the bat
  mutate(flatDemandUnits = coalesce(flatDemandUnits, "kW")) |>
  # 1. Isolate the unique tiers by MONTH instead of period
  distinct(eiaId, rateName, utilityName, month, tier, flatDemandUnits, max, total_rate) |>
  # 2. Sort to ensure tiers cascade in the correct order (1, 2, etc.)
  arrange(eiaId, rateName, month, tier) |>
  # 3. Group by the plan and MONTH to calculate the math across the tiers
  group_by(eiaId, rateName, utilityName, month) |>
  mutate(
    # Convert -1 to Infinity
    real_max = if_else(max == -1, Inf, as.numeric(max)),

    # Calculate the size of each bucket
    prev_max = lag(real_max, default = 0),
    bucket_size = real_max - prev_max,

    # --- THE MAGIC STEP ---
    # Dynamically select the correct peak demand based on the plan's unit
    target_peak_demand = case_when(
      flatDemandUnits == "kW" ~ peak_demand_kW,
      flatDemandUnits == "hp" ~ peak_demand_hp,
      flatDemandUnits == "kVA" ~ peak_demand_kVA,
      TRUE ~ peak_demand_kW # Fallback safety net
    ),

    # Allocate the *dynamic* target demand into the proper tiers
    remaining_demand = pmax(target_peak_demand - prev_max, 0),
    tier_demand = pmin(remaining_demand, bucket_size),

    # Calculate the cost for this specific tier
    tier_cost = tier_demand * total_rate
  ) |>
  # 4. Roll it up! Sum the tiers together to get the final Demand Charge (Single Summarize)
  summarize(
    total_flat_demand_billed = sum(tier_demand),
    total_flat_demand_cost = sum(tier_cost),
    .groups = "drop"
  ) |>
  # 5. Calculate the Levelized Demand Rate
  mutate(
    levelized_flat_demand_rate = total_flat_demand_cost / total_flat_demand_billed
  )
