calculate_levelized_demand_costs <- function(dataset, peak_demand_kW = 500) {
  # Convert to correct unit
  peak_demand_hp <- peak_demand_kW / 0.746
  peak_demand_kVA <- peak_demand_kW / 0.9

  dataset |>
    # Fill in any missing units with "kW"
    mutate(demandRateUnits = coalesce(demandRateUnits, "kW")) |>
    # Isolate the unique tiers
    distinct(eiaId, rateName, utilityName, period, tier, demandRateUnits, max, total_rate) |>
    # Sort tiers to have them descend in correct order
    arrange(eiaId, rateName, period, tier) |>
    # Group by the plan and period to calculate the math across the tiers
    group_by(eiaId, rateName, utilityName, period) |>
    mutate(
      # Treat NA, -1, AND 0 as Infinity (uncapped tiers)
      real_max = case_when(
        is.na(max) ~ Inf,
        as.numeric(max) <= 0 ~ Inf,
        TRUE ~ as.numeric(max)
      ),

      # Make sure total_rate is never NA
      safe_rate = coalesce(as.numeric(total_rate), 0),

      # Calculate the size of each bucket
      prev_max = lag(real_max, default = 0),

      # Stop Inf - Inf math and don't allow negative bucket sizes
      bucket_size = if_else(
        is.infinite(prev_max),
        0,
        pmax(real_max - prev_max, 0, na.rm = TRUE)
      ),

      # convert to proper unit for calculation
      target_peak_demand = case_when(
        demandRateUnits == "kW" ~ peak_demand_kW,
        demandRateUnits == "hp" ~ peak_demand_hp,
        demandRateUnits == "kVA" ~ peak_demand_kVA,
        TRUE ~ peak_demand_kW
      ),

      # Figure out the remaining demand
      remaining_demand = if_else(
        is.infinite(prev_max),
        0,
        pmax(target_peak_demand - prev_max, 0, na.rm = TRUE)
      ),
      tier_demand = pmin(remaining_demand, bucket_size, na.rm = TRUE),

      # Calculate the cost for the specific tier
      tier_cost = tier_demand * safe_rate
    ) |>
    # Sum the tiers together to get the final demand rate
    summarize(
      total_demand_billed = sum(tier_demand, na.rm = TRUE),
      total_demand_cost = sum(tier_cost, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # divide by peak_demand_kW to keep rates consistent
      levelized_demand_rate = ifelse(
        peak_demand_kW == 0,
        0,
        total_demand_cost / peak_demand_kW
      )
    )
}
