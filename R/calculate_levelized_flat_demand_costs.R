calculate_levelized_flat_demand_costs <- function(dataset, peak_demand_kW = 500) {
  # Calculate converted peak demands inside the function environment
  peak_demand_hp <- peak_demand_kW / 0.746
  peak_demand_kVA <- peak_demand_kW / 0.9

  dataset |>
    # Fill in any missing units with "kW"
    mutate(flatDemandUnits = coalesce(flatDemandUnits, "kW")) |>
    # Isolate the unique tiers by month instead of by period
    distinct(eiaId, rateName, utilityName, month, tier, flatDemandUnits, max, total_rate) |>
    # Sort to ensure tiers descend correctly
    arrange(eiaId, rateName, month, tier) |>
    # 3. Group by the plan and month to calculate the math across the tiers
    group_by(eiaId, rateName, utilityName, month) |>
    mutate(
      # Treat NA, -1, AND 0 as Infinity (i.e. uncapped tiers)
      real_max = case_when(
        is.na(max) ~ Inf,
        as.numeric(max) <= 0 ~ Inf,
        TRUE ~ as.numeric(max)
      ),

      # Make sure total_rate is never NA
      safe_rate = coalesce(as.numeric(total_rate), 0),

      # Calculate the size of each bucket
      prev_max = lag(real_max, default = 0),

      # Stop Inf - Inf and block negative bucket sizes
      bucket_size = if_else(
        is.infinite(prev_max),
        0,
        pmax(real_max - prev_max, 0, na.rm = TRUE)
      ),

      # Choose peak demand based on the plan's unit
      target_peak_demand = case_when(
        flatDemandUnits == "kW" ~ peak_demand_kW,
        flatDemandUnits == "hp" ~ peak_demand_hp,
        flatDemandUnits == "kVA" ~ peak_demand_kVA,
        TRUE ~ peak_demand_kW # Fallback safety net
      ),

      # Figure out the remaining demand
      remaining_demand = if_else(
        is.infinite(prev_max),
        0,
        pmax(target_peak_demand - prev_max, 0, na.rm = TRUE)
      ),
      tier_demand = pmin(remaining_demand, bucket_size, na.rm = TRUE),

      # Calculate the cost for this specific tier
      tier_cost = tier_demand * safe_rate
    ) |>
    # Sum the tiers together to get the final demand rate
    summarize(
      total_flat_demand_billed = sum(tier_demand, na.rm = TRUE),
      total_flat_demand_cost = sum(tier_cost, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Divide by peak_demand_kW to keep units consistent
      levelized_flat_demand_rate = ifelse(
        peak_demand_kW == 0,
        0,
        total_flat_demand_cost / peak_demand_kW
      )
    )
}
