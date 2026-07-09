calculate_levelized_energy_costs <- function(dataset, monthly_usage_kWh = 2000, peak_demand_kW = 500) {
  # Convert if necessary
  peak_demand_hp <- peak_demand_kW / 0.746
  peak_demand_kVA <- peak_demand_kW / 0.9

  dataset |>
    distinct(eiaId, rateName, utilityName, period, tier, unit, max, total_rate) |>
    arrange(eiaId, rateName, period, tier) |>
    group_by(eiaId, rateName, utilityName, period) |>
    mutate(
      # Treat NA, -1, AND 0 as Infinity (uncapped tiers)
      raw_max = case_when(
        is.na(max) ~ Inf,
        as.numeric(max) <= 0 ~ Inf,
        TRUE ~ as.numeric(max)
      ),
      safe_rate = coalesce(as.numeric(total_rate), 0),
      real_max = case_when(
        unit == "kWh" ~ raw_max,
        unit == "kWh/kW" ~ raw_max * peak_demand_kW,
        unit == "kWh/hp" ~ raw_max * peak_demand_hp,
        unit == "kWh/kVA" ~ raw_max * peak_demand_kVA,
        TRUE ~ raw_max
      ),
      prev_max = lag(real_max, default = 0),

      # Stop Inf - Inf math
      bucket_size = if_else(
        is.infinite(prev_max),
        0,
        pmax(real_max - prev_max, 0, na.rm = TRUE)
      ),
      # figure out remaining energy
      remaining_usage = if_else(
        is.infinite(prev_max),
        0,
        pmax(monthly_usage_kWh - prev_max, 0, na.rm = TRUE)
      ),
      tier_usage = pmin(remaining_usage, bucket_size, na.rm = TRUE),
      tier_cost = tier_usage * safe_rate
    ) |>
    summarize(
      total_kWh_billed = sum(tier_usage, na.rm = TRUE),
      total_energy_cost = sum(tier_cost, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Keep units consistent by dividing by total_kWh_billed
      levelized_rate = total_energy_cost / total_kWh_billed
    )
}
