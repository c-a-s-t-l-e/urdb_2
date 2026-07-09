remove_high_fixed_charge_plans <- function(dataset) {
  dataset |>
    filter(
      fixedChargeFirstMeter < 10000,
    )
}

remove_high_energy_rate_plans <- function(dataset) {
  dataset |>
    group_by(utilityName, rateName) |>
    filter(all(total_energy_rate <= 100, na.rm = TRUE)) |>
    ungroup()
}

remove_problem_rate_plans <- function(dataset) {
  dataset |>
    remove_high_fixed_charge_plans() |>
    remove_high_energy_rate_plans()
}
