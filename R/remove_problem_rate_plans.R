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

remove_negative_rate_plans <- function(dataset) {
  dataset |>
    group_by(utilityName, rateName) |>
    filter(
      all(total_energy_rate >= 0, na.rm = TRUE),
      all(total_demand_rate >= 0, na.rm = TRUE)
    ) |>
    ungroup()
}

# remove irrigation, agriculture, agricultural, pumping, furnace plans
remove_irrelevant_plans <- function(dataset) {
  dataset |>
    filter(!str_detect(
      rateName,
      regex("irrigation|agricultur|agribusiness|pumping|furnace|heating|conditioning|catfish", ignore_case = TRUE)
    ))
}

remove_multi_unit_plans <- function(dataset) {
  dataset |>
    filter(
      multi_unit == FALSE
    )
}

remove_single_unit_plans <- function(dataset) {
  dataset |>
    filter(
      multi_unit == TRUE
    )
}

# remove_problem_rate_plans <- function(dataset) {
#   dataset |>
#     remove_high_fixed_charge_plans() |>
#     remove_high_energy_rate_plans() |>
#     remove_irrelevant_plans() |>
#     remove_multi_unit_plans()
# }

count_plans <- function(df, keys = c("eiaId", "utilityName", "rateName")) {
  df %>%
    distinct(across(all_of(keys))) %>%
    nrow()
}

remove_problem_rate_plans <- function(dataset) {
  steps <- list(
    high_fixed_charge = remove_high_fixed_charge_plans,
    high_energy_rate = remove_high_energy_rate_plans,
    irrelevant_plans = remove_irrelevant_plans,
    multi_unit_plans = remove_multi_unit_plans,
    remove_negative_plans = remove_negative_rate_plans
  )

  current <- dataset
  n_prev <- count_plans(current)

  for (nm in names(steps)) {
    current <- steps[[nm]](current)
    n_now <- count_plans(current)

    targets::tar_message(
      sprintf("%s: removed %d plans (remaining %d)", nm, n_prev - n_now, n_now),
      class = character(0)
    )

    n_prev <- n_now
  }

  current
}
