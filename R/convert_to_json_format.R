build_12x24_matrix <- function(data, value_col) {
  grid <- expand_grid(month = 1:12, hour = 1:24)

  # Clean and deduplicate the data
  cleaned_data <- data |>
    select(month, hour, !!sym(value_col)) |>
    distinct(month, hour, .keep_all = TRUE)

  # Make a grid and reshape
  mat_data <- grid |>
    left_join(cleaned_data, by = c("month", "hour")) |>
    mutate(across(!!sym(value_col), ~ replace_na(., 0))) |>
    pivot_wider(names_from = hour, values_from = !!sym(value_col)) |>
    arrange(month) |>
    select(-month) |>
    as.matrix()

  return(mat_data)
}

nest_and_build_schedules <- function(dataset) {
  dataset |>
    distinct() |>
    # Group by plan identifiers
    group_by(eiaId, rateName, utilityName, fixedChargeFirstMeter) |>
    # Nest the data into a list-column that's called "data"
    nest() |>
    # Use map to iterate over that nested data and build the matrices
    mutate(
      totaldemandweekdayschedule = map(data, ~ build_12x24_matrix(filter(.x, week_part == "Weekday"), "total_demand_rate")),
      totaldemandweekendschedule = map(data, ~ build_12x24_matrix(filter(.x, week_part == "Weekend"), "total_demand_rate")),
      energyweekdayschedule      = map(data, ~ build_12x24_matrix(filter(.x, week_part == "Weekday"), "total_energy_rate")),
      energyweekendschedule      = map(data, ~ build_12x24_matrix(filter(.x, week_part == "Weekend"), "total_energy_rate"))
    ) |>
    ungroup() |>
    select(-data) |>
    rename(
      eiaid = eiaId,
      name = rateName,
      utility = utilityName,
      fixedchargefirstmeter = fixedChargeFirstMeter
    )
}

convert_to_json_format <- function(dataset) {
  dataset |>
    nest_and_build_schedules() |>
    toJSON(pretty = TRUE, auto_unbox = TRUE)
}
