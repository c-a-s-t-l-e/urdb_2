library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)

# 1. Keep your helper function exactly as it was
build_12x24_matrix <- function(data, value_col) {
  grid <- expand_grid(month = 1:12, hour = 1:24)

  mat_data <- grid |>
    left_join(data, by = c("month", "hour")) |>
    select(month, hour, !!sym(value_col)) |>
    mutate(across(!!sym(value_col), ~ replace_na(., 0))) |>
    pivot_wider(names_from = hour, values_from = !!sym(value_col)) |>
    arrange(month) |>
    select(-month) |>
    as.matrix()

  return(mat_data)
}

# 2. The Updated Grouping Workflow
nested_plans <- your_df |>
  # Group by the top-level plan identifiers
  group_by(eiaId, rateName, utilityName, fixedChargeFirstMeter) |>
  # NEST the data! This safely packs your 576 rows into a neat list-column called 'data'
  nest() |>
  # Use purrr::map to iterate over that nested data and build the matrices
  mutate(
    totaldemandweekdayschedule = map(data, ~ build_12x24_matrix(filter(.x, week_part == "weekday"), "total_demand_rate")),
    totaldemandweekendschedule = map(data, ~ build_12x24_matrix(filter(.x, week_part == "weekend"), "total_demand_rate")),
    energyweekdayschedule      = map(data, ~ build_12x24_matrix(filter(.x, week_part == "weekday"), "total_energy_rate")),
    energyweekendschedule      = map(data, ~ build_12x24_matrix(filter(.x, week_part == "weekend"), "total_energy_rate"))
  ) |>
  # Clean up: remove the raw nested data and ungroup
  ungroup() |>
  select(-data) |>
  # 3. Rename columns to strictly match your JSON target keys
  rename(
    eiaid = eiaId,
    name = rateName,
    utility = utilityName,
    fixedchargefirstmeter = fixedChargeFirstMeter
  )

# 4. Generate the final JSON
final_json <- toJSON(nested_plans, pretty = TRUE, auto_unbox = TRUE)

writeLines(final_json, "rates_version_4.json")
