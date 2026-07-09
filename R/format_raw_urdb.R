library(tidyverse)

# Step 1
get_latest_revised_plans <- function(dataset) {
  dataset |>
    unnest(revisions) |>
    rename(revision_date = date) |>
    group_by(utilityName, rateName) |>
    slice_max(revision_date,
      with_ties = FALSE,
      na_rm = TRUE
    ) |>
    ungroup()
}

# Step 2
flatten_dates <- function(dataset) {
  dataset_flat <- jsonlite::flatten(dataset)

  dataset_flat <- dataset_flat |>
    mutate(
      # Flatten the entries
      flat_effDate_chars = map_chr(`effectiveDate.$date`, ~ {
        if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]])
      }),
      flat_endDate_chars = map_chr(`endDate.$date`, ~ {
        if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]])
      }),

      # Parse the dates
      effectiveDate = as.POSIXct(flat_effDate_chars, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      endDate = as.POSIXct(flat_endDate_chars, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    ) |>
    # Drop the temporary character column
    select(-c(flat_effDate_chars, flat_endDate_chars))

  return(dataset_flat)
}

# fill in missing demand and energy min's and max's
fill_mins_and_maxes <- function(dataset) {
  dataset |>
    mutate(
      demandMin = coalesce(demandMin, -1),
      demandMax = coalesce(demandMax, -1),
      energyMin = coalesce(energyMin, -1),
      energyMax = coalesce(energyMax, -1)
    )
}

# fill in missing fixedcharges and units
fill_fixed_charges <- function(dataset) {
  dataset |>
    mutate(
      fixedChargeFirstMeter = ifelse(is.na(fixedChargeFirstMeter), 0, fixedChargeFirstMeter),
      fixedChargeUnits = ifelse(is.na(fixedChargeUnits), "
$/month", fixedChargeUnits)
    )
}

# convert fixed charge per day to fixed charge per month
convert_daily_fixed_charge <- function(dataset) {
  dataset |>
    mutate(
      fixedChargeFirstMeter = ifelse(fixedChargeUnits == "$/day", fixedChargeFirstMeter * 30.4375, fixedChargeFirstMeter),
      fixedChargeUnits = ifelse(fixedChargeUnits == "$/day", "$/month", fixedChargeUnits)
    )
}

# remove irrigation, agriculture, agricultural, pumping, furnace plans
remove_irrigation_plans <- function(dataset) {
  dataset |>
    filter(!str_detect(
      rateName,
      regex("irrigation|agricultur|agribusiness|pumping|furnace", ignore_case = TRUE)
    ))
}

# replace NULL objects with missing values
remove_null_objects <- function(dataset) {
  dataset |>
    mutate(across(
      where(is.list),
      ~ map(.x, ~ if (is.null(.x) || length(.x) == 0) NA else .x)
    ))
}

replace_missing_schedules <- function(dataset) {
  dataset |>
    mutate(
      # Get all columns whose names end with "sched"
      across(
        ends_with("sched"),
        ~ map(.x, ~ if (is.logical(.x) && is.na(.x)) matrix(0, nrow = 12, ncol = 24) else .x)
      )
    )
}

# Step 3
reformat_urdb <- function(dataset) {
  dataset |>
    mutate(
      effectiveDate_year = year(effectiveDate),
      effectiveDate_month = month(effectiveDate),
      endDate_year = year(endDate),
      endDate_month = month(endDate)
    ) |>
    select(
      eiaId,
      revision_date,
      effectiveDate,
      effectiveDate_year,
      effectiveDate_month,
      endDate,
      endDate_year,
      endDate_month,
      utilityName,
      rateName,
      sector,
      demandMin,
      demandMax,
      fixedChargeFirstMeter,
      fixedChargeUnits,
      flatDemandMonths,
      flatDemandStrux,
      flatDemandUnits,
      demandRateStrux,
      demandRateUnits,
      demandWeekdaySched,
      demandWeekendSched,
      demandUnits,
      energyMin,
      energyMax,
      energyRateStrux,
      energyWeekdaySched,
      energyWeekendSched
    )
}

# get_plans_in_effect <- function(dataset) {
#   dataset |>
#     filter(
#       effectiveDate_year >= 2021,
#       is.na(endDate) | endDate > Sys.Date() # FIX
#     )
# }

get_plans_in_effect <- function(dataset) {
  dataset |>
    group_by(utilityName) |>
    # Filter by year and endDate value
    filter(
      effectiveDate_year >= 2021,
      is.na(endDate) | endDate > Sys.Date()
    ) |>
    # Group by utility and plan
    group_by(utilityName, rateName) |>
    # Sort to have missing endDate first
    arrange(desc(is.na(endDate)), .by_group = TRUE) |>
    # Take the top row for each group
    slice_head(n = 1) |>
    ungroup()
}


format_raw_urdb <- function(dataset) {
  dataset |>
    flatten_dates() |>
    get_latest_revised_plans() |>
    reformat_urdb() |>
    get_plans_in_effect() |>
    fill_mins_and_maxes() |>
    fill_fixed_charges() |>
    convert_daily_fixed_charge() |>
    remove_irrigation_plans() |>
    remove_null_objects() |>
    replace_missing_schedules() |>
    filter(sector == "Commercial")
}
