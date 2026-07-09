label_demand_periods <- function(dataset) {
  dataset |>
    mutate(
      demandRateStrux = map(
        demandRateStrux,
        # Check if it's a dataframe rather than just not NULL to avoid errors on NAs
        ~ if (is.data.frame(.x)) {
          mutate(.x, period = seq_len(n()))
        } else {
          NA
        }
      )
    ) |>
    # keep plans that don't have them
    unnest(demandRateStrux, keep_empty = TRUE)
}

label_demand_tiers <- function(dataset) {
  # Make the demandRateTiers column if it doesn't exist
  if (!"demandRateTiers" %in% names(dataset)) {
    dataset <- mutate(dataset, demandRateTiers = NA)
  }

  # Mutate and unnest again
  dataset |>
    mutate(
      demandRateTiers = map(
        demandRateTiers,
        ~ if (is.data.frame(.x)) {
          mutate(.x, tier = seq_len(n()))
        } else {
          NA
        }
      )
    ) |>
    unnest(any_of("demandRateTiers"), keep_empty = TRUE)
}

reformat_demand_strux <- function(dataset) {
  expected_cols <- c("period", "tier", "rate", "adj", "max")

  # Make columns with NA if they don't exist after unnesting
  for (col in expected_cols) {
    if (!col %in% names(dataset)) {
      dataset <- mutate(dataset, !!col := NA_real_)
    }
  }

  # Replace all NAs with 0 and compute
  dataset |>
    mutate(
      across(all_of(expected_cols), as.numeric),
      across(all_of(expected_cols), ~ coalesce(.x, 0)),
      total_rate = rate + adj
    ) |>
    relocate(
      tier,
      .after = period
    )
}

fill_missing_units <- function(dataset) {
  # Create a blank 'unit' column if it wasn't present
  if (!"unit" %in% names(dataset)) {
    dataset <- mutate(dataset, unit = NA_character_)
  }

  dataset |>
    group_by(rateName, utilityName) |>
    fill(unit, .direction = "down") |>
    ungroup() |>
    mutate(
      unit = coalesce(unit, "kWh")
    )
}

make_demand_strux <- function(dataset) {
  dataset |>
    select(
      eiaId,
      rateName,
      utilityName,
      sector,
      demandMin,
      demandMax,
      demandRateUnits,
      demandRateStrux
    ) |>
    label_demand_periods() |>
    label_demand_tiers() |>
    reformat_demand_strux() |>
    fill_missing_units() |>
    select(-any_of("sell")) # drop sell column
}
