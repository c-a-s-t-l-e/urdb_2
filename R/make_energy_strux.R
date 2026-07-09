label_energy_periods <- function(dataset) {
  dataset |>
    mutate(
      energyRateStrux = map(
        energyRateStrux,
        # Check if it's a dataframe rather than just not NULL to avoid errors on NAs
        ~ if (is.data.frame(.x)) {
          mutate(.x, period = seq_len(n()))
        } else {
          NA
        }
      )
    ) |>
    # keep the plans that don't have structures
    unnest(energyRateStrux, keep_empty = TRUE)
}

label_energy_tiers <- function(dataset) {
  # Make the tier column if it doesn't exist
  if (!"energyRateTiers" %in% names(dataset)) {
    dataset <- mutate(dataset, energyRateTiers = NA)
  }

  # 2. Mutate and unnest again
  dataset |>
    mutate(
      energyRateTiers = map(
        energyRateTiers, # Now this is a standard column reference, no any_of() needed here
        ~ if (is.data.frame(.x)) {
          mutate(.x, tier = seq_len(n()))
        } else {
          NA
        }
      )
    ) |>
    # keep empty entries
    unnest(any_of("energyRateTiers"), keep_empty = TRUE)
}

reformat_energy_strux <- function(dataset) {
  expected_cols <- c("period", "tier", "rate", "adj", "max")

  # Make columns if they don't exist
  for (col in expected_cols) {
    if (!col %in% names(dataset)) {
      dataset <- mutate(dataset, !!col := NA_real_)
    }
  }

  # 3. Replace all NAs with 0 and compute
  dataset |>
    mutate(
      across(c("rate", "adj"), ~ coalesce(.x, 0)),
      across(c("max"), ~ coalesce(.x, -1)),
      total_rate = rate + adj
    ) |>
    relocate(
      tier,
      .after = period
    )
}


fill_missing_units <- function(dataset) {
  # Make a 'unit' column if it didn't exist
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

make_energy_strux <- function(dataset) {
  dataset |>
    select(
      eiaId,
      rateName,
      utilityName,
      sector,
      energyMin,
      energyMax,
      energyRateStrux
    ) |>
    label_energy_periods() |>
    label_energy_tiers() |>
    reformat_energy_strux() |>
    fill_missing_units() |>
    select(-any_of("sell")) # Remove sell column
}
