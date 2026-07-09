label_flat_demand_period <- function(dataset) {
  # 1. Make the column if it doesn't exist
  if (!"flatDemandStrux" %in% names(dataset)) {
    dataset <- mutate(dataset, flatDemandStrux = NA)
  }

  dataset |>
    mutate(
      flatDemandStrux = map(
        flatDemandStrux,
        ~ if (is.data.frame(.x)) {
          mutate(.x, period = (seq_len(n()) - 1))
        } else {
          NA
        }
      )
    ) |>
    # Unnest without dropping empty rows
    unnest(any_of("flatDemandStrux"), keep_empty = TRUE)
}

label_flat_demand_tier <- function(dataset) {
  # 1. Make the column if it doesn't exist
  if (!"flatDemandTiers" %in% names(dataset)) {
    dataset <- mutate(dataset, flatDemandTiers = NA)
  }

  dataset |>
    mutate(
      flatDemandTiers = map(
        flatDemandTiers,
        ~ if (is.data.frame(.x)) {
          mutate(.x, tier = (seq_len(n())))
        } else {
          NA
        }
      )
    ) |>
    unnest(any_of("flatDemandTiers"), keep_empty = TRUE)
}

reformat_flat_demand_strux <- function(dataset) {
  expected_cols <- c("period", "tier", "rate", "adj", "max")

  # 2. Make the columns with NA if they don't exist
  for (col in expected_cols) {
    if (!col %in% names(dataset)) {
      dataset <- mutate(dataset, !!col := NA_real_)
    }
  }

  # 3. Replace all NAs with 0 and compute
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

make_flat_demand_strux <- function(dataset) {
  dataset |>
    select(
      any_of(c(
        "eiaId",
        "rateName",
        "utilityName",
        "sector",
        "flatDemandStrux",
        "flatDemandUnits"
      ))
    ) |>
    label_flat_demand_period() |>
    label_flat_demand_tier() |>
    reformat_flat_demand_strux()
}
