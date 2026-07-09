urdb_flat <- jsonlite::flatten(urdb_r)

urdb_flat <- urdb_flat %>%
  mutate(
    # 1. Safely extract the strings. If it's NULL or empty, make it NA.
    flat_effDate_chars = map_chr(`effectiveDate.$date`, ~ {
      if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]])
    }),
    flat_endDate_chars = map_chr(`endDate.$date`, ~ {
      if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]])
    }),

    # 2. Now that we have a standard character vector, parse the dates!
    effectiveDate = as.POSIXct(flat_effDate_chars, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    endDate = as.POSIXct(flat_endDate_chars, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  ) %>%
  # Optional: Drop the temporary character column if you want
  select(-c(flat_effDate_chars, flat_endDate_chars))

urdb_select <- urdb_flat |>
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
