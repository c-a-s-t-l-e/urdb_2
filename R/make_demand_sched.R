mat_to_month_hour <- function(m) {
  if (is.null(m)) {
    return(tibble::tibble(month = integer(), hour = integer(), period = numeric()))
  }

  # sometimes you get a 1-element list
  if (is.list(m) && length(m) == 1) {
    m <- m[[1]]
    if (is.null(m)) {
      return(tibble::tibble(month = integer(), hour = integer(), period = numeric()))
    }
  }

  m <- as.matrix(m)

  m_df <- as.data.frame(m)
  colnames(m_df) <- paste0("hour_", seq_len(ncol(m_df)))
  m_df$month <- seq_len(nrow(m_df))

  m_df %>%
    pivot_longer(
      cols = starts_with("hour_"),
      names_to = "hour",
      values_to = "period"
    ) %>%
    mutate(
      hour = as.integer(sub("hour_", "", hour)),
      period = period + 1
    )
}

make_demand_sched <- function(dataset) {
  cols_to_expand <- c("demandWeekdaySched", "demandWeekendSched")

  dataset |>
    select(
      eiaId,
      rateName,
      utilityName,
      sector,
      demandWeekdaySched,
      demandWeekendSched
    ) |>
    mutate(across(all_of(cols_to_expand),
      ~ map(.x, mat_to_month_hour),
      .names = "tmp_{.col}"
    )) %>%
    tidyr::pivot_longer(
      cols = starts_with("tmp_"),
      names_to = "week_part",
      values_to = "tmp"
    ) %>%
    tidyr::unnest(tmp) %>%
    mutate(week_part = sub("^tmp_", "", week_part)) |>
    mutate(
      week_part = case_when(
        # If the string contains "Weekday", label it "weekday"
        str_detect(week_part, "Weekday") ~ "Weekday",

        # If the string contains "Weekend", label it "weekend"
        str_detect(week_part, "Weekend") ~ "Weekend",
        TRUE ~ "Unknown"
      )
    ) |>
    select(-c(demandWeekdaySched, demandWeekendSched))
}
