library(jsonlite)
library(tidyverse)
library(arrow)

urdb <- fromJSON("./data/usurdb.json")

demand <- urdb_select |>
  filter(sector == "Commercial") |>
  select(eiaId, rateName, utilityName, sector, demandMin, demandMax, demandRateUnits, demandRateStrux, demandWeekdaySched, demandWeekendSched)

mat_to_month_hour <- function(m) {
  if (is.null(m)) {
    return(tibble::tibble(month = integer(), hour = integer(), period = numeric()))
  }

  # sometimes you get a 1-element list wrapping the matrix
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

demand_sched <- demand |>
  select(c(eiaId, rateName, utilityName, sector, demandMin, demandMax, demandRateUnits, demandWeekdaySched, demandWeekendSched)) |>
  distinct()

cols_to_expand <- c("demandWeekdaySched", "demandWeekendSched") # <-- add more

demand_sched <- demand_sched %>%
  mutate(across(all_of(cols_to_expand),
    ~ map(.x, mat_to_month_hour),
    .names = "tmp_{.col}"
  )) %>%
  tidyr::pivot_longer(
    cols = starts_with("tmp_"),
    names_to = "sched_var",
    values_to = "tmp"
  ) %>%
  tidyr::unnest(tmp) %>%
  mutate(sched_var = sub("^tmp_", "", sched_var)) |>
  select(-c(demandWeekdaySched, demandWeekendSched))
