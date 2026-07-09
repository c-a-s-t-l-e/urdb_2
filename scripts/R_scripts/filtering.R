library(jsonlite)
library(tidyverse)
library(arrow)

get_most_recent_plans <- function(data) {
  check_required_cols(data, c("utility", "latest_update", "sector"))

  data |>
    dplyr::group_by(.data$utility) |>
    dplyr::slice_max(.data$latest_update, with_ties = TRUE, na_rm = TRUE) |>
    dplyr::ungroup()
}

urdb <- fromJSON("./data/usurdb.json")

demand <- urdb |>
  filter(sector == "Commercial") |>
  select(eiaId, rateName, utilityName, sector, demandMin, demandMax, demandRateUnits, demandRateStrux, demandWeekdaySched, demandWeekendSched)

demand <- demand %>%
  mutate(
    demandRateStrux = map(
      demandRateStrux,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, period = (seq_len(n()) - 1))
      }
    )
  )

demand <- demand |> unnest(demandRateStrux)

demand <- demand %>%
  mutate(
    demandRateTiers = map(
      demandRateTiers,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, tier = (seq_len(n()) - 1))
      }
    )
  )

demand <- demand |> unnest(demandRateTiers)

demand_strux <- demand |> select(-c(demandWeekdaySched, demandWeekendSched))
demand_strux <- demand_strux |>
  mutate(
    across(c(rate, adj), ~ coalesce(.x, 0)),
    across(c(demandMin, demandMax, max), ~ coalesce(.x, -1)),
    total_rate = rate + adj
  ) |>
  relocate(
    tier,
    .after = period
  )

mat_to_month_hour <- function(m) {
  if (is.null(m)) {
    return(tibble::tibble(month = integer(), hour = integer(), value = numeric()))
  }

  # sometimes you get a 1-element list wrapping the matrix
  if (is.list(m) && length(m) == 1) {
    m <- m[[1]]
    if (is.null(m)) {
      return(tibble::tibble(month = integer(), hour = integer(), value = numeric()))
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
    mutate(hour = as.integer(sub("hour_", "", hour)))
}

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
