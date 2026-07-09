library(jsonlite)
library(tidyverse)
library(arrow)

urdb <- fromJSON("./data/usurdb.json")

energy <- urdb_select |>
  filter(sector == "Commercial") |>
  select(eiaId, revision_date, rateName, utilityName, sector, energyMin, energyMax, energyRateStrux, energyWeekdaySched, energyWeekendSched)

energy_period <- energy %>%
  mutate(
    energyRateStrux = map(
      energyRateStrux,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, period = (seq_len(n())))
      }
    )
  )

energy_period <- energy_period |> unnest(energyRateStrux)

energy_tier <- energy_period %>%
  mutate(
    energyRateTiers = map(
      energyRateTiers,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, tier = (seq_len(n())))
      }
    )
  )

energy_tier <- energy_tier |> unnest(energyRateTiers)

energy_strux <- energy_tier |> select(-c(energyWeekdaySched, energyWeekendSched))
energy_strux <- energy_strux |>
  mutate(
    across(c(rate, adj), ~ coalesce(.x, 0)),
    across(c(energyMin, energyMax, max), ~ coalesce(.x, -1)),
    total_rate = rate + adj
  ) |>
  relocate(
    tier,
    .after = period
  )
