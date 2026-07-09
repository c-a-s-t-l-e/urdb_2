library(jsonlite)
library(tidyverse)
library(arrow)

urdb <- fromJSON("./data/usurdb.json")

demand <- urdb_select |>
  filter(sector == "Commercial") |>
  select(eiaId, rateName, utilityName, sector, demandMin, demandMax, demandRateUnits, demandRateStrux, demandWeekdaySched, demandWeekendSched)

demand_period <- demand %>%
  mutate(
    demandRateStrux = map(
      demandRateStrux,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, period = (seq_len(n())))
      }
    )
  )

demand_period <- demand_period |> unnest(demandRateStrux)

demand_tier <- demand_period %>%
  mutate(
    demandRateTiers = map(
      demandRateTiers,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, tier = (seq_len(n())))
      }
    )
  )

demand_tier <- demand_tier |> unnest(demandRateTiers)

demand_strux <- demand_tier |> select(-c(demandWeekdaySched, demandWeekendSched))
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
