flat_demand <- urdb_select |>
  filter(sector == "Commercial") |>
  select(eiaId, rateName, utilityName, sector, demandMin, demandMax, flatDemandStrux, flatDemandMonths, flatDemandUnits)

flat_demand_period <- flat_demand %>%
  mutate(
    flatDemandStrux = map(
      flatDemandStrux,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, period = (seq_len(n()) - 1))
      }
    )
  )

flat_demand_period <- flat_demand_period |> unnest(flatDemandStrux)

flat_demand_tier <- flat_demand_period %>%
  mutate(
    flatDemandTiers = map(
      flatDemandTiers,
      ~ if (is.null(.x)) {
        NULL
      } else {
        mutate(.x, tier = (seq_len(n())))
      }
    )
  )

flat_demand_tier <- flat_demand_tier |> unnest(flatDemandTiers)

flat_demand_strux <- flat_demand_tier |> select(-c(flatDemandMonths))

flat_demand_strux <- flat_demand_strux |>
  mutate(
    across(c(rate, adj), ~ coalesce(.x, 0)),
    across(c(demandMin, demandMax, max), ~ coalesce(.x, -1)),
    total_rate = rate + adj
  ) |>
  relocate(
    tier,
    .after = period
  )
