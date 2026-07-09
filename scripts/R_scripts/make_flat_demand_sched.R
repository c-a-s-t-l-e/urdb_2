library(jsonlite)
library(tidyverse)
library(arrow)

urdb <- fromJSON("./data/usurdb.json")

flat_demand <- urdb_select |>
  filter(sector == "Commercial") |>
  select(eiaId, rateName, utilityName, sector, demandMin, demandMax, flatDemandStrux, flatDemandMonths, flatDemandUnits)

flat_demand <- flat_demand |> unnest(flatDemandMonths)

flat_demand_sched <- flat_demand |>
  select(c(eiaId, rateName, utilityName, sector, demandMin, demandMax, flatDemandMonths))
