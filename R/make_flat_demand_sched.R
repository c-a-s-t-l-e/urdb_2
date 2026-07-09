library(jsonlite)
library(tidyverse)
library(arrow)

make_flat_demand_sched <- function(dataset) {
  dataset |>
    select(
      eiaId,
      rateName,
      utilityName,
      sector,
      flatDemandMonths
    ) |>
    unnest(flatDemandMonths) |>
    rename(
      period = flatDemandMonths
    ) |>
    mutate(
      month = rep(1:12, length.out = n())
    )
}
