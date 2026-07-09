library(jsonlite)
library(tidyverse)
library(arrow)

urdb <- fromJSON("./data/usurdb.json")

urdb_r <- urdb |>
  unnest(revisions) |>
  rename(
    revision_date = date
  ) |>
  group_by(
    utilityName
  ) |>
  slice_max(revision_date, with_ties = TRUE, na_rm = TRUE) |>
  ungroup()
