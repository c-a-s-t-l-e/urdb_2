make_multi_unit_flag <- function(df) {
  out <- df %>%
    group_by(eiaId, rateName, utilityName, sector) %>%
    mutate(multi_unit = n_distinct(unit) > 1) %>%
    ungroup()

  if (!"multi_unit" %in% names(out)) stop("multi_unit not created")
  out
}
