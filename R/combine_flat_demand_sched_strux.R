combine_flat_demand_sched_strux <- function(dataset_1, dataset_2) {
  dataset_1 |>
    left_join(
      dataset_2 %>%
        select(eiaId, rateName, utilityName, sector, period, tier, flatDemandUnits, total_rate, max),
      by = c("eiaId", "rateName", "utilityName", "sector", "period")
    )
}
