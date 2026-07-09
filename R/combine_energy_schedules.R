combine_energy_schedules <- function(energy_sched, energy_cost) {
  energy_sched |>
    # Combine energy_sched with energy_cost to get levelized cost per period
    left_join(
      energy_cost,
      by = c("eiaId", "rateName", "utilityName", "period")
    )
}
