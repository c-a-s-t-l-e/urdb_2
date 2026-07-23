library(targets)
library(tidyverse)
library(jsonlite)
library(pointblank)
library(arrow)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "jsonlite", "pointblank", "arrow"), # Packages that your targets need for their tasks.
  format = "qs", # set default storage format

  # The following sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # read in URDB json file
  tar_target(
    urdb,
    fromJSON("./data/usurdb.json"),
    format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    urdb_reformated,
    format_raw_urdb(urdb),
    format = "qs"
  ),
  tar_target(
    demand_sched,
    make_demand_sched(urdb_reformated),
    format = "qs"
  ),
  tar_target(
    demand_strux,
    make_demand_strux(urdb_reformated),
    format = "qs"
  ),
  tar_target(
    flat_demand_sched,
    make_flat_demand_sched(urdb_reformated),
    format = "qs"
  ),
  tar_target(
    flat_demand_strux,
    make_flat_demand_strux(urdb_reformated),
    format = "qs"
  ),
  tar_target(
    energy_sched,
    make_energy_sched(urdb_reformated),
    format = "qs"
  ),
  tar_target(
    energy_strux,
    make_energy_strux(urdb_reformated),
    format = "qs"
  ),
  tar_target(
    demand_combo,
    combine_demand_sched_strux(demand_sched, demand_strux),
    format = "qs"
  ),
  tar_target(
    flat_demand_combo,
    combine_flat_demand_sched_strux(flat_demand_sched, flat_demand_strux),
    format = "qs"
  ),
  tar_target(
    energy_combo,
    combine_energy_sched_strux(energy_sched, energy_strux),
    format = "qs"
  ),
  tar_target(
    energy_multi_unit,
    make_multi_unit_flag((energy_combo))
  ),
  tar_target(
    demand_cost,
    calculate_levelized_demand_costs(demand_combo,
      peak_demand_kW = 500
    ),
    format = "qs"
  ),
  tar_target(
    flat_demand_cost,
    calculate_levelized_flat_demand_costs(flat_demand_combo,
      peak_demand_kW = 500
    ),
    format = "qs"
  ),
  tar_target(
    energy_cost,
    calculate_levelized_energy_costs(energy_multi_unit,
      monthly_usage_kWh = 2000,
      peak_demand_kW = 500
    ),
    format = "qs"
  ),
  tar_target(
    demand_total_sched,
    combine_demand_schedules(demand_sched, demand_cost, flat_demand_cost),
    format = "qs"
  ),
  tar_target(
    energy_total_sched,
    combine_energy_schedules(energy_sched, energy_cost),
    format = "qs"
  ),
  tar_target(
    utility_rates,
    compile_all_utility_rates(urdb_reformated, demand_total_sched, energy_total_sched),
    format = "qs"
  ),
  tar_target(
    no_irrelevant_plans,
    remove_irrelevant_plans(utility_rates),
    format = "qs"
  ),
  tar_target(
    no_high_fixed_charges,
    remove_high_fixed_charge_plans(no_irrelevant_plans),
    format = "qs"
  ),
  tar_target(
    no_high_energy_rate_plans,
    remove_high_energy_rate_plans(no_high_fixed_charges),
    format = "qs"
  ),
  # tar_target(
  #   multi_unit_plans,
  #   remove_single_unit_plans(no_high_energy_rate_plans),
  #   format = "qs"
  # ),
  tar_target(
    no_multi_unit_plans,
    remove_multi_unit_plans(no_high_energy_rate_plans),
    format = "qs"
  ),
  tar_target(
    no_negative_rate_plans,
    remove_negative_rate_plans(no_multi_unit_plans),
    format = "qs"
  ),
  tar_target(
    validation_dashboard,
    generate_validation_report(
      input_dataset = urdb_reformated,
      output_dataset = no_negative_rate_plans,
      output_html_path = "outputs/data_quality_report.html"
    ),
    format = "file"
  ),
  tar_target(
    parquet_rates,
    write_parquet(no_multi_unit_plans, "outputs/utility_rates.parquet"),
    format = "qs"
  ),
  tar_target(
    json_rates,
    convert_to_json_format(no_multi_unit_plans),
    format = "qs"
  ),
  tar_target(
    save_json_rates,
    writeLines(json_rates, "outputs/utility_rates.json")
  ),
  tar_target(
    zipped_outputs,
    {
      # Wait for all the files to be written
      force(validation_dashboard)
      force(parquet_rates)
      force(save_json_rates)

      zip_file <- "outputs.zip"

      # Zip the outputs directory
      zip(
        zipfile = zip_file,
        files = "outputs"
      )

      return(zip_file)
    },
    format = "file"
  )
)
