generate_validation_report <- function(input_dataset, output_dataset, output_html_path) {
  # Pre-calculate unique plan counts
  unique_input_plans <- input_dataset |>
    select(eiaId, utilityName, rateName) |>
    distinct() |>
    nrow()

  unique_output_plans <- output_dataset |>
    select(eiaId, utilityName, rateName) |>
    distinct() |>
    nrow()

  # Create the validation agent
  agent <- create_agent(
    tbl = output_dataset,
    label = paste("URDB Data Check -", Sys.Date())
  ) |>
    # Test 1: No plans with fixed charges above 10,000
    col_vals_lte(columns = fixedChargeFirstMeter, value = 10000, label = "Fixed Charge <= 10000") |>
    # Test 2: No plans with energy rates above 100
    col_vals_lte(columns = total_energy_rate, value = 100, label = "Energy Rate <= 100") |>
    # Test 3: No repeating plans
    rows_distinct(
      columns = c(eiaId, utilityName, rateName, month, week_part, hour),
      label = "No repeating plans"
    ) |>
    # Test 4: No ag or electric furnace plans
    col_vals_expr(
      expr = ~ !grepl("agriculture|agricultural|agribusiness|pumping|irrigation|furnace|heating|conditioning|catfish", rateName,
        ignore.case = TRUE
      ),
      label = "No agricultural or electric furnace plans"
    ) |>
    # Test 5: No negative demand rates
    col_vals_gte(columns = total_demand_rate, value = 0, label = "Demand Rate >= 0") |>
    # Test 6: No negative energy rates
    col_vals_gte(columns = total_energy_rate, value = 0, label = "Energy Rate >= 0") |>
    # Test 7: Count check between input and output (as a warning)
    col_vals_expr(
      expr = ~ unique_output_plans == unique_input_plans,
      label = paste0("Plan count matches input (Expected: ", unique_input_plans, ", Got: ", unique_output_plans, ")"),
      actions = action_levels(warn_at = 1)
    ) |>
    interrogate()

  # Export the HTML report
  export_report(
    agent,
    filename = basename(output_html_path),
    path = dirname(output_html_path)
  )

  # Grab metrics from the agent
  agent_intel <- get_agent_x_list(agent)

  # If any of the first 4 steps have failures, stop the pipeline
  if (sum(agent_intel$n_failed[1:4]) > 0) {
    stop(paste(
      "Data validation failed on a critical check! Visual report generated at:",
      output_html_path
    ))
  }

  return(output_html_path)
}
