#' Validate inputs for run_flw_module
#'
#' @noRd
validate_run_flw_module_inputs <- function(
    results_production,
    results_emissions,
    flw_loss
) {
  check_data_table(results_production, "results_production")
  check_data_table(results_emissions, "results_emissions")
  check_data_table(flw_loss, "flw_loss")
  check_required_columns(
    results_production,
    c("herd_id", "species_short", "variable_name", "value_total"),
    "results_production"
  )
  check_required_columns(
    results_emissions,
    c(
      "herd_id", "species_short", "commodity_name",
      "value_total_allocated_co2eq"
    ),
    "results_emissions"
  )
  check_required_columns(
    flw_loss,
    c("herd_id", "flw_loss_fraction"),
    "flw_loss"
  )
  if (anyDuplicated(flw_loss$herd_id)) {
    cli::cli_abort("{.arg flw_loss} must have one row per {.field herd_id}.")
  }
}
