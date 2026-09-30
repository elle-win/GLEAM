#' Validate inputs for calc_flw_milk_loss
#'
#' @noRd
validate_flw_milk_loss_inputs <- function(q_milk, flw_loss_fraction, ef_farm) {
  validate_scalar_numeric(q_milk)
  validate_scalar_numeric(flw_loss_fraction)
  validate_scalar_numeric(ef_farm)
  if (q_milk < 0) {
    cli::cli_abort("{.arg q_milk} must be greater than or equal to 0.")
  }
  if (flw_loss_fraction < 0 || flw_loss_fraction > 1) {
    cli::cli_abort("{.arg flw_loss_fraction} must be between 0 and 1.")
  }
  if (ef_farm < 0) {
    cli::cli_abort("{.arg ef_farm} must be greater than or equal to 0.")
  }
}

#' Validate inputs for calc_flw_sequential_loss
#'
#' @noRd
validate_flw_sequential_loss_inputs <- function(
    q_produced,
    loss_rates,
    ef_farm,
    delta_ef
) {
  validate_scalar_numeric(q_produced)
  validate_scalar_numeric(ef_farm)
  if (q_produced < 0) {
    cli::cli_abort("{.arg q_produced} must be greater than or equal to 0.")
  }
  if (ef_farm < 0) {
    cli::cli_abort("{.arg ef_farm} must be greater than or equal to 0.")
  }
  if (!is.numeric(loss_rates) || length(loss_rates) < 1L || anyNA(loss_rates)) {
    cli::cli_abort("{.arg loss_rates} must be a non-empty numeric vector.")
  }
  if (!is.numeric(delta_ef) || anyNA(delta_ef)) {
    cli::cli_abort("{.arg delta_ef} must be a numeric vector.")
  }
  if (length(loss_rates) != length(delta_ef)) {
    cli::cli_abort("{.arg loss_rates} and {.arg delta_ef} must have the same length.")
  }
  if (any(loss_rates < 0 | loss_rates > 1)) {
    cli::cli_abort("{.arg loss_rates} values must be between 0 and 1.")
  }
}
