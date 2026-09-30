#' Calculate farm-gate milk food-loss emissions
#'
#' Applies a single edible-loss fraction to gleam milk mass and the milk
#' emission factor from the same gleam run (allocated Milk CO2eq / kg milk).
#' Does not change herd emissions, production mass, or allocation shares.
#'
#' @param q_milk Numeric. Milk mass produced over the assessment period (kg).
#' @param flw_loss_fraction Numeric. Edible milk lost as a fraction of
#'   \code{q_milk} (0--1). Additional to gleam \code{milk_yield_day} (already
#'   milk for human consumption).
#' @param ef_farm Numeric. Farm-gate milk emission intensity from this gleam
#'   run (kg CO2eq / kg milk).
#'
#' @return A named list with:
#' \describe{
#'   \item{q_lost}{Numeric. Milk mass lost (kg).}
#'   \item{e_loss_kgco2e}{Numeric. Embodied GHG in lost milk (kg CO2eq).}
#' }
#'
#' @details
#' \deqn{q\_lost = q\_milk \times flw\_loss\_fraction}
#' \deqn{e\_loss = q\_lost \times ef\_farm}
#'
#' Not avoided production emissions. Post-farm stage add-ons are not applied.
#'
#' @seealso [run_flw_module()], [run_gleam()], [calc_flw_sequential_loss()]
#'
#' @export
calc_flw_milk_loss <- function(q_milk, flw_loss_fraction, ef_farm) {
  validate_flw_milk_loss_inputs(q_milk, flw_loss_fraction, ef_farm)
  q_lost <- q_milk * flw_loss_fraction
  list(
    q_lost = q_lost,
    e_loss_kgco2e = q_lost * ef_farm
  )
}

#' Sequential remaining-mass FLW loss times embodied GHG
#'
#' Same arguments as the Excel / Python sequential engine. GLEAM farm-gate
#' milk uses [calc_flw_milk_loss()] (one fraction, no stage \code{delta_ef}).
#'
#' @param q_produced Numeric. Mass entering the chain (kg).
#' @param loss_rates Numeric vector. Loss fraction of remaining mass at each stage.
#' @param ef_farm Numeric. Farm-gate EF (kg CO2eq / kg).
#' @param delta_ef Numeric vector. Stage EF increments; same length as
#'   \code{loss_rates}. Use 0 for gleam farm-gate only.
#'
#' @return A list of stage vectors and totals, including
#'   \code{e_total_kgco2e}.
#'
#' @seealso [calc_flw_milk_loss()]
#'
#' @export
calc_flw_sequential_loss <- function(q_produced, loss_rates, ef_farm, delta_ef) {
  validate_flw_sequential_loss_inputs(q_produced, loss_rates, ef_farm, delta_ef)
  n <- length(loss_rates)
  q_in <- q_loss <- ef <- e_loss <- numeric(n)
  remaining <- q_produced
  cum_delta <- 0
  for (s in seq_len(n)) {
    q_in[s] <- remaining
    q_loss[s] <- remaining * loss_rates[s]
    remaining <- remaining - q_loss[s]
    cum_delta <- cum_delta + delta_ef[s]
    ef[s] <- ef_farm + cum_delta
    e_loss[s] <- q_loss[s] * ef[s]
  }
  list(
    q_in = q_in,
    q_loss = q_loss,
    q_out = q_in - q_loss,
    ef = ef,
    e_loss = e_loss,
    q_lost_total = sum(q_loss),
    sequential_loss_frac = if (q_produced > 0) sum(q_loss) / q_produced else 0,
    q_reaching_processor = remaining,
    e_total_kgco2e = sum(e_loss),
    e_total_tonnes = sum(e_loss) / 1000
  )
}
