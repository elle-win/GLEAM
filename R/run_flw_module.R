#' Run milk food-loss (FLW) module
#'
#' After a gleam run, applies a user (or default) edible-loss fraction to
#' herd milk mass and the milk EF from that run (sum of allocated Milk CO2eq
#' divided by milk mass). Does not alter gleam production, allocation, or
#' enteric/manure/feed totals.
#'
#' @param results_production data.table. \code{results_production} from
#'   [run_aggregation_module()].
#' @param results_emissions data.table. \code{results_emissions} from
#'   [run_aggregation_module()].
#' @param flw_loss data.table. One row per herd:
#'   \describe{
#'     \item{herd_id}{Character. Must match gleam herd identifiers.}
#'     \item{flw_loss_fraction}{Numeric. Edible milk lost as a fraction of
#'       gleam milk mass (0--1).}
#'   }
#' @param show_indicator Logical. Progress messages. Default \code{TRUE}.
#'
#' @return A \code{data.table} with columns \code{herd_id},
#'   \code{species_short}, \code{q_milk}, \code{flw_loss_fraction},
#'   \code{ef_farm}, \code{q_lost}, \code{e_loss_kgco2e}.
#'
#' @details
#' \code{ef_farm} is computed per herd as allocated Milk CO2eq / milk mass
#' from this gleam run. Geographic loss fractions are inputs; they are not
#' emission factors.
#'
#' @seealso [calc_flw_milk_loss()], [run_gleam()]
#'
#' @export
#'
#' @importFrom data.table := .SD
run_flw_module <- function(
    results_production,
    results_emissions,
    flw_loss,
    show_indicator = TRUE
) {
  results_production <- data.table::as.data.table(results_production)
  results_emissions <- data.table::as.data.table(results_emissions)
  flw_loss <- data.table::as.data.table(flw_loss)

  validate_run_flw_module_inputs(
    results_production,
    results_emissions,
    flw_loss
  )

  if (show_indicator) {
    cli::cli_status("Calculating milk food-loss emissions\u2026")
  }

  milk_q <- results_production[
    variable_name == "milk_production_mass_cohort",
    list(q_milk = sum(value_total)),
    by = c("herd_id", "species_short")
  ]
  milk_e <- results_emissions[
    commodity_name == "Milk",
    list(e_milk_kgco2e = sum(value_total_allocated_co2eq)),
    by = c("herd_id", "species_short")
  ]

  out <- merge(milk_q, milk_e, by = c("herd_id", "species_short"), all.x = TRUE)
  out[is.na(e_milk_kgco2e), e_milk_kgco2e := 0]
  out <- merge(out, flw_loss[, list(herd_id, flw_loss_fraction)], by = "herd_id", all.x = TRUE)

  missing_l <- out[q_milk > 0 & is.na(flw_loss_fraction), unique(herd_id)]
  if (length(missing_l) > 0) {
    cli::cli_abort(
      "flw_loss is missing herd_id{?s}: {.val {missing_l}}."
    )
  }

  out[is.na(flw_loss_fraction), flw_loss_fraction := 0]
  out[, ef_farm := data.table::fifelse(q_milk > 0, e_milk_kgco2e / q_milk, 0)]
  out[
    ,
    c("q_lost", "e_loss_kgco2e") := calc_flw_milk_loss(
      q_milk = q_milk,
      flw_loss_fraction = flw_loss_fraction,
      ef_farm = ef_farm
    ),
    by = .I
  ]

  if (show_indicator) {
    cli::cli_status_clear()
    cli::cli_alert_success("Milk food-loss calculations completed.")
  }

  out[, list(
    herd_id,
    species_short,
    q_milk,
    flw_loss_fraction,
    ef_farm,
    q_lost,
    e_loss_kgco2e
  )]
}
