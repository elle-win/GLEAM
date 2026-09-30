test_that("calc_flw_milk_loss is q times L times EF", {
  result <- calc_flw_milk_loss(
    q_milk = 1e6,
    flw_loss_fraction = 0.02,
    ef_farm = 0.85
  )
  expect_equal(result$q_lost, 20000)
  expect_equal(result$e_loss_kgco2e, 17000)
})

test_that("calc_flw_sequential_loss matches golden test", {
  got <- calc_flw_sequential_loss(
    1e6,
    c(0.01, 0.02, 0.03, 0.01),
    0.85,
    c(0, 0.30, 0.10, 0.20)
  )
  expect_equal(got$e_total_kgco2e, 81298.363, tolerance = 1e-6)
})

test_that("run_flw_module uses milk EF from this run", {
  results_production <- data.table::data.table(
    herd_id = "h1",
    species_short = "CTL",
    variable_name = "milk_production_mass_cohort",
    value_total = 1000
  )
  results_emissions <- data.table::data.table(
    herd_id = "h1",
    species_short = "CTL",
    commodity_name = "Milk",
    value_total_allocated_co2eq = 2000
  )
  flw_loss <- data.table::data.table(
    herd_id = "h1",
    flw_loss_fraction = 0.1
  )
  out <- run_flw_module(
    results_production,
    results_emissions,
    flw_loss,
    show_indicator = FALSE
  )
  expect_equal(out$ef_farm, 2)
  expect_equal(out$q_lost, 100)
  expect_equal(out$e_loss_kgco2e, 200)
})
