# Source this in RStudio from the GLEAM project (after load_all or install).
got <- calc_flw_sequential_loss(
  1e6,
  c(0.01, 0.02, 0.03, 0.01),
  0.85,
  c(0, 0.30, 0.10, 0.20)
)
expect <- 81298.363
ok <- abs(got$e_total_kgco2e - expect) < 1e-6
cat(sprintf("e_total_kgco2e = %.6f  expected %.3f  %s\n",
            got$e_total_kgco2e, expect, if (ok) "PASS" else "FAIL"))
if (!ok) stop("Golden test failed")

farm <- calc_flw_milk_loss(1e6, 0.02, 0.85)
cat(sprintf("farm-gate e_loss_kgco2e = %.1f (Q*L*EF)\n", farm$e_loss_kgco2e))
