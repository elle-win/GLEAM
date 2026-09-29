# Source this in RStudio to check FLW maths (does not load the gleam package).
source("R/flw_sequential_loss.R")
got <- flw_sequential_loss(
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
