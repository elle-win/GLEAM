# Current gleam package + FLW sidecar (not in NAMESPACE yet).
# In RStudio: open GLEAM.Rproj, then Source this file.

if (!requireNamespace("gleam", quietly = TRUE)) {
  stop(
    "Install gleam from this folder or from GitHub first, e.g.\n",
    "  remotes::install_local(\".\", upgrade = \"never\")"
  )
}

library(gleam)
source("R/flw_sequential_loss.R", local = FALSE)

got <- flw_sequential_loss(
  1e6,
  c(0.01, 0.02, 0.03, 0.01),
  0.85,
  c(0, 0.30, 0.10, 0.20)
)
expect <- 81298.363
ok <- abs(got$e_total_kgco2e - expect) < 1e-6
cat(sprintf(
  "e_total_kgco2e = %.6f  expected %.3f  %s\n",
  got$e_total_kgco2e,
  expect,
  if (ok) "PASS" else "FAIL"
))
if (!ok) {
  stop("Golden test failed")
}

message("gleam ", as.character(utils::packageVersion("gleam")), " loaded")
message("FLW is sourced, not inside gleam:: yet")
message("Next: gleam production mass + allocated EF -> flw_sequential_loss()")
