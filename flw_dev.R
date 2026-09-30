# Current gleam + milk FLW (farm-gate CORE/RUN). Open GLEAM.Rproj, then Source.
if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else if (!requireNamespace("gleam", quietly = TRUE)) {
  stop("Install gleam or use pkgload::load_all() in this folder.")
} else {
  library(gleam)
}

source("flw_check.R")
message("gleam ", as.character(utils::packageVersion("gleam")))
message("run_gleam(..., flw_loss = data.table(herd_id, flw_loss_fraction))")
message("results_flw uses EF = allocated Milk CO2eq / milk mass from that run")
