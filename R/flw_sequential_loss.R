# Sequential FLW loss × embodied GHG. Same arguments as FLW Excel / Python sequential_loss.
# Not loaded by gleam yet — Source this file, or later add to NAMESPACE.

flw_sequential_loss <- function(q_produced, loss_rates, ef_farm, delta_ef) {
  stopifnot(length(loss_rates) == length(delta_ef), q_produced >= 0)
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
