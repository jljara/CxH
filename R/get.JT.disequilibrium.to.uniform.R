get.JT.disequilibrium.to.uniform <- function(
  counts,
  beta = 1/2,
  q,
  ...
)
{
  counts.max <- c(1, rep(0, length(counts) - 1))

  D <- get.jensen.divergence.to.uniform(
    counts = counts,
    beta = beta,
    disorder.fun = get.tsallis.disorder,
    q = q,
    ...
  )
  Dmax <- get.jensen.divergence.to.uniform(
    counts = counts.max,
    beta = beta,
    disorder.fun = get.tsallis.disorder,
    q = q,
    ...
  )
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
