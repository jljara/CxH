get.KLS.disequilibrium.to.uniform <- function(
  counts,
  base = 2,
  ...
)
{
  counts.max <- c(1, rep(0, length(counts) - 1))

  D <- get.KLS.distance.to.uniform(
    counts = counts,
    base = base,
    ...)
  Dmax <- get.KLS.distance.to.uniform(
    counts = counts.max,
    base = base,
    ...)
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
