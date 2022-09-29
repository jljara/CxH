get.KLR.disequilibrium.to.uniform <- function(
  counts,
  q,
  base = 2,
  ...
)
{
  counts.max <- c(1, rep(0, length(counts) - 1))

  D <- get.KLR.distance.to.uniform(
    counts = counts,
    q = q,
    base = base,
    ...
  )
  Dmax <- get.KLR.distance.to.uniform(
    counts = counts.max,
    q = q,
    base = base,
    ...
  )
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
