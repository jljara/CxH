get.euclidean.disequilibrium.to.uniform <- function(counts,  ...)
{
  counts.max <- c(1, rep(0, length(counts) - 1))

  D <- get.euclidean.distance.to.uniform(counts, ...)
  Dmax <- get.euclidean.distance.to.uniform(counts.max, ...)
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
