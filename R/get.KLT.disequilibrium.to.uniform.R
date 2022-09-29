
# Kullback-Tsallis
get.KLT.disequilibrium.to.uniform <- function(
  counts,
  q,
  ...
)
{
  counts.max <- c(1, rep(0, length(counts) - 1))

  D <- get.KLT.distance.to.uniform(
    counts = counts,
    q = q,
    ...)
  Dmax <- get.KLT.distance.to.uniform(
    counts = counts.max,
    q = q,
    ...)
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
