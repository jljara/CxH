get.euclidean.disequilibrium <- function(counts, counts.ref, counts.max_to_ref, ...)
{
  D <- get.euclidean.distance(counts, counts.ref, ...)
  Dmax <- get.euclidean.distance(counts.max_to_ref, counts.ref, ...)
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
