get.hellinger.disequilibrium <- function(counts, counts.ref, counts.max_to_ref, ...)
{
  D <- get.hellinger.distance(counts, counts.ref, ...)
  Dmax <- get.hellinger.distance(counts.max_to_ref, counts.ref, ...)
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
