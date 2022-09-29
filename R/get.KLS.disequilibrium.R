get.KLS.disequilibrium <- function(
  counts,
  counts.ref,
  counts.max_to_ref,
  base = 2,
  tol = 1e-15,
  ...)
{
  D <- get.KLS.distance(
    counts1 = counts,
    counts2 = counts.ref,
    base = base,
    tol = tol,
    ...
  )
  Dmax <- get.KLS.distance(
    counts1 = counts.max_to_ref,
    counts2 = counts.ref,
    base = base,
    tol = tol,
    ...
  )
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
