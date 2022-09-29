
get.KLR.disequilibrium <- function(
  counts,
  counts.ref,
  counts.max_to_ref,
  q,
  base = 2,
  tol = 1e-15,
  ...
)
{
  stopifnot(q != 1)
  N <- length(counts)
  stopifnot(N > 1)
  stopifnot(N == length(counts.ref))
  stopifnot(N == length(counts.max_to_ref))

  D <- get.KLR.distance(
    counts1 = counts,
    counts2 = counts.ref,
    q = q,
    base = base,
    tol = tol,
    ...
  )
  Dmax <- get.KLR.distance(
    counts1 = counts.max_to_ref,
    counts2 = counts.ref,
    q = q,
    base = base,
    tol = tol,
    ...
  )
  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
