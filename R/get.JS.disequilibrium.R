get.JS.disequilibrium <- function(
  counts,
  counts.ref,
  counts.max_to_ref,
  beta = 1/2,
  base = 2,
  ...   # Pass to disorder.fun
)
{
  stopifnot(beta >= 0, beta <= 1)
  N <- length(counts)
  stopifnot(N > 0)
  stopifnot(N == length(counts.ref))
  stopifnot(N == length(counts.max_to_ref))

  D <- get.jensen.divergence(
    counts1 = counts,
    counts2 = counts.ref,
    beta = beta,
    disorder.fun = get.shannon.disorder,
    base = base,
    ...
  )

  Dmax <- get.jensen.divergence(
    counts1 = counts.max_to_ref,
    counts2 = counts.ref,
    beta = beta,
    disorder.fun = get.shannon.disorder,
    base = base,
    ...
  )

  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
