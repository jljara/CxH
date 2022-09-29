get.JT.disequilibrium <- function(
  counts,
  counts.ref,
  counts.max_to_ref,
  beta = 1/2,
  q,
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
    disorder.fun = get.tsallis.disorder,
    q = q,
    ...
  )

  Dmax <- get.jensen.divergence(
    counts1 = counts.max_to_ref,
    counts2 = counts.ref,
    beta = beta,
    disorder.fun = get.tsallis.disorder,
    q = q,
    ...
  )

  Q0 <- 1 / Dmax
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
