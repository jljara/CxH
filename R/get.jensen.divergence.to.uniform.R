get.jensen.divergence.to.uniform <- function(
  counts,
  beta = 1/2,
  disorder.fun,
  q = 1,
  base = 2,
  tol = 1e-15,
  ...   # Pass to disorder.fun
)
{
  stopifnot(beta >= 0, beta <= 1)
  N <- length(counts)
  stopifnot(N > 0)

  S1 <- sum(counts)
  stopifnot(S1 > 0)

  counts.uniform <- rep(1, length(counts))
  S2 <- sum(counts.uniform)
  stopifnot(S2 > 0)

  contr1 <- beta^2 * counts + beta * (1 - beta) * (S2 / S1) * counts
  contr2 <- (1 - beta)^2 * counts.uniform + beta * (1 - beta) * (S1 / S2) * counts.uniform
  mix.counts <- contr1 + contr2

  dis1 <- disorder.fun(counts, q = q, base = base, tol = tol, ...)
  dis2 <- disorder.fun(counts.uniform, q = q, base = base, tol = tol, ...)
  dis12 <- disorder.fun(mix.counts, q = q, base = base, tol = tol, ...)

  dis12[["S"]] - beta * dis1[["S"]] - (1 - beta) * dis2[["S"]]
}
