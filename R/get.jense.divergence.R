get.jensen.divergence <- function(
  counts1,
  counts2,
  beta = 1/2,
  disorder.fun,
  q = 1,
  base = 2,
  tol = 1e-15,
  ...   # Pass to disorder.fun
)
{
  stopifnot(beta >= 0, beta <= 1)
  N <- length(counts1)
  stopifnot(N > 0)
  stopifnot(N == length(counts2))

  S1 <- sum(counts1)
  stopifnot(S1 > 0)
  S2 <- sum(counts2)
  stopifnot(S2 > 0)

  contr1 <- beta^2 * counts1 + beta * (1 - beta) * (S2 / S1) * counts1
  contr2 <- (1 - beta)^2 * counts2 + beta * (1 - beta) * (S1 / S2) * counts2
  mix.counts <- contr1 + contr2

  dis1 <- disorder.fun(counts1, q = q, base = base, tol = tol, ...)
  dis2 <- disorder.fun(counts2, q = q, base = base, tol = tol, ...)
  dis12 <- disorder.fun(mix.counts, q = q, base = base, tol = tol, ...)

  dis12[["S"]] - beta * dis1[["S"]] - (1 - beta) * dis2[["S"]]
}
