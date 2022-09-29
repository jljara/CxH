get.KLS.disequilibrium.by.fmla <- function(
  counts1,
  counts2 = rep(1, length(counts1)),
  base = 2,
  tol = 1e-15,
  ...)
{
  N <- length(counts1)
  stopifnot(N > 1)
  stopifnot(N == length(counts2))

  S1 <- sum(counts1)
  stopifnot(S1 > 0)
  S2 <- sum(counts2)
  stopifnot(S2 > 0)
  p1 <- sapply(counts1, function(n) ifelse(n > 0, n / S1, 0))
  p2 <- sapply(counts2, function(n) ifelse(n > 0, n / S2, 0))

  i <- p1 > tol
  stopifnot(all(p2[i] > tol))

  D <- sum(p1[i] * log(p1[i] / p2[i], base = base))
  Q0 <- 1 / log(N, base = base)
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
