get.KLR.disequilibrium.by.fmla <- function(
  counts,
  counts.ref,
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

  S1 <- sum(counts)
  stopifnot(S1 > 0)
  S2 <- sum(counts.ref)
  stopifnot(S2 > 0)
  p1 <- sapply(counts, function(n) ifelse(n > 0, n / S1, 0))
  p2 <- sapply(counts.ref, function(n) ifelse(n > 0, n / S2, 0))

  i <- p1 > tol
  stopifnot(all(p2[i] > tol))

  D <- (1 / (q - 1)) * log(sum(p1[i]^q * p2[i]^(1 - q)), base = base)
  Q0 <- 1 / log(N, base = base)
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
