get.KLR.distance <- function(
  counts1,
  counts2,
  q,
  base = 2,
  tol = 1e-15,
  ...
)
{
  stopifnot(q != 1)
  N <- length(counts1)
  stopifnot(N > 0)
  stopifnot(N == length(counts2))

  S1 <- sum(counts1)
  stopifnot(S1 > 0)
  S2 <- sum(counts2)
  stopifnot(S2 > 0)
  p1 <- sapply(counts1, function(n) ifelse(n > 0, n / S1, 0))
  p2 <- sapply(counts2, function(n) ifelse(n > 0, n / S2, 0))

  i <- p1 > tol
  stopifnot(all(p2[i] > tol))

  (1 / (q - 1)) * log(sum(p1[i]^q * p2[i]^(1 - q)), base = base)
}
