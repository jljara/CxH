get.wootters.disequilibrium.by.fmla <- function(counts1, counts2, ...)
{
  N <- length(counts1)
  stopifnot(N > 0)
  stopifnot(N == length(counts2))

  S1 <- sum(counts1)
  stopifnot(S1 > 0)
  S2 <- sum(counts2)
  stopifnot(S2 > 0)
  p1 <- sapply(counts1, function(n) ifelse(n > 0, n / S1, 0))
  p2 <- sapply(counts2, function(n) ifelse(n > 0, n / S2, 0))

  D <- acos(sum(sqrt(p1) * sqrt(p2)))
  Q0 <- 1 / acos(sqrt(1 / N))
  Q <- Q0 * D

  data.frame(D, Q0, Q)
}
