get.hellinger.distance <- function(counts1, counts2, ...)
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

  sqrt(1/2 * sum((sqrt(p1) - sqrt(p2))^2))
}
