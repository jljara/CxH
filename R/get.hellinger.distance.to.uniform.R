get.hellinger.distance.to.uniform <- function(counts, ...)
{
  N <- length(counts)
  stopifnot(N > 0)

  S1 <- sum(counts)
  stopifnot(S1 > 0)

  p1 <- sapply(counts, function(n) ifelse(n > 0, n / S1, 0))
  p2 <- rep(1, length(counts)) / N

  invisible(sqrt(1/2 * sum((sqrt(p1) - sqrt(p2))^2)))
}
