get.KLS.distance.to.uniform <- function(
  counts,
  base = 2,
  ...)
{
  N <- length(counts)
  stopifnot(N > 0)

  S1 <- get.shannon.entropy(counts = counts, base = base, ...)

  counts.uniform <- rep(1, length(counts))
  S2 <- get.shannon.entropy(counts = counts.uniform, base = base, ...)

  invisible(S2 - S1)
}
