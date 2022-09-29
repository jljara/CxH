get.KLR.distance.to.uniform <- function(
  counts,
  q,
  base = 2,
  ...)
{
  N <- length(counts)
  stopifnot(N > 0)

  Rq1 <- get.renyi.entropy(counts = counts, alpha = q, base = base, ...)

  counts.uniform <- rep(1, length(counts))
  Rq2 <- get.renyi.entropy(counts = counts.uniform, alpha = q, base = base, ...)

  invisible(Rq2 - Rq1)
}
