get.KLT.distance.to.uniform <- function(
  counts,
  q,
  ...)
{
  stopifnot(q != 1)
  N <- length(counts)
  stopifnot(N > 0)

  Tq1 <- get.tsallis.entropy(counts = counts, q = q, ...)

  counts.uniform <- rep(1, length(counts))
  Tq2 <- get.tsallis.entropy(counts = counts.uniform, q = q, ...)

  invisible(N^(q - 1) * (Tq2 - Tq1))
}
