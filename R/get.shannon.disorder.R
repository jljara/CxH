get.shannon.disorder <- function(counts, base = 2, tol = 1e-15, ...)
{
  N <- length(counts)
  stopifnot(N > 0)
  S <- sum(counts)
  stopifnot(S > 0)

  p <- counts[counts > 0] / S
  S <- sum(p)
  stopifnot(any(S > (1 - tol), S < (1 + tol)))

  S <- -sum(p * log(p, base = base))
  Smax <- log(N, base = base)
  H <- S / Smax

  data.frame(S, Smax, H)
}
