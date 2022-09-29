
# for compability, q = alpha
get.renyi.disorder <- function(counts, q, base = 2, tol = 1e-15, ...)
{
  stopifnot(q > 0)
  if(q == 1)
    return(get.shannon.disorder(counts, base = base, tol = tol, ...))

  N <- length(counts)
  stopifnot(N > 0)
  S <- sum(counts)
  stopifnot(S > 0)

  p <- counts[counts > 0] / S
  S <- sum(p)
  stopifnot(any(S > (1 - tol), S < (1 + tol)))

  S <- (1 / (1 - q)) * log(sum(p^q), base = base)
  Smax <- log(N, base = base)
  H <- S / Smax

  data.frame(S, Smax, H)
}
