get.renyi.entropy <- function(counts, alpha, base = 2)
{
  stopifnot(alpha > 0)
  if(alpha == 1)
    return(get.shannon.entropy(counts, base = base))

  S <- sum(counts)
  stopifnot(S > 0)

  p <- counts[counts > 0] / S
  (1 / (1 - alpha)) * log(sum(p^alpha), base = base)
}
