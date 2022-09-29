get.escort.tsallis.entropy <- function(counts, q, base = 2)
{
  if(q == 1)
    return(get.shannon.entropy(counts, base = base))

  S <- sum(counts)
  stopifnot(S > 0)

  p <- counts[counts > 0] / S
  (1 / (q - 1)) * (1 - sum(p^(1 / q))^(-q))
}
