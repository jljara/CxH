get.tsallis.entropy <- function(counts, q, base = 2)
{
  if(q == 1)
    return(get.shannon.entropy(counts, base = base))

  S <- sum(counts)
  stopifnot(S > 0)

  p <- counts[counts > 0] / S
  (1 - sum(p^q)) / (q - 1)
}
