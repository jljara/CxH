
get.shannon.entropy <- function(counts, base = 2)
{
  p <- counts[counts > 0] / sum(counts)
  (-sum(p * log(p, base = base)))
}
