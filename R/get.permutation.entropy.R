get.permutation.entropy <- function(signal, embdim){
  opd = ordinal_pattern_distribution(x = signal, ndemb = embdim)
  permutation_entropy(opd)
}
