library(statcomp)

get.permutation.lempel.ziv.complexity <- function(signal, embdim){
  opd = ordinal_pattern_time_series(x = signal, ndemb = embdim)
  opd <- opd[!is.na(opd)]
  threshold = mean(opd)
  newSignal = c()
  for (variable in opd) {
    if(variable < threshold){
      newSignal <- append(newSignal, 0)
    }
    else{
      newSignal <- append(newSignal, 1)
    }
  }
  lzc <- get.lempel.ziv.complexity(newSignal, c(0,1))
  n <- length(newSignal)
  denominator <- n/log2(n)
  normalizedLzc <- lzc/denominator

  normalizedLzc

}
