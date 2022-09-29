get.normalized.lempel.ziv.complexity <- function(signal,
                alphabet= c(0,1)){
  meanLZC <- mean(signal)
  s<-map(signal, function(x) if(x < meanLZC) 0 else 1)
  denominator <- length(s)/log(x=length(s), base=2)
  return(get.lempel.ziv.complexity(s)/denominator)
}
