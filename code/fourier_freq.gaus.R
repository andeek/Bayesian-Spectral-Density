#source("code/udf_20131202.R")

# Put in udf #
spec_iidgaus <- function(sigma2){
  sigma2/(2*pi)
}

##Simulate Draws
M <- 50000 #number of draws
n <- 100 #length of ts
sigma2 <- 1

draws.gaus <- sapply(1:M, function(x) rnorm(n, 0 ,sqrt(sigma2)))

##Get periodograms at fourier frequencies
perio.gaus <- apply(draws.gaus, 2, function(x) periodogram(x, fourier_freq(length(x)), center=TRUE, plot=FALSE)$spec)

##Test against Exponential(f(w)*2*pi)
spec.gaus <- spec_iidgaus(sigma2)
exp.gaus <- matrix(rexp(M*nrow(perio.gaus), 1/(spec.gaus*2*pi)), nrow=nrow(perio.gaus), ncol=M) #independent exponential draws

ks.test(apply(perio.gaus, 2, prod), apply(exp.gaus, 2, prod))

qplot(quantile(apply(perio.gaus, 2, prod), seq(0,.5,.01)),quantile(apply(exp.gaus, 2, prod), seq(0,.5,.01))) + geom_abline() 



