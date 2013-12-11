source("code/udf_20131202.R")

##Simulate Draws
M <- 1000 #number of draws
n <- 500 #length of ts
sigma2 <- 1

draws.gaus <- sapply(1:M, function(x) rnorm(n, 0 ,sqrt(sigma2)))

##Get periodograms at fourier frequencies
perio.gaus <- apply(draws.gaus, 2, function(x) periodogram(x, fourier_freq(length(x)), center=TRUE, plot=FALSE)$spec)
perio.gaus <- perio.gaus*2

##Test against Exponential(f(w)*2*pi)
spec.gaus <- spec_iidgaus(sigma2)
exp.gaus <- matrix(rexp(M*nrow(perio.gaus), 1/(spec.gaus*2*pi)*1/2), nrow=nrow(perio.gaus), ncol=M) #independent exponential draws

ks.test(apply(perio.gaus, 2, prod), apply(exp.gaus, 2, prod))$p.value

qplot(quantile(apply(perio.gaus, 2, prod), seq(0,.6,.01)),quantile(apply(exp.gaus, 2, prod), seq(0,.6,.01))) + geom_abline() 


qplot(dnorm(seq(0,.95,.001)), )

pv<-NULL
for(i in 1:nrow(perio.gaus)){
  p<-ks.test(perio.gaus[i,], pexp)$p.value
  pv<-c(pv,p)
}
sum(pv < 0.01)


