library(ggplot2)
source("code/udf_20131202.R")

set.seed(1050)

##Simulate Draws
M <- 1000 #number of draws
n <- 500 #length of ts
phi <- runif(1,0,1) #causal AR
sigma2 <- 1

model.ar1 <- list(ar=phi) 
draws.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))

##Get periodograms at fourier frequencies
freq <- fourier_freq(n)

test <- function(data, freq, start_freq, spec_func) {
  perio <- apply(data, 2, function(x) periodogram(x, freq[freq > start_freq], center=TRUE, plot=FALSE)$spec)
  spect <- do.call(spec_func, args=list(phi, sigma2, freq[freq > start_freq]))
  expon <- t(sapply(spect, function(x) rexp(M, 1/(x*2*pi)))) #independent exponential draws
  
  perio.prod <- apply(perio, 2, prod)
  expon.prod <- apply(expon, 2, prod)
  
  zoom <- ceiling(log10(median(perio.prod)))
  ks <- ks.test(perio.prod, expon.prod)
  g <- qplot(quantile(perio.prod, prob=seq(0,1,by=.001)), quantile(expon.prod, prob=seq(0,1,by=.001))) + geom_abline() + coord_fixed(xlim=c(0,10^(zoom)), ylim=c(0,10^(zoom))) +
          xlab("Periodogram") + ylab("Independent Exponentials") + ggtitle("Quantile Plot")
  return(list(test=ks, graph=g))
}

test.ar1.full <- test(draws.ar1, freq, 0, spec_ar1)
test.ar1.half <- test(draws.ar1, freq, pi/2, spec_ar1)
test.ar1.quarter <- test(draws.ar1, freq, pi/4, spec_ar1)


