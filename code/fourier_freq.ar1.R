library(plyr)
source("code/udf_20131202.R")

set.seed(1001)

##Simulate Draws
M <- 1000 #number of draws
n <- 230 #length of ts
phi <- runif(1,0,1) #causal AR
sigma2 <- 1

model.ar1 <- list(ar=phi) 
draws.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))

##Get periodograms at fourier frequencies
perio.ar1 <- apply(draws.ar1, 2, function(x) periodogram(x, fourier_freq(length(x)), center=TRUE, plot=FALSE)$spec)

##Test against Exponential(f(w)*2*pi)
spec.ar1 <- spec_ar1(phi, sigma2, fourier_freq(nrow(draws.ar1)))
exp.ar1 <- t(sapply(spec.ar1, function(x) rexp(1000, x*2*pi))) #independent exponential draws

ks.test(apply(perio.ar1, 2, prod), apply(exp.ar1, 2, prod))