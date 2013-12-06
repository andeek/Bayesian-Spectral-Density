setwd("U:/Documents/Dropbox/Stat 520/Project")

## fourier_freq, periodogram, spec_ar1, spec_iidgaus
source("udf_20131202.R")



M <- 1000 #number of draws
n <- 100 #length of ts
sigma2 <- 1

## Simulate from distribution ##
freq <- fourier_freq(n)

draws.gaus <- sapply(1:M, function(x) rnorm(n, 0 ,sqrt(sigma2)))

## Test exponential distribution at each frequency (iid N(0, sigma2) case)
test_exp_gaus <- function(data, freq, sigma2, alpha) {
  perio <- apply(data, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
  
  ks <- apply(perio, 1, function(x) ks.test(x, "pexp", sigma2)$p.value)            
  return(list(pvals=ks, fails=which(ks < alpha)))
}

test_full <- test_exp_gaus(draws.gaus, freq, sigma2, 0.01)
test_full$pvals[test_full$fails]

 
## (AR(1) case)
M <- 1000 #number of draws
n <- 100 #length of ts
phi <- runif(1,0,1) #weakly dependent causal AR
sigma2 <- 1
freq <- fourier_freq(n)

model.ar1 <- list(ar=phi) 
draws.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))

## Function to test exponential distribution at each frequency
test_exp_ar1 <- function(data, freq, sigma2, alpha, phi) {
  perio <- apply(data, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
  spect <- spec_ar1(phi, sigma2, freq)
  ks<-NULL
  for(i in 1:length(spect)){
    p <- ks.test(perio[i,], "pexp", 1/(spect[i]*2*pi))$p.value
    ks <- c(ks, p)
  }
  return(list(pvals=ks, fails=which(ks < alpha)))
}

test_ar1 <- test_exp_ar1(draws.ar1, freq, sigma2, 0.01, phi)
length(test_ar1$fails)


#### Test of independence ####
# Function to test evenly spaced pairs
test_ind_evenly <- function(perio, freq, space, alpha) {
  if(space > length(freq) - 2) stop("space between Fourier frequencies is too large")
  pairs<-data.frame(x=seq(1,(length(freq)-space-1),1), y=seq(1+(space+1), length(freq),1))
  pvals<-apply(pairs, 1, function(x) cor.test(perio[x[1],], perio[x[2],], method="spearman")$p.value)
  return(list(pvals=pvals, freq_pairs=pairs, pairs_fails=pairs[which(pvals < alpha),], pval_fails=pvals[pvals < alpha], prop_fails=sum(pvals < alpha)/nrow(pairs)))
}

## IID GAUSSIAN CASE ##
draws.gaus <- sapply(1:M, function(x) rnorm(n, 0 ,sqrt(sigma2)))
perio.gaus <- apply(draws.gaus, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)

neighbors <- test_ind_evenly(perio.gaus, freq, 0, 0.01)
one_sep <- test_ind_evenly(perio.gaus, freq, 1, 0.01)
two_sep <- test_ind_evenly(perio.gaus, freq, 2, 0.01)

fails <- NULL
for(i in 1:(length(freq)-2)){
  fs <- test_ind_evenly(perio.gaus, freq, i, 0.01)$pval_fails
  fails <- c(fails,fs)
}

## AR(1) CASE ##
M=5000
n=500
freq=fourier_freq(n)
model.ar1 <- list(ar=phi) 
draws.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))
perio.ar1 <- apply(draws.ar1, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)

fails <- NULL
for(i in 0:(length(freq)-2)){
  fs <- test_ind_evenly(perio.ar1, freq, i, 0.001)$pvals_fails
  fails <- c(fails,fs)
  cat("\r")
  cat(paste("Spacing:", i), "\r") 
}

qplot(1:248, fails) # what the f*#@
qplot(1:248, fails, geom="line")

which_fails <- list()
for(i in 0:(length(freq)-2)){
  test<-test_ind_evenly(perio.ar1, freq, i, 0.05)
  which_fails[[i+1]] <- test$pairs_fails
  which_fails[[i+1]]$z <- rep(i, nrow(which_fails[[i+1]]))
  which_fails[[i+1]]$p <- test$pval_fails
  cat("\r")
  cat(paste("Spacing:", i), "\r") 
}

which_fails.df<-do.call(rbind, which_fails)

qplot(data=which_fails.df, x=x, xend=y, y=z, yend=z, geom="segment", xlab="j", ylab="spacing") + 
  scale_x_continuous(breaks=seq(1,500,1)) + 
  scale_y_continuous(breaks=seq(1,498,1))


qplot(data=which_fails.df, x=x, y=y, colour=p, xlab="j", ylab="j", main="Correlations significantly different from 0") + geom_abline(intercept=1) + coord_fixed(xlim=c(-5, n/2+3), ylim=c(-5,n/2+3))


