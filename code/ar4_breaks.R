setwd("C:/Users/Maggie/Dropbox/Stat 520/Project")
source("udf_20131202.R")

## Ar(4) where independence fails ##
phi <- c(.08, .33, .1, .45) ##c(0.07794297, 0.32976393, 0.10472191, 0.45397725)
M <- 1000 #number of draws
n <- 100 #length of ts
sigma2 <- 1

freq <- fourier_freq(n)
model.ar4b <- list(ar=phi, ma=c()) 
draws.ar4b <- sapply(1:M, function(x) arima.sim(model.ar4b, n=n, sd=sqrt(sigma2)))
perio.ar4b <- apply(draws.ar4b, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)

### Function to run s simulations and test independence ### 
sim_ind_tests<-function(model, freq, space=0, s=200, M=1000, alpha=0.05, sigma2=1){
  ps<-matrix(NA, nrow=(248-space), ncol=s)
  for(j in 1:s){
    draws<- sapply(1:M, function(x) arima.sim(model, n=n, sd=sqrt(sigma2)))
    perio <- apply(draws, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
    ps[,j]<-test_ind_evenly(perio, freq, space, alpha)$pvals < alpha
    cat("\r")
    cat(j, "\r") 
  }
  return(prop_fails=apply(ps, 1, sum)/s)
}

sim_ind_tests2<-function(model, freq, nfreq=length(freq), space=0, s=200, M=1000, alpha=0.05, sigma2=1){
  ps<-matrix(NA, nrow=(nfreq-space-1), ncol=s)
  for(j in 1:s){
    draws<- sapply(1:M, function(x) arima.sim(model, n=n, sd=sqrt(sigma2)))
    perio <- apply(draws, 2, function(x) periodogram(x, freq[1:nfreq], center=TRUE, plot=FALSE)$spec)
    ps[,j]<-test_ind_evenly(perio, freq[1:nfreq], space, alpha)$pvals < alpha
    cat("\r")
    cat(j, "\r") 
  }
  return(prop_fails=apply(ps, 1, sum)/s)
}

ps<-sim_ind_tests2(model.ar4b, freq,s=100, M=500)
qplot(1:length(ps), ps, geom="line") + geom_hline(yintercept=0.05) + geom_smooth()

ps_matrix<-matrix(ps, ncol=18, nrow=length(ps))
ps_matrix[1:20, 2:18]<-NA

for(i in 1:17){
 ps_matrix[1:(19-i),i+1]<-sim_ind_tests2(model.ar4b, freq, space=i, nfreq=20,s=100, M=500) 
 cat(i, "\n")
}

qplot(1:nrow(ps_matrix), ps_matrix[,3], geom="line") + geom_hline(yintercept=0.05)
