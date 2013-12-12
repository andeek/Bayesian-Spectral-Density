source("/code/udf_20131202.R")

## Ar(4) where independence fails ##
phi <- c(.075, .3, .1, .45) ##c(0.07794297, 0.32976393, 0.10472191, 0.45397725)
M <- 1000 #number of draws
n <- 500 #length of ts
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

ps<-sim_ind_tests(model.ar4b, freq)
qplot(1:length(ps), ps, geom="line") + geom_hline(yintercept=0.05) + geom_smooth()



