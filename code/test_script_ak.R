source("code/udf_20131202.R")

M <- 1000 #number of draws
n <- 500 #length of ts
s <- 200 #number of sims
sigma2 <- 1
alpha <- 0.05

freq <- fourier_freq(n)

model.iid <- list()

model.ar1 <- list(ar=0.5)
model.ar4 <- list(ar=round(c(0.07794297, 0.32976393, 0.10472191, 0.45397725),2))

model.ma1 <- list(ma=0.7)
model.ma2 <- list(ma=c(0.7, 0.3))

model.arma41 <- list(ar=round(c(0.07794297, 0.32976393, 0.10472191, 0.45397725),2), ma=c(.7))
model.arma42 <- list(ar=round(c(0.07794297, 0.32976393, 0.10472191, 0.45397725),2), ma=c(.7,.3))

sim_ind_tests<-function(model, freq, space=0, s=200, M=1000, alpha=0.05, sigma2=1){
  require(plyr)
  ps <- matrix(NA, nrow=(248-space), ncol=s)
  es <- NULL
  for(j in 1:s){
    draws <- sapply(1:M, function(x) arima.sim(model, n=n, sd=sqrt(sigma2)))
    perio <- apply(draws, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
    ps[,j] <- test_ind_evenly(perio, freq, space, alpha)$pvals < alpha
    es <- c(es, test_exp(draws, perio, spec_arma(model, sigma2, freq), alpha)$fails)
    cat("\r")
    cat(j, "\r") 
  }
  
  es <- as.data.frame(es)
  es.final<- ddply(es, .(es), nrow)
  es.final <- merge(x=es.final, y=data.frame(freq = 1:length(freq)), by.x="es", by.y="freq", all.y=TRUE)
  es.final[is.na(es.final$V1), "V1"] <- 0
  es.final$prop <- es.final$V1/s
  
  return(list(prop_fails.ind=apply(ps, 1, sum)/s, pro_fails.exp=es.final))
}

tests.iid<-sim_ind_tests(model.iid, freq)

tests.ar1<-sim_ind_tests(model.ar1, freq)

tests.ar4<-sim_ind_tests(model.ar4, freq)

tests.ma1<-sim_ind_tests(model.ma1, freq)

tests.ma2<-sim_ind_tests(model.ma2, freq)

tests.arma41<-sim_ind_tests(model.arma41, freq)

tests.arma42<-sim_ind_tests(model.arma42, freq)

draws.iid <- sapply(1:M, function(x) arima.sim(model.iid, n=n, sd=sqrt(sigma2)))
perio.iid <- apply(draws.iid, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.iid <- spec_arma(model.iid, sigma2, freq)

draws.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))
perio.ar1 <- apply(draws.ar1, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.ar1 <- spec_arma(model.ar1, sigma2, freq)

draws.ar4 <- sapply(1:M, function(x) arima.sim(model.ar4, n=n, sd=sqrt(sigma2)))
perio.ar4 <- apply(draws.ar4, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.ar4 <- spec_arma(model.ar4, sigma2, freq)

draws.ma1 <- sapply(1:M, function(x) arima.sim(model.ma1, n=n, sd=sqrt(sigma2)))
perio.ma1 <- apply(draws.ma1, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.ma1 <- spec_arma(model.ma1, sigma2, freq)

draws.ma2 <- sapply(1:M, function(x) arima.sim(model.ma2, n=n, sd=sqrt(sigma2)))
perio.ma2 <- apply(draws.ma2, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.ma2 <- spec_arma(model.ma2, sigma2, freq)

draws.arma41 <- sapply(1:M, function(x) arima.sim(model.arma41, n=n, sd=sqrt(sigma2)))
perio.arma41 <- apply(draws.arma41, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.arma41 <- spec_arma(model.arma41, sigma2, freq)

draws.arma42 <- sapply(1:M, function(x) arima.sim(model.arma42, n=n, sd=sqrt(sigma2)))
perio.arma42 <- apply(draws.arma42, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
spec.arma42 <- spec_arma(model.arma42, sigma2, freq)


save.image(file="tests.RData") #periodic saves in case we get booted