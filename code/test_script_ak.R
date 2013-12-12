source("code/udf_20131202.R")

M <- 1000 #number of draws
n <- 500 #length of ts
s <- 200 #number of sims
sigma2 <- 1
alpha <- 0.05

freq <- fourier_freq(n)

model.iid <- list()
model.ar1 <- list(ar=0.5)
model.ar4 <- list(ar=c(.075, .3, .1, .45))

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

g_exp.iid <- qplot(data=tests.iid$pro_fails.exp, x=es, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("IID Gaussian(0,1) - Distribution")

g_ind.iid <- qplot(1:length(tests.iid), tests.iid$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("IID Gaussian(0,1) - Independence")

save.image(file="tests.RData") #periodic saves in case we get booted

tests.ar1<-sim_ind_tests(model.ar1, freq)

g_exp.ar1 <- qplot(data=tests.ar1$pro_fails.exp, x=es, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle(expression(paste("AR(1), ", phi, " = 0.5", sep="")))

g_ind.ar1 <- qplot(1:length(tests.ar1), tests.ar1$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("IID Gaussian(0,1) - Independence")

save.image(file="tests.RData")

tests.ar4<-sim_ind_tests(model.ar4, freq)

g_exp.ar4 <- qplot(data=tests.ar1$pro_fails.exp, x=es, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle(bquote(paste("AR(4), ", phi, " = [",.(paste(model.ar4$ar, collapse=", ")),"]", sep="")))

g_ind.ar4 <- qplot(1:length(tests.ar4), tests.ar1$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("IID Gaussian(0,1) - Independence")

save.image(file="tests.RData")

