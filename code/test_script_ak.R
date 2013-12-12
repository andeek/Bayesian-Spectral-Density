library(plyr)
source("code/udf_20131202.R")

M <- 1000 #number of draws
n <- 500 #length of ts
sigma2 <- 1

freq <- fourier_freq(n)

model.iid <- list()
model.ar1 <- list(ar=0.5)
model.ar4 <- list(ar=c(0.15, 0.05, 0.1, 0.5))

fails.exp.iid <- NULL
for(i in 1:500) {
  x.iid <- sapply(1:M, function(x) arima.sim(model.iid, n=n, sd=sqrt(sigma2)))
  perio.iid <- apply(x.iid, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
  test_exp.iid <- test_exp(x.iid, perio.iid, spec_arma(model.iid, sigma2, freq), 0.01)
  fails.exp.iid <- c(fails.exp.iid, test_exp.iid$fails)
}

fails.exp.iid <- as.data.frame(fails.exp.iid)
fails.exp.sim <- ddply(fails.exp.iid, .(fails.exp.iid), nrow)
fails.exp.sim <- merge(x=fails.exp.sim, y=data.frame(freq = 1:length(freq)), by.x="fails.exp.iid", by.y="freq", all.y=TRUE)
fails.exp.sim[is.na(fails.exp.sim$V1), "V1"] <- 0
fails.exp.sim$prop <- fails.exp.sim$V1/500

qplot(fails.exp.sim$prop)
qplot(data=fails.exp.sim, x=fails.exp.iid, y=prop, geom="line") + 
  geom_hline(aes(yintercept=0.01)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("IID Gaussian(0,1)")


fails.exp.ar1 <- NULL
for(i in 1:500) {
  x.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2))) 
  perio.ar1 <- apply(x.ar1, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
  test_exp.ar1 <- test_exp(x.ar1, perio.ar1, spec_arma(model.ar1, sigma2, freq), 0.01)
  fails.exp.ar1 <- c(fails.exp.ar1, test_exp.ar1$fails)
}

fails.exp.ar1 <- as.data.frame(fails.exp.ar1)
fails.exp.sim.ar1 <- ddply(fails.exp.ar1, .(fails.exp.ar1), nrow)
fails.exp.sim.ar1 <- merge(x=fails.exp.sim.ar1, y=data.frame(freq = 1:length(freq)), by.x="fails.exp.ar1", by.y="freq", all.y=TRUE)
fails.exp.sim.ar1[is.na(fails.exp.sim.ar1$V1), "V1"] <- 0
fails.exp.sim.ar1$prop <- fails.exp.sim.ar1$V1/500

qplot(fails.exp.sim.ar1$prop)
qplot(data=fails.exp.sim.ar1, x=fails.exp.ar1, y=prop, geom="line") + 
  geom_hline(aes(yintercept=0.01)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle(expression(paste("AR(1), ", phi, " = 0.5", sep="")))


fails.exp.ar4 <- NULL
for(i in 1:500) {
  x.ar4 <- sapply(1:M, function(x) arima.sim(model.ar4, n=n, sd=sqrt(sigma2))) 
  perio.ar4 <- apply(x.ar4, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
  test_exp.ar4 <- test_exp(x.ar4, perio.ar4, spec_arma(model.ar4, sigma2, freq), 0.01)
  fails.exp.ar4 <- c(fails.exp.ar4, test_exp.ar4$fails)
}

fails.exp.ar4 <- as.data.frame(fails.exp.ar4)
fails.exp.sim.ar4 <- ddply(fails.exp.ar4, .(fails.exp.ar4), nrow)
fails.exp.sim.ar4 <- merge(x=fails.exp.sim.ar4, y=data.frame(freq = 1:length(freq)), by.x="fails.exp.ar4", by.y="freq", all.y=TRUE)
fails.exp.sim.ar4[is.na(fails.exp.sim.ar4$V1), "V1"] <- 0
fails.exp.sim.ar4$prop <- fails.exp.sim.ar4$V1/500

qplot(fails.exp.sim.ar4$prop)
qplot(data=fails.exp.sim.ar4, x=fails.exp.ar4, y=prop, geom="line") + 
  geom_hline(aes(yintercept=0.01)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle(bquote(paste("AR(4), ", phi, " = [",.(paste(model.ar4$ar, collapse=", ")),"]", sep="")))

