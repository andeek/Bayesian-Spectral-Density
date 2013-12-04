library(ggplot2)
source("code/udf_20131202.R")

set.seed(1050)

##Simulate Draws
M <- 1000 #number of draws
n <- 500 #length of ts
phi <- runif(1,0,1) #causal AR
sigma2 <- 1

freq <- fourier_freq(n)

model.ar1 <- list(ar=phi) 
draws.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))
spect.ar1 <- spec_ar1(phi, sigma2, freq)


perio <- apply(draws.ar1, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
expon <- t(sapply(spect.ar1, function(x) rexp(M, rate=1/(2*pi*x))))

perio.prod <- apply(perio, 2, prod)
expon.prod <- apply(expon, 2, prod)
quantiles.prod <- data.frame(perio = quantile(perio.prod, prob=seq(0,1,by=.001)),
                             expon = quantile(expon.prod, prob=seq(0,1,by=.001)))

quantiles.graph <- rbind(quantiles.prod, quantiles.prod, quantiles.prod)
quantiles.graph$zoom1 <- c(rep(0, nrow(quantiles.prod)),
                          rep(0, nrow(quantiles.prod)),
                          rep(.5, nrow(quantiles.prod)))

quantiles.graph$zoom2 <- c(rep(1, nrow(quantiles.prod)),
                           rep(.5, nrow(quantiles.prod)),
                           rep(1, nrow(quantiles.prod)))

ggplot(data=quantiles.prod) + geom_point(aes(x=perio, y=expon)) + 


test <- sapply(1:length(freq), function(x) ks.test(perio[x,], expon[x,])$p.value)
means <- 2*pi*spect.ar1
ggplot() + geom_histogram(aes(x=perio[42,], y=..density..)) + geom_line(aes(x=seq(0,50,by=.1), y=dexp(seq(0,50,by=.1), 1/means[42])), inherit.aes=FALSE)


test <- function(data, freq, start_freq, spec_func) {
  perio <- apply(data, 2, function(x) periodogram(x, freq[freq > start_freq], center=TRUE, plot=FALSE)$spec)
  spect <- do.call(spec_func, args=list(phi, sigma2, freq[freq > start_freq]))
  expon <- t(sapply(spect, function(x) rexp(M, rate=1/(x*2*pi)))) #independent exponential draws
  
  perio.prod <- apply(perio, 2, prod)
  expon.prod <- apply(expon, 2, prod)
  
  zoom <- max(c(perio.prod,expon.prod))
  ks <- ks.test(perio.prod, expon.prod)
  g <- qplot(quantile(perio.prod, prob=seq(0,1,by=.01)), quantile(expon.prod, prob=seq(0,1,by=.01))) + geom_abline() + coord_fixed(xlim=c(0,1e-70), ylim=c(0,1e-70)) +
          xlab("Periodogram") + ylab("Independent Exponentials") + ggtitle("Quantile Plot")
  return(list(test=ks, graph=g))
}

test.ar1.full <- test(draws.ar1, freq, 0, spec_ar1)
test.ar1.half <- test(draws.ar1, freq, pi/2, spec_ar1)
test.ar1.quarter <- test(draws.ar1, freq, pi/4, spec_ar1)


