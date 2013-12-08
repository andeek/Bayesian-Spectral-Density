##Helper functions for simulation/estimation of necessary spacing
##12/2/2013

periodogram <- function(y, freq, center=TRUE, plot=TRUE) {
  require(ggplot2)
  
  if(center) y <- y - mean(y)
  
  n <- length(y)
  spec <- sapply(freq, function(x) 1/n*((sum(y*cos(x*(1:n))))^2 + (sum(y*sin(x*(1:n))))^2)) 
  df <- data.frame(freq = freq, spec= spec)
  
  if(plot) print(ggplot(df, aes(x=freq, y=spec)) + geom_line() + geom_point(size=2, shape=1) + xlab("Frequency") + ylab("") + ggtitle("Periodogram"))
  
  return(df)
}

fourier_freq <- function(n) {
  2*pi/n*(1:floor((n-1)/2))
}

spec_ar1 <- function(phi, sigma2, w) {
  sigma2/(2*pi)*(1-2*phi*cos(w) + phi^2)^(-1)
}

spec_iidgaus <- function(sigma2){
  sigma2/(2*pi)
}

spec_arma <- function(model, sigma2, w) {
  require(polynom)
  
  stopifnot(is.list(model))

  if (length(model$ar)) {
    minroots <- min(Mod(polyroot(c(1, -model$ar))))
    if (minroots <= 1) 
      stop("'ar' part of model is not stationary")
  }  
  
  phi <- model$ar
  theta <- model$ma
  
  if(length(phi)) phi <- -phi
  
  ar.poly <- as.function(polynomial(coef=c(1,phi)))
  ma.poly <- as.function(polynomial(coef=c(1,theta)))
  
  ar.poly(complex(real=cos(w), imaginary=sin(w)))
  ma.poly(complex(real=cos(w), imaginary=sin(w)))
  
  sigma2/(2*pi)*(Mod(ma.poly(complex(real=cos(w), imaginary=sin(w)))))^2/(Mod(ar.poly(complex(real=cos(w), imaginary=sin(w)))))^2
}

test_exp <- function(data, freq, spec, alpha) {
  perio <- apply(data, 2, function(x) periodogram(x, freq, center=TRUE, plot=FALSE)$spec)
  ks <- apply(perio, 1, function(x) ks.test(x, "pexp", 2*pi*spec)$p.value)            
  return(list(pvals=ks, fails=which(ks < alpha)))
}

