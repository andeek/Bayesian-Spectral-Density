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
  2*pi/n*(1:floor(n/2))
}

spec_ar1 <- function(phi, sigma2, w) {
  sigma2/(2*pi)*(1-2*phi*cos(w) + phi^2)^(-1)
}