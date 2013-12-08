source("code/udf_20131202.R")

M <- 1000 #number of draws
n <- 500 #length of ts
sigma2 <- 1

freq <- fourier_freq(n)

model.iid <- list()
model.ar1 <- list(ar=0.5)

x.iid <- sapply(1:M, function(x) arima.sim(model.iid, n=n, sd=sqrt(sigma2)))
x.ar1 <- sapply(1:M, function(x) arima.sim(model.ar1, n=n, sd=sqrt(sigma2)))

test_exp.iid <- test_exp(x.iid, freq, spec_arma(model.iid, sigma2, freq), 0.01)
test_exp.ar1 <- test_exp(x.ar1, freq, spec_arma(model.ar1, sigma2, freq), 0.01)

length(test_exp.iid$fails)
length(test_exp.ar1$fails)

test_indep <- function(data) {
  
}

test_ind_evenly <- function(perio, freq, space, alpha) {
  if(space > length(freq) - 2) stop("space between Fourier frequencies is too large")
  pairs<-data.frame(x=seq(1,(length(freq)-space-1),1), y=seq(1+(space+1), length(freq),1))
  pvals<-apply(pairs, 1, function(x) cor.test(perio[x[1],], perio[x[2],], method="spearman")$p.value)
  return(list(pvals=pvals, freq_pairs=pairs, pairs_fails=pairs[which(pvals < alpha),], pval_fails=pvals[pvals < alpha], prop_fails=sum(pvals < alpha)/nrow(pairs)))
}

neighbors <- test_ind_evenly(perio.gaus, freq, 0, 0.01)
one_sep <- test_ind_evenly(perio.gaus, freq, 1, 0.01)
two_sep <- test_ind_evenly(perio.gaus, freq, 2, 0.01)

fails <- NULL
for(i in 1:(length(freq)-2)){
  fs <- test_ind_evenly(perio.gaus, freq, i, 0.01)$pval_fails
  fails <- c(fails,fs)
}


