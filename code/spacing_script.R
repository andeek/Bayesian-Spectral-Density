source("/code/udf_20131202.R")

## Ar(4) where independence fails ##
phi <- c(.08, .33, .1, .45) ##c(0.07794297, 0.32976393, 0.10472191, 0.45397725)
M <- 1000 #number of draws
n <- 500 #length of ts
sigma2 <- 1

freq <- fourier_freq(n)
model.ar4 <- list(ar=phi, ma=c())

## Slightly different than the one you (Andee) adapted, 'nfreq' is how far 
## away from 0 you want to try spacing out
sim_ind_tests<-function(model, freq, nfreq=length(freq), space=0, s=200, M=1000, alpha=0.05, sigma2=1){
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

## Need first run to be just neighbors, if you have this stored you can just use that
ps<-sim_ind_tests(model.ar4, freq, s=200, M=1000) 

## Data frame of proportions and test indices (1,2), (2,3), etc. for neighbors
df<-data.frame(x=seq(1,length(ps),1), xe=seq(2, length(ps)+1,1), y=jitter(ps, factor=5))

## For AR(4) I think 1:60ish is enough to consider (i.e. nfreq=60)
df.list<-list()
df.list[[1]]<-df

## Construct list of dataframes of proportions with test indices for spacings
p<-10 #Equal to nfreq if doing all possible spacings, otherwise is equal to (i-1) spacings
for(i in 2:p){
  ps1<-sim_ind_tests(model.ar4, freq, nfreq=60, space=i-1, s=200, M=1000) 
  df.list[[i]]<-data.frame(x=seq(1,length(ps1),1), xe=seq(1+i, length(ps1)+i,1), y=jitter(ps1, factor=5))
  cat(i, "\n")
}

### Function to plot all possible choice of frequencies for each spacing
## df_full: full sequence of neighbor test proportions
## df: one dataframe from one set of spacings (i.e. element of df.list in above)
## space matches the spacing of df
seg_plot<-function(df, nfreq, space, alpha=0.05, xlims=c(0,250), ylims=c(0,1), df_full){
  require(gridExtra)
  df_space<-list()
  for(i in 1:(space+1)){
    df_space[[i]]<-rbind(df[seq(i,nfreq, space+1),], df_full[nfreq:nrow(df_full),])
  }
  df_p<-lapply(df_space, function(df) qplot(data=df, x=x, xend=xe, y=y, yend=y, geom="segment", xlim=xlims, ylim=ylims, xlab="Frequency", ylab="Proportion of Fails", main="Tests of Independence") + 
                 geom_hline(yintercept=alpha) + geom_smooth(method="loess")) #+ ggtitle(paste("Starting j =", i))
  p<-do.call(grid.arrange, c(df_p, ncol=1))
  return(p)
}

## Examples ##
seg_plot(df.list[[2]], nfreq=60, space=1, xlims=c(0,250), ylims=c(0,.3), df_full=df.list[[1]])
seg_plot(df.list[[3]], nfreq=60, space=2, xlims=c(0,250), ylims=c(0,.3), df_full=df.list[[1]])
seg_plot(df.list[[4]], nfreq=60, space=3, xlims=c(0,250), ylims=c(0,.3), df_full=df.list[[1]])

## Data to save is df.list




