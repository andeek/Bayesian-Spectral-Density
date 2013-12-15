library(ggplot2)
library(gridExtra)
load("../../../tests.RData")
load("../../../df_list.RData")

g_draws.iid <- qplot(1:n, draws.iid[,1], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.iid <- qplot(freq, perio.iid[,1], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.iid <- qplot(freq, spec.iid, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.iid <- qplot(data=tests.iid$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.iid <- qplot((1:length(tests.iid$prop_fails.ind))*2*pi/n, tests.iid$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")

g_draws.ar1 <- qplot(1:n, draws.ar1[,1], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.ar1 <- qplot(freq, perio.ar1[,1], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.ar1 <- qplot(freq, spec.ar1, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.ar1 <- qplot(data=tests.ar1$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.ar1 <- qplot((1:length(tests.ar1$prop_fails.ind))*2*pi/n, tests.ar1$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")

g_draws.ar4 <- qplot(1:n, draws.ar4[,2], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.ar4 <- qplot(freq, perio.ar4[,2], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.ar4 <- qplot(freq, spec.ar4, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.ar4 <- qplot(data=tests.ar4$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.ar4 <- qplot((1:length(tests.ar4$prop_fails.ind))*2*pi/n, tests.ar4$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")

g_draws.ma1 <- qplot(1:n, draws.ma1[,1], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.ma1 <- qplot(freq, perio.ma1[,1], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.ma1 <- qplot(freq, spec.ma1, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.ma1 <- qplot(data=tests.ma1$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.ma1 <- qplot((1:length(tests.ma1$prop_fails.ind))*2*pi/n, tests.ma1$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")

g_draws.ma2 <- qplot(1:n, draws.ma2[,1], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.ma2 <- qplot(freq, perio.ma2[,1], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.ma2 <- qplot(freq, spec.ma2, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.ma2 <- qplot(data=tests.ma2$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.ma2 <- qplot((1:length(tests.ma2$prop_fails.ind))*2*pi/n, tests.ma2$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")

g_draws.arma41 <- qplot(1:n, draws.arma41[,1], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.arma41 <- qplot(freq, perio.arma41[,1], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.arma41 <- qplot(freq, spec.arma41, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.arma41 <- qplot(data=tests.arma41$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.arma41 <- qplot((1:length(tests.arma41$prop_fails.ind))*2*pi/n, tests.arma41$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")

g_draws.arma42 <- qplot(1:n, draws.arma42[,1], geom="line") +
  xlab("Time") + ylab("Data") +
  ggtitle("One Series of Data")

g_perio.arma42 <- qplot(freq, perio.arma42[,1], geom="line") +
  xlab("Frequency") + ylab(expression(I[n](omega))) +
  ggtitle("Periodogram")

g_spec.arma42 <- qplot(freq, spec.arma42, geom="line") +
  xlab("Frequency") + ylab("Spectrum") +
  ggtitle("Spectral Density")

g_exp.arma42 <- qplot(data=tests.arma42$pro_fails.exp, x=2*pi*es/n, y=prop, geom="line") + 
  geom_hline(aes(yintercept=alpha)) + 
  geom_smooth(method="loess") +
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Distribution")

g_ind.arma42 <- qplot((1:length(tests.arma42$prop_fails.ind))*2*pi/n, tests.arma42$prop_fails.ind, geom="line") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") + 
  xlab("Frequency") + ylab("Proportion of Fails") +
  ggtitle("Tests of Independence")



nfreq=60
df_space1<-list()
for(i in 1:2){
  df_space1[[i]]<-rbind(df.list[[2]][seq(i,nfreq, 1+1),], df.list[[1]][nfreq:nrow(df.list[[1]]),])
}

g_space1a.ar4<-qplot(data=df_space1[[1]], x=2*pi*x/500, xend=2*pi*xe/500, y=y, yend=y, geom="segment", xlim=c(0,pi), ylim=c(0,.2), xlab="Frequency", ylab="Proportion of Fails", main="Tests of Independence") + 
               geom_hline(yintercept=alpha) + geom_smooth(method="loess") #+ ggtitle(paste("Starting j =", i))

g_space1b.ar4<-qplot(data=df_space1[[2]], x=2*pi*x/500, xend=2*pi*xe/500, y=y, yend=y, geom="segment", xlim=c(0,pi), ylim=c(0,.2), xlab="Frequency", ylab="Proportion of Fails", main="Tests of Independence") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") #+ ggtitle(paste("Starting j =", i))

df_space2<-list()
for(i in 1:3){
  df_space2[[i]]<-rbind(df.list[[3]][seq(i,nfreq, 2+1),], df.list[[1]][nfreq:nrow(df.list[[1]]),])
}

g_space2a.ar4<-qplot(data=df_space2[[1]], x=2*pi*x/500, xend=2*pi*xe/500, y=y, yend=y, geom="segment", xlim=c(0,pi), ylim=c(0,.2), xlab="Frequency", ylab="Proportion of Fails", main="Tests of Independence") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") #+ ggtitle(paste("Starting j =", i))

g_space2b.ar4<-qplot(data=df_space2[[2]], x=2*pi*x/500, xend=2*pi*xe/500, y=y, yend=y, geom="segment", xlim=c(0,pi), ylim=c(0,.2), xlab="Frequency", ylab="Proportion of Fails", main="Tests of Independence") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") #+ ggtitle(paste("Starting j =", i))

g_space2c.ar4<-qplot(data=df_space2[[3]], x=2*pi*x/500, xend=2*pi*xe/500, y=y, yend=y, geom="segment", xlim=c(0,pi), ylim=c(0,.2), xlab="Frequency", ylab="Proportion of Fails", main="Tests of Independence") + 
  geom_hline(yintercept=alpha) + geom_smooth(method="loess") #+ ggtitle(paste("Starting j =", i))



