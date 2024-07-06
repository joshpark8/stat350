library(ggplot2)

set.seed(54321)

SRS <- 1500
n <- 250

f_data.vec <- rf(n=SRS*n, df1=1, df2=15)

f_data.mat <- matrix(f_data.vec, nrow = SRS) 

avg <- apply(f_data.mat, MARGIN=1, FUN=mean)
avg_df <- data.frame(avg=avg)

number_bins <- max(round(sqrt(n))+2, 5)

xbar <- mean(avg)
s <- sd(avg)

ggplot(avg_df, aes(x=avg)) + 
  geom_histogram(aes(y= after_stat(density)), bins = number_bins, 
                 fill="grey", col="black") +
  geom_density(col="red", lwd=1) + 
  stat_function(fun=dnorm, args=list(mean=xbar, sd=s), col="blue", lwd=1) +
  xlab("Data") +
  ylab("Proportion") +
  ggtitle("Histogram for F Distribution (n=250)")

ggplot(avg_df, aes(sample=avg)) + 
  stat_qq() +
  geom_abline(slope=s, intercept=xbar) + 
  ggtitle("Normal Probability Plot of F Distribution (n=250)")
