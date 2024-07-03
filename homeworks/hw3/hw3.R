library(ggplot2)

set.seed(54321)
n <- 900
RandomData <- rnorm(n, mean=-10, sd=0.25)
RandomDataDF <- data.frame(RandomData=RandomData)

xbar <- mean(RandomData)
s <- sd(RandomData)

number_bins <- max(round(sqrt(n))+2,5)

ggplot(RandomDataDF, aes(x=RandomData)) + 
  geom_histogram(aes(y= after_stat(density)), bins = number_bins, 
                 fill="grey", col="black") +
  geom_density(col="red", lwd=1) + 
  stat_function(fun=dnorm, args=list(mean=xbar, sd=s), col="blue", lwd=1) +
  xlab("Data") +
  ylab("Proportion") +
  ggtitle("Histogram of RandomData")

# p2

ggplot(RandomDataDF, aes(sample=RandomData)) + 
  stat_qq() +
  geom_abline(slope=s, intercept=xbar) + 
  ggtitle("normal Proability Plot of RandomData")