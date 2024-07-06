library(ggplot2)

set.seed(54321)
RandomData <- rcauchy(n=280, location=0, scale=1)
RandomDataDF <- data.frame(RandomData=RandomData)

number_bins <- max(round(sqrt(n))+2,5)

xbar <- mean(log(CreditDataSummer2024_cleaned$duration))
s <- sd(log(CreditDataSummer2024_cleaned$duration))

logdf <- data.frame(log=log(CreditDataSummer2024_cleaned$duration))

ggplot(CreditDataSummer2024_cleaned, aes(x=duration)) + 
  geom_histogram(aes(y= after_stat(density)), bins = number_bins, 
                 fill="grey", col="black") +
  geom_density(col="red", lwd=1) + 
  stat_function(fun=dnorm, args=list(mean=xbar, sd=s), col="blue", lwd=1) +
  xlab("Data") +
  ylab("Proportion") +
  ggtitle("Histogram of Duration")

ggplot(logdf, aes(sample=log)) + 
  stat_qq() +
  geom_abline(slope=s, intercept=xbar) + 
  ggtitle("normal Probability Plot of Duration")
