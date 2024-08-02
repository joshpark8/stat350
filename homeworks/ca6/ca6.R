library(ggplot2)

newdf <- subset(CreditDataSummer2024_cleaned, CreditDataSummer2024_cleaned$purpose != "tv")

n <- sum(complete.cases(newdf$lamount))

number_bins <- max(round(sqrt(n))+2, 5)

xbar <- mean(newdf$lamount)
s <- sd(newdf$lamount)

ggplot(newdf, aes(x=lamount)) + 
  geom_histogram(aes(y= after_stat(density)), bins = number_bins, 
                 fill="grey", col="black") +
  geom_density(col="red", lwd=1) + 
  stat_function(fun=dnorm, args=list(mean=xbar, sd=s), col="blue", lwd=1) +
  xlab("Data") +
  ylab("Proportion") +
  ggtitle("Histogram of lamount")

ggplot(newdf, aes(sample=lamount)) + 
  stat_qq() +
  geom_abline(slope=s, intercept=xbar) + 
  ggtitle("Normal Probability Plot of lamount")

ggplot(newdf, aes(x = "", y = lamount)) + 
  stat_boxplot(geom = "errorbar") + 
  geom_boxplot() + 
  ggtitle("Boxplot of lamount") + 
  stat_summary(fun = mean, col = "black", geom = "point", size = 3)

t.test(newdf$lamount, mu = 7.00, alternative = "greater", conf.level = 0.95)

C <- 0.95
alpha <- 1-C #One-sided test dont need to divide by 2.
mu_0 <- 7
n <- nrow(newdf)
xbar <- mean(newdf$lamount)
s <- sd(newdf$lamount)
t_TS <- (xbar-mu_0)/(s/sqrt(n)) #Obtain test statistic
pvalue <- pt(q = t_TS, df = n-1, lower.tail = FALSE) #Obtain p-value
pvalue