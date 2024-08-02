library(ggplot2)

df <- CreditDataSummer2024_cleaned

df$diff <- df$lamountPred - df$lamount

npred <- sum(complete.cases(df$lamountPred))
nac <- sum(complete.cases(df$lamount))

bins <- max(round(sqrt(n))+2, 5)

dbar <- mean(df$diff)

delta0 <- 0

s <- sd(df$diff)
sac <- sd(df$lamount)
spred <- sd(df$lamountPred)

SE <- sqrt(sigmapred^2/npred+sigmaac^2/nac)

t.test.results <- t.test(df$diff, mu = 0, conf.level = 0.95, alternative = "greater")
t.test.results

ggplot(df, aes(x=diff)) + 
  geom_histogram(aes(y= after_stat(density)), bins = bins, 
                 fill="grey", col="black") +
  geom_density(col="red", lwd=1) + 
  stat_function(fun=dnorm, args=list(mean=dbar, sd=s), col="blue", lwd=1) +
  xlab("Data") +
  ylab("Proportion") +
  ggtitle("Histogram of diff")

ggplot(df, aes(sample=diff)) + 
  stat_qq() +
  geom_abline(slope=s, intercept=dbar) + 
  ggtitle("Normal Probability Plot of diff")

ggplot(df, aes(x = "", y = diff)) + 
  stat_boxplot(geom = "errorbar") + 
  geom_boxplot() + 
  ggtitle("Boxplot of diff") + 
  stat_summary(fun = mean, col = "black", geom = "point", size = 3)

SE <- sqrt(shigh^2/nhigh+slow^2/nlow) 
tts <- (xbarhigh - xbarlow - delta0)/SE
c(xbarhigh-xbarlow-tts*SE, xbarhigh-xbarlow+tts*SE)

SEhigh <- shigh^2/nhigh
SElow <- slow^2/nlow

df <- (SEhigh+SElow)^2/(SEhigh^2/(nhigh-1)+SElow^2/(nlow-1))
