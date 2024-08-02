library(ggplot2)

df <- subset(CreditDataSummer2024_cleaned, CreditDataSummer2024_cleaned$duration > 6 & CreditDataSummer2024_cleaned$duration < 30)

fit <- lm(lamount ~ lduration, data = df)
df$resids <- fit$residuals

# 1a
ggplot(df, aes(x = lduration, y = resids)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "solid", color = "blue") +
  ggtitle("Residual Plot vs. Explanatory Variable") +
  xlab("Explanatory Variable (lduration)") +
  ylab("Residuals")

# 2a
n_bins <- round(max(sqrt(nrow(df))+2, 5))
xbar.resids <- mean(df$resids) 
s.resids <- sd(df$resids)

ggplot(df, aes(x = resids)) +
  geom_histogram(aes(y = after_stat(density)), bins = n_bins, fill = "grey", col = "black") + 
  geom_density(col = "red", lwd = 1) + 
  stat_function(fun = dnorm, args = list(mean = xbar.resids, sd = s.resids), col="blue", lwd = 1) + 
  ggtitle("Histogram of Residuals")

ggplot(df, aes(sample=resids)) + 
  stat_qq() + 
  geom_abline(slope = s.resids, intercept = xbar.resids) + 
  ggtitle("QQ Plot of Residuals")

# 4a
summary(fit)

# 5a
confint(fit, level = 0.99)
