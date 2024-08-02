library(ggplot2)

df <- subset(CreditDataSummer2024_cleaned, CreditDataSummer2024_cleaned$duration > 6 & CreditDataSummer2024_cleaned$duration < 30)

# 1a
fit <- lm(lamount ~ duration, data = df)
sample <- data.frame(duration = 7.25)
predict(fit, sample, interval = 