library(ggplot2)

df <- subset(CreditDataSummer2024_cleaned, CreditDataSummer2024_cleaned$duration > 6 & CreditDataSummer2024_cleaned$duration < 30)

# 1a
fit <- lm(lamount ~ duration, data = df)
sample <- data.frame(duration = 7.25)
predict(fit, sample, interval = "prediction")

# 1b
fit <- lm(resids ~ duration, data = df)
df$resids <- fit$residuals
predict(fit, sample, interval = "prediction")

# 2a
fit <- lm(lamount ~ duration, data = df)
sample <- data.frame(duration = 7.25)
predict(fit, sample, interval = "confidence", level = 0.99)

# 3a
fit <- lm(lamount ~ duration, data = df)
sample <- data.frame(duration = 7.25)
predict(fit, sample, interval = "prediction", level = 0.99)
