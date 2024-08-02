library(ggplot2)

df <- subset(CreditDataSummer2024_cleaned, CreditDataSummer2024_cleaned$duration > 6 & CreditDataSummer2024_cleaned$duration < 30)

# 1a
ggplot(df, aes(x = duration, y = creditAmountDollar)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("Scatter Plot of duration vs. creditAmountDollar") +
  xlab("duration") +
  ylab("creditAmountDollar")

# 1b
ggplot(df, aes(x = duration, y = lamount)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("Scatter Plot of duration vs. lamount") +
  xlab("duration") +
  ylab("lamount")

# 1c
df$lduration <- log(df$duration)
ggplot(df, aes(x = lduration, y = lamount)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("Scatter Plot of lduration vs. lamount") +
  xlab("lduration") +
  ylab("lamount")

# 3a
cor(df$lduration, df$lamount, use = "everything", method = c("pearson", "kendall", "spearman"))

# 5a 
fit <- lm(lamount ~ lduration, data = df)
summary(fit)

# 6a 
anova(fit)