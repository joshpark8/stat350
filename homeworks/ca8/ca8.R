library(ggplot2)

df <- CreditDataSummer2024_cleaned

df$propertyMagnitude <- factor(df$propertyMagnitude)

car <- subset(df, df$propertyMagnitude == "car")
life <- subset(df, df$propertyMagnitude == "life_insurance")
none <- subset(df, df$propertyMagnitude == "no_known_property")
house <- subset(df, df$propertyMagnitude == "real_estate")

# effects plot
ggplot(data = df, aes(x = propertyMagnitude, y = lamount)) +
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group = 1)) +
  ggtitle("Effects Plot of lamount by propertyMagnitude")

# boxplot
ggplot(df, aes(x = propertyMagnitude, y = lamount)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar") +
  stat_summary(fun = mean, colour = "black", geom = "point", size = 3) +
  ggtitle("Boxplots of lamount by propertyMagnitude")

# summary table
n <- tapply(df$lamount, df$propertyMagnitude, length)
xbar <- tapply(df$lamount, df$propertyMagnitude, mean)
s <- tapply(df$lamount, df$propertyMagnitude, sd)

df$normal.density <- apply(df, 1, function(x){
  dnorm(as.numeric(x["lamount"]), 
        xbar[x["propertyMagnitude"]], s[x["propertyMagnitude"]])})

summary_table <- data.frame(
  GroupName = names(n),
  SampleSize = n,
  Mean = xbar,
  StandardDeviation = s
)
cat("Summary Statistics by propertyMagnitude\n")
print(summary_table, row.names = FALSE)

# histograms
n_bins <- round(max(sqrt(max(n) + 2),5))

ggplot(df, aes(x = lamount)) +
  geom_histogram(aes(y = after_stat(density)), bins = n_bins, fill = "grey", col = "black") +
  facet_grid(propertyMagnitude ~ .) +
  geom_density(col = "red", lwd = 1) +
  geom_line(aes(y = normal.density), col = "blue", lwd = 1) +
  ggtitle("Histograms of lamount by propertyMagnitude")

# qq plot
df$intercept <- apply(df, 1, function(x){xbar[x["propertyMagnitude"]]})
df$slope <- apply(df, 1, function(x){s[x["propertyMagnitude"]]})

ggplot(df, aes(sample = lamount)) +
  stat_qq() +
  facet_grid(propertyMagnitude ~ .) +
  geom_abline(data = df, aes(intercept = intercept, slope = slope)) +
  ggtitle("QQ Plots of lamount by propertyMagnitude")

# ANOVA
fit <- aov(lamount~propertyMagnitude, data = df)
summary(fit)

# Tukey
tukey_result <- TukeyHSD(fit, conf.level = 0.99)
tukey_result
plot(tukey_result)

library(DescTools)
DunnettTest(x=data$value, g=data$Group)