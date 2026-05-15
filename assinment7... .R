install.packages("ggplot2")

library(ggplot2)

set.seed(42)
n <- 100
data <- data.frame(
  Income = rnorm(n, mean = 52000, sd = 12000),
  SpendingScore = sample(1:100, n, replace = TRUE),
  Age = sample(18:70, n, replace = TRUE),
  Region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  Purchase = sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6))
)

# --- Q1) Retail Income vs Spending Strategy ---
cat("\n--- Q1 Results ---\n")
# 1 & 2. Pearson Correlation and Significance
cor_q1 <- cor.test(data$Income, data$SpendingScore, method = "pearson")
print(cor_q1)
# 3. Normal Distribution Comparison
hist(data$Income, prob = TRUE, main = "Income: Actual vs Normal Curve", col="lightblue")
curve(dnorm(x, mean(data$Income), sd(data$Income)), add = TRUE, col = "red", lwd = 2)
# 4. One-sample t-test (Income = 50,000)
t.test(data$Income, mu = 50000)
# 5. Binomial test (Success rate 50%)
binom.test(sum(data$Purchase), n, p = 0.5)

# --- Q2) Customer Segmentation Strategy ---
cat("\n--- Q2 Results ---\n")
# 1. Correlation Matrix
print(cor(data[, c("Age", "Income", "SpendingScore")]))
# 4. T-test (SpendingScore = 60)
t.test(data$SpendingScore, mu = 60)
# 5. Kruskal-Wallis (SpendingScore vs Region)
kruskal.test(SpendingScore ~ Region, data = data)

# --- Q3) Financial Risk Assessment ---
cat("\n--- Q3 Results ---\n")
# 1 & 2. Correlation Income/Spending
print(cor.test(data$Income, data$SpendingScore))
# 3. Poisson Distribution (Risky Customers)
risky_sim <- rpois(100, lambda = 5)
# 4. T-test (Income = 40,000)
t.test(data$Income, mu = 40000)
# 5. Binomial test (60% safe)
binom.test(sum(data$Purchase == 1), n, p = 0.6)

# --- Q4) Product Demand Forecasting ---
cat("\n--- Q4 Results ---\n")
# 1. Spearman Correlation (Age/Income)
print(cor.test(data$Age, data$Income, method = "spearman"))
# 3. Binomial Distribution (Purchase events)
purchase_sim <- rbinom(10, size = 1, prob = 0.6)
# 4. T-test (Age = 35)
t.test(data$Age, mu = 35)
# 5. Chi-square (Region distribution)
chisq.test(table(data$Region))

# --- Q5) Customer Satisfaction Modeling ---
cat("\n--- Q5 Results ---\n")
# 1 & 2. Pearson vs Spearman
print(cor(data$Income, data$SpendingScore, method = "pearson"))
print(cor(data$Income, data$SpendingScore, method = "spearman"))
# 4. T-test (Satisfaction = 55)
t.test(data$SpendingScore, mu = 55)
# 5. Wilcoxon Test (Age vs SpendingScore)
wilcox.test(data$Age, data$SpendingScore)

# --- Q6) Sales Probability Analysis ---
cat("\n--- Q6 Results ---\n")
# 1. Correlation (Binary Purchase vs Income)
print(cor(data$Purchase, data$Income))
# 3. Binomial test
binom.test(sum(data$Purchase), n)
# 4. T-test (Income = 55,000)
t.test(data$Income, mu = 55000)
# 5. Chi-square (Purchase distribution)
chisq.test(table(data$Purchase))

# --- Q7) Strategic Business Decision Model ---
cat("\n--- Q7 Results ---\n")
# 1. Correlation Matrix
print(cor(data[sapply(data, is.numeric)]))
# 3. Poisson Distribution (Simulated Demand)
print(head(rpois(100, lambda = 10)))
# 4. T-test (Income = 60,000)
t.test(data$Income, mu = 60000)
# 5. Kruskal-Wallis (Age across Regions)
kruskal.test(Age ~ Region, data = data)