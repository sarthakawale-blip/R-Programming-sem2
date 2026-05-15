set.seed(42)
data <- data.frame(
  Region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Purchase = sample(c("Yes", "No"), 100, replace = TRUE),
  Amount = sample(1000:8000, 100, replace = TRUE)
)

# --- Q1) Sales Performance Analysis ---
# a) Total sales by region
q1a <- aggregate(Amount ~ Region, data = data, sum)
# b) Average sales per gender
q1b <- aggregate(Amount ~ Gender, data = data, mean)
# c) Frequency distribution of purchase
q1c <- table(data$Purchase)
# d) Cross-tab: Region vs Purchase
q1d <- table(data$Region, data$Purchase)
# e) Chi-square test
q1e <-传播chi2 <- chisq.test(q1d)

# --- Q2) Customer Segmentation ---
# a) Count customers by region
q2a <- table(data$Region)
# b) Average purchase by region using tapply
q2b <- tapply(data$Amount, data$Region, mean)
# c) Frequency of gender
q2c <- table(data$Gender)
# d) Cross-tab: Gender vs Region
q2d <- table(data$Gender, data$Region)
# e) Chi-square test
q2e <- chisq.test(q2d)

# --- Q3) Purchase Pattern Analysis ---
# a) Total purchase amount
q3a <- sum(data$Amount)
# b) Row-wise sum using apply (summing the 'Amount' column per row)
q3b <- apply(data[, "Amount", drop=F], 1, sum)
# c) Frequency of purchase
q3c <- table(data$Purchase)
# d) Cross-tab: Purchase vs Region
q3d <- table(data$Purchase, data$Region)
# e) Chi-square test
q3e <- chisq.test(q3d)

# --- Q4) High-Value Customer Analysis ---
# a) Filter customers with Amount > 5000
high_value <- subset(data, Amount > 5000)
# b) Mean purchase of filtered group
q4b <- mean(high_value$Amount)
# c) Frequency of region (filtered)
q4c <- table(high_value$Region)
# d) Cross-tab: Gender vs Purchase (filtered)
q4d <- table(high_value$Gender, high_value$Purchase)
# e) Chi-square test
q4e <- chisq.test(q4d)

# --- Q5) Business Decision Analysis ---
# a) Total revenue
q5a <- sum(data$Amount)
# b) Average revenue per region
q5b <- aggregate(Amount ~ Region, data = data, mean)
# c) Frequency distribution of regions
q5c <- table(data$Region)
# d) Cross-tab: Region vs Purchase
q5d <- table(data$Region, data$Purchase)

# --- Print Results (Example) ---
print("Total Sales by Region:")
print(q1a)
print("Chi-square Test (Region vs Purchase):")
print(q1e)