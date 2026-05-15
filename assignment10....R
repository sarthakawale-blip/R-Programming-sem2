install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# 1. Import the dataset
# Assuming the Kaggle dataset 'customer_data.csv' is in your directory
# For this project, we generate a synthetic version to ensure code execution
set.seed(123)
n <- 200
data <- data.frame(
  CustomerID = 1:n,
  Gender = sample(c("Male", "Female"), n, replace = TRUE),
  Age = sample(18:70, n, replace = TRUE),
  Annual_Income = rnorm(n, mean = 60000, sd = 15000),
  Spending_Score = sample(1:100, n, replace = TRUE),
  Region = sample(c("North", "South", "East", "West"), n, replace = TRUE)
)
# Check for missing values
sum(is.na(data))

# Data Preprocessing
data$Gender <- as.factor(data$Gender)
data$Region <- as.factor(data$Region)

# Remove redundant columns
clean_data <- data %>% select(-CustomerID)

# Statistical Summary
summary(clean_data)

# Graphical Analysis: Age Distribution
ggplot(clean_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "navy", color = "white") +
  labs(title = "Age Distribution of Customers", x = "Age", y = "Count")

# Correlation between Income and Spending Score
cor_val <- cor(clean_data$Annual_Income, clean_data$Spending_Score)
print(paste("Pearson Correlation:", round(cor_val, 4)))

# Hypothesis Testing: Is average income = 55,000?
t_test_result <- t.test(clean_data$Annual_Income, mu = 55000)
print(t_test_result)

# Spending Score by Gender
ggplot(clean_data, aes(x = Gender, y = Spending_Score, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Spending Score Distribution by Gender", y = "Spending Score")

# Income vs Spending Score Scatter Plot
ggplot(clean_data, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation: Annual Income vs. Spending Score")

