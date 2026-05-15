library(methods) # Required for S4 classes

# --- Dataset Generation (Simulating oop_customer_data.xlsx) ---
set.seed(789)
data <- data.frame(
  CustomerID = 1:5,
  Age = c(25, 34, 45, 23, 56),
  Income = c(50000, 65000, 80000, 42000, 95000),
  Membership = c("Gold", "Silver", "Platinum", "Gold", "Silver"),
  SpendingScore = c(60, 70, 85, 40, 90),
  stringsAsFactors = FALSE
)

# --- Q1) Customer Object Modeling (S3 System) ---
cat("\n--- Q1: S3 System ---\n")
# 1 & 2. Create S3 Object
cust_s3 <- list(Age = data$Age[1], Income = data$Income[1], Membership = data$Membership[1])
class(cust_s3) <- "Customer"

# 3. Custom print method
print.Customer <- function(obj) {
  cat("S3 Customer Object\n")
  cat("Age:", obj$Age, "| Income:", obj$Income, "| Level:", obj$Membership, "\n")
}
print(cust_s3) # 4. Display
cust_s3$Age <- 26 # 5. Modify
print(cust_s3)

# --- Q2) Advanced Customer Class (S4 System) ---
cat("\n--- Q2: S4 System ---\n")
# 1. Define S4 Class
setClass("CustomerS4", slots = list(Age = "numeric", Income = "numeric", Membership = "character"))

# 2. Create Object
cust_s4 <- new("CustomerS4", Age = data$Age[2], Income = data$Income[2], Membership = data$Membership[2])

# 3. Write show method
setMethod("show", "CustomerS4", function(object) {
  cat("S4 Customer Record\n")
  cat("Age:", object@Age, "\nIncome:", object@Income, "\nMember:", object@Membership, "\n")
})
show(cust_s4)

# --- Q3) Object Referencing and Copy Behavior ---
cat("\n--- Q3: Copy Behavior ---\n")
data_copy <- data # 1. Assign
data_copy$Age[1] <- 99 # 2. Modify
cat("Original Age:", data$Age[1], " | Copy Age:", data_copy$Age[1], "\n") # 3. Check
cat("Are they identical?", identical(data, data_copy), "\n") # 4. Compare

# --- Q4) Memory Optimization Study ---
cat("\n--- Q4: Memory Management ---\n")
large_obj <- replicate(100, data, simplify = FALSE) # 1. Create large object
print(object.size(large_obj)) # 2. Check size
rm(large_obj) # 3. Remove
gc() # 4. Garbage collection

# --- Q5) List-Based Customer Object System ---
cat("\n--- Q5: List-Based OOP ---\n")
customer_list <- split(data, seq(nrow(data))) # 1. Convert to list
customer_list <- lapply(customer_list, function(x) { class(x) <- "Customer"; x }) # 2. Assign class
print(customer_list[[1]]) # 3. Access
customer_list[[1]]$Income <- 55000 # 4. Modify
str(customer_list[[1]]) # 5. Print structure

# --- Q6) Class Inspection ---
cat("\n--- Q6: Inspection ---\n")
class(data) # 1. Class
str(data) # 2. Structure
attributes(data) # 3. Attributes
summary(data) # 4. Overview

# --- Q7) S4 Slot Manipulation ---
cat("\n--- Q7: S4 Slots ---\n")
setClass("SpendingProfile", slots = list(SpendingScore = "numeric", Income = "numeric")) # 1 & 2
profile <- new("SpendingProfile", SpendingScore = data$SpendingScore[1], Income = data$Income[1]) # 3
cat("Initial Score:", profile@SpendingScore, "\n") # 4. Access @
profile@SpendingScore <- 95 # 5. Modify
validObject(profile)

# --- Q8) Advanced Data Structure Integration ---
cat("\n--- Q8: Nested Structures ---\n")
nested_system <- list(RawData = data, Stats = summary(data)) # 1. Nested list
nested_system$RawData$Age[1] # 2. Access
nested_system$NewElement <- "Updated" # 3. Modify
class(nested_system$Stats) <- "SummaryReport" # 4. Apply class
str(nested_system) # 5. Structure

# --- Q9) Object-Oriented Data Analysis ---
cat("\n--- Q9: S3 Analysis System ---\n")
class(data) <- "SalesData" # 1. Convert to S3
calc_avg_income <- function(obj) UseMethod("calc_avg_income") # 2. Generic
calc_avg_income.SalesData <- function(obj) mean(obj$Income) # Method
cat("Avg Income:", calc_avg_income(data), "\n") # 3. Apply
attr(data, "Analyst") <- "Sandesh" # 4. Extend attribute
print(attributes(data)$Analyst)

# --- Q10) Enterprise-Level Design ---
cat("\n--- Q10: S3 vs S4 Comparison ---\n")
# S3 is informal (list with a class tag), S4 is formal (defined slots and types).
# S4 allows for strict validation (validObject), making it better for banking/enterprise.
cat("S3 is dispatch-based and easy to extend.\n")
cat("S4 is structure-based and ensures data integrity.\n")