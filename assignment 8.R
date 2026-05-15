install.packages("dplyr")

library(dplyr)

# --- DATASET GENERATION (Run this to simulate your files) ---
set.seed(456)
sales <- data.frame(
  ProductID = 101:120,
  Category = sample(c("Electronics", "Clothing", "Home"), 20, replace = TRUE),
  Price = runif(20, 10, 500),
  Quantity = sample(10:100, 20, replace = TRUE)
) %>% mutate(Revenue = Price * Quantity)

customers <- data.frame(
  CustomerID = 1:50,
  Gender = sample(c("Male", "Female"), 50, replace = TRUE),
  Age = sample(18:80, 50, replace = TRUE),
  PurchaseAmount = runif(50, 100, 5000)
)

transactions <- data.frame(
  TransactionID = 1:30,
  Type = sample(c("Deposit", "Withdrawal"), 30, replace = TRUE),
  Amount = runif(30, 500, 20000)
)

patients <- data.frame(
  PatientID = 1:20,
  Age = sample(20:85, 20, replace = TRUE),
  BP = sample(110:160, 20, replace = TRUE),
  Temp = runif(20, 36.0, 39.5)
)

students <- data.frame(
  StudentID = 1:30,
  Subject = sample(c("Math", "Stats", "DS"), 30, replace = TRUE),
  Marks = sample(30:100, 30, replace = TRUE)
)

posts <- data.frame(
  PostID = 1:20,
  Likes = sample(50:1000, 20, replace = TRUE),
  Comments = sample(10:200, 20, replace = TRUE)
) %>% mutate(TotalEngagement = Likes + Comments)


# --- Q1) Retail Sales Analysis ---
cat("\n--- Q1: Sales Analysis ---\n")
print(head(sales, 10)) # a
q1b <- sales %>% group_by(ProductID) %>% summarise(TotalRev = sum(Revenue)) # b
q1c <- sales %>% arrange(desc(Revenue)) %>% head(5) # c
q1d <- filter(sales, Quantity > 50) # d
q1e <- sales %>% group_by(Category) %>% summarise(AvgPrice = mean(Price)) # e
print(q1c)

# --- Q2) E-Commerce Customer Analysis ---
cat("\n--- Q2: Customer Analysis ---\n")
print(head(customers, 10)) # a
avg_pur <- mean(customers$PurchaseAmount) # b
high_spenders <- filter(customers, PurchaseAmount > avg_pur) # c
table(customers$Gender) # d
customers$AgeGroup <- cut(customers$Age, breaks=c(0, 25, 50, Inf), labels=c("Youth", "Adult", "Senior")) # e
q2f <- customers %>% arrange(desc(PurchaseAmount)) %>% head(10) # f
print(q2f)

# --- Q3) Banking Transaction Analysis ---
cat("\n--- Q3: Transaction Analysis ---\n")
print(head(transactions, 10)) # a
sum(transactions$Amount[transactions$Type == "Deposit"]) # b
sum(transactions$Amount[transactions$Type == "Withdrawal"]) # c
high_trans <- filter(transactions, Amount > 10000) # d
mean(transactions$Amount) # e
table(transactions$Type) # f

# --- Q4) Healthcare Patient Dataset ---
cat("\n--- Q4: Healthcare Analysis ---\n")
print(head(patients, 10)) # a
filter(patients, BP > 140) # b
filter(patients, Temp > 37) # c
mean(patients$Age) # d
range(patients$BP) # e
nrow(filter(patients, Age > 60)) # f

# --- Q5) Student Performance Dataset ---
cat("\n--- Q5: Student Analysis ---\n")
print(head(students, 10)) # a
filter(students, Marks > 80) # b
students %>% group_by(Subject) %>% summarise(AvgMarks = mean(Marks)) # c
students[which.max(students$Marks), ] # d
filter(students, Marks < 40) # e
table(students$Subject) # f

# --- Q6) Social Media Analytics Dataset ---
cat("\n--- Q6: Social Media Analysis ---\n")
print(head(posts, 10)) # a
# b is handled in generation (TotalEngagement)
filter(posts, TotalEngagement > 500) # c
posts[which.max(posts$Likes), ] # d
mean(posts$TotalEngagement) # e
filter(posts, TotalEngagement < 100) # f