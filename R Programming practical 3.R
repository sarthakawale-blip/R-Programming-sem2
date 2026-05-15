#PRACTICAL NO.3: Advanced Data Manipulation Techniques in R Recoding Variables

#Questions:

#Use following data frame to solve following questions

set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),200,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),200,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),200,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),200,replace=TRUE),
  Quantity = sample(1:10,200,replace=TRUE),
  Price = sample(seq(500,50000,500),200,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),200,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),200,replace=TRUE)
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount/100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount


#Q1) Problems on Recoding Variables, Sorting and New Variable Creation 
#a) Create OrderSize (Small, Medium, Large)

# Create OrderSize column

# Rule:
# Quantity 1 to 3  = Small
# Quantity 4 to 7  = Medium
# Quantity 8 to 10 = Large

sales_data$OrderSize <-
  ifelse(sales_data$Quantity <= 3,
         "Small",
         
         ifelse(sales_data$Quantity <= 7,
                "Medium",
                "Large"))

#b) Sort by Quantity descending

# Sort Quantity from biggest to smallest

sorted_quantity <-
  sales_data[order(-sales_data$Quantity), ]

#c) Sort by City and Revenue

# First sort by City
# Then sort Revenue inside each city

sorted_city_revenue <-
  sales_data[order(sales_data$City,
                   sales_data$Revenue), ]


#d) Create Tax column

# Create Tax column

# Assume Tax = 18%

sales_data$Tax <-
  sales_data$FinalAmount * 0.18

#e) Create TotalBill column

# Create TotalBill column

# Formula:
# TotalBill = FinalAmount + Tax

sales_data$TotalBill <-
  sales_data$FinalAmount + sales_data$Tax

#Q2) Problems on dplyr select() and filter()
#a) Select CustomerName and Product

# Select CustomerName and Product columns

install.packages("dplyr")

library(dplyr)

select_data <- select(
  sales_data,
  CustomerName,
  Product
)

# Display output
head(select_data)

#b) Select numeric columns

# Select only numeric columns

numeric_data <- select_if(
  sales_data,
  is.numeric
)

# Display output
head(numeric_data)

#c) Remove Discount column

# Remove Discount column

remove_discount <- select(
  sales_data,
  -Discount
)

# Display output
head(remove_discount)

#d) Filter Pune customers

# Filter customers from Pune

pune_customers <- filter(
  sales_data,
  City == "Pune"
)

# Display output
head(pune_customers)

#e) Filter multiple conditions (Price > 20000 and Quantity > 5)

# Filter multiple conditions

high_sales <- filter(
  sales_data,
  Price > 20000 &
    Quantity > 5
)

# Display output
head(high_sales)


#Q3) Problems on Using mutate() and arrange()
#a)	Create Tax variable

# Load dplyr package

library(dplyr)

# Create Tax variable using mutate()
# Tax = 18% of FinalAmount

sales_data <- sales_data %>%
  mutate(
    Tax = FinalAmount * 0.18
  )

# Display first 6 rows
head(sales_data)

#b) Create Profit variable

# Create Profit variable
# Profit = 20% of FinalAmount

sales_data <- sales_data %>%
  mutate(
    Profit = FinalAmount * 0.20
  )

# Display first 6 rows
head(sales_data)

#c) Create OrderCategory

# Create OrderCategory variable
# If Revenue is greater than 100000 then High
# Otherwise Low

sales_data <- sales_data %>%
  mutate(
    OrderCategory = ifelse(
      Revenue > 100000,
      "High",
      "Low"
    )
  )

# Display first 6 rows
head(sales_data)

#d) Create DiscountCategory

# Create DiscountCategory variable
# If Discount is 10 or more then High Discount
# Otherwise Low Discount

sales_data <- sales_data %>%
  mutate(
    DiscountCategory = ifelse(
      Discount >= 10,
      "High Discount",
      "Low Discount"
    )
  )

# Display first 6 rows
head(sales_data)


#e) Create PaymentType variable

# Create PaymentType variable
# Cash payment = Offline
# Card and UPI payment = Online

sales_data <- sales_data %>%
  mutate(
    PaymentType = ifelse(
      PaymentMethod == "Cash",
      "Offline",
      "Online"
    )
  )

# Display first 6 rows
head(sales_data)

#f) Sort by Revenue descending by using arrange()

# Sort data by Revenue in descending order

revenue_sort <- sales_data %>%
  arrange(desc(Revenue))

# Display first 6 rows
head(revenue_sort)


#Q4) Problems on Using group_by() and summarise()
#a) Total revenue by city

# Calculate total revenue for each city

city_revenue <- sales_data %>%
  group_by(City) %>%
  summarise(
    TotalRevenue = sum(Revenue)
  )

# Display output
city_revenue


#b) Average price by product

# Calculate average price of each product

average_price <- sales_data %>%
  group_by(Product) %>%
  summarise(
    AveragePrice = mean(Price)
  )

# Display output
average_price


#c) Total quantity sold by product

# Calculate total quantity sold for each product

total_quantity <- sales_data %>%
  group_by(Product) %>%
  summarise(
    TotalQuantity = sum(Quantity)
  )

# Display output
total_quantity

#d) Maximum revenue by city

# Find maximum revenue in each city

maximum_revenue <- sales_data %>%
  group_by(City) %>%
  summarise(
    MaximumRevenue = max(Revenue)
  )

# Display output
maximum_revenue

#e) Minimum revenue by product

# Find minimum revenue for each product

minimum_revenue <- sales_data %>%
  group_by(Product) %>%
  summarise(
    MinimumRevenue = min(Revenue)
  )

# Display output
minimum_revenue

#Q5) Problems on Customer Analysis
#1) Count orders per customer

# Count total orders for each customer

customer_orders <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(
    TotalOrders = n()
  )

# Display output
customer_orders

#2) Total revenue per customer

# Calculate total revenue for each customer

customer_revenue <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(
    TotalRevenue = sum(Revenue)
  )

# Display output
customer_revenue

#3) Average order value

# Calculate average order value for each customer

average_order <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(
    AverageOrderValue = mean(FinalAmount)
  )

# Display output
average_order

#4) Top customer

# Find top customer based on highest revenue

top_customer <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(
    TotalRevenue = sum(Revenue)
  ) %>%
  arrange(desc(TotalRevenue))

# Display output
top_customer

#5) Sort customers by revenue

# Sort customers by revenue in descending order

sorted_customers <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(
    TotalRevenue = sum(Revenue)
  ) %>%
  arrange(desc(TotalRevenue))

# Display output
sorted_customers
