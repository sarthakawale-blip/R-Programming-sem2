# Practical 2 : Data Structures and Control Structures in R

# Data Frame : Use this Data frame to solve following Questions
set.seed(123)

sales_data <- data.frame(
  OrderID = 1:100,
  CustomerName = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),100,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),100,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),100,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),100,replace=TRUE),
  Quantity = sample(1:10,100,replace=TRUE),
  Price = sample(seq(500,50000,500),100,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),100,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),100,replace=TRUE)
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount

head(sales_data)

# ------------------------------------------------------------------

# Q1) Filtering Rows
# 1.	Show orders where Price > 20000.
# 2.	Show orders where City = Pune.
# 3.	Show orders where Quantity > 5.
# 4.	Show orders where Category = Electronics.
# 5.	Combine two conditions.

# Solution : 

# 1.	Show orders where Price > 20000.
high_price_orders <- sales_data[sales_data$Price > 20000, ]
high_price_orders

# 2.	Show orders where City = Pune.
pune_orders <- sales_data[sales_data$City == "Pune", ]
pune_orders

# 3.	Show orders where Quantity > 5.
high_quantity_orders <- sales_data[sales_data$Quantity > 5, ]
high_quantity_orders

# 4.	Show orders where Category = Electronics.
electronics_orders <- sales_data[sales_data$Category == "Electronics", ]
electronics_orders

# 5.	Combine two conditions.
combined_orders <- sales_data[sales_data$Price > 20000 & sales_data$Quantity > 5, ]
combined_orders


# Q2) Sorting Data
# 1.	Sort dataset by Price.
# 2.	Sort dataset by Revenue.
# 3.	Sort by City alphabetically.
# 4.	Sort by Quantity descending.
# 5.	Sort by multiple columns.

# Solution:

# 1.	Sort dataset by Price.
sales_data_price <- sales_data[order(sales_data$Price), ]
head(sales_data_price)

# 2.	Sort dataset by Revenue.
sales_data_revenue <- sales_data[order(sales_data$Revenue), ]
head(sales_data_revenue)

# 3.	Sort by City alphabetically.
sales_data_city <- sales_data[order(sales_data$City), ]
head(sales_data_city)

# 4.	Sort by Quantity descending.
sales_data_quantity <- sales_data[order(-sales_data$Quantity), ]
head(sales_data_quantity)

# 5.	Sort by multiple columns.
sales_data_multi <- sales_data[order(sales_data$City, sales_data$Price), ]
head(sales_data_multi)


# Q3) Aggregation
# 1.	Total revenue by city.
# 2.	Average price by category.
# 3.	Total quantity sold by product.
# 4.	Maximum order value by city.
# 5.	Minimum order value by product.

# Solution:

# 1.	Total revenue by city.
aggregate(Revenue ~ City, data = sales_data, sum)

# 2.	Average price by category.
aggregate(Price ~ Category, data = sales_data, mean)

# 3.	Total quantity sold by product.
aggregate(Quantity ~ Product, data = sales_data, sum)

# 4.	Maximum order value by city.
aggregate(FinalAmount ~ City, data = sales_data, max)

# 5.	Minimum order value by product.
aggregate(FinalAmount ~ Product, data = sales_data, min)


# Q4) Conditional Statements
# 1. If discount > 10 mark "High Discount".
# 2. Classify orders into High / Medium / Low value by using if else.
# 3. Print revenue of each order by using for loop.
# 4. Calculate cumulative revenue by using while loop
# 5. Skip fashion category. 

# Solution :

# 1. If discount > 10 mark "High Discount".
sales_data$Discount_Status <- ifelse(sales_data$Discount > 10, "High Discount", "Low Discount")
head(sales_data)

# 2. Classify orders into High / Medium / Low value by using if else.
sales_data$Order_Value <- ifelse(sales_data$FinalAmount > 30000, "High",
                                 ifelse(sales_data$FinalAmount > 15000, "Medium", "Low"))
head(sales_data)

# 3. Print revenue of each order by using for loop.
for(i in 1:nrow(sales_data)) {
  print(sales_data$Revenue[i])
}

# 4. Calculate cumulative revenue by using while loop
i <- 1
total <- 0

while(i <= nrow(sales_data)) {
  total <- total + sales_data$Revenue[i]
  i <- i + 1
}

total

# 5. Skip fashion category. 
for(i in 1:nrow(sales_data)) {
  if(sales_data$Category[i] == "Fashion") {
    next
  }
  print(sales_data[i, ])
}

# Q5) Discount Analysis
# 1.	Find Average discount.
# 2.	Orders with discount > 10%.
# 3.	Total discount amount.
# 4.	City with highest discount.
# 5.	Count discount levels.

# Solution:

# 1.	Find Average discount.
mean(sales_data$Discount)

# 2.	Orders with discount > 10%.
sales_data[sales_data$Discount > 10, ]

# 3.	Total discount amount.
sum(sales_data$DiscountAmount)

# 4.	City with highest discount.
aggregate(DiscountAmount ~ City, data = sales_data, sum)

# 5.	Count discount levels.
agg <- aggregate(DiscountAmount ~ City, data = sales_data, sum)
agg[which.max(agg$DiscountAmount), ]


# Q6) Multi Condition Filtering
# 1.	Electronics + Price > 20000
# 2.	Fashion + Quantity > 5
# 3.	Pune + Discount > 10
# 4.	Revenue > 50000 + Cash payment
# 5.	High price + high quantity

# Solution:

# 1.	Electronics + Price > 20000
sales_data[sales_data$Category == "Electronics" & sales_data$Price > 20000, ]

# 2.	Fashion + Quantity > 5
sales_data[sales_data$Category == "Fashion" & sales_data$Quantity > 5, ]

# 3.	Pune + Discount > 10
sales_data[sales_data$City == "Pune" & sales_data$Discount > 10, ]

# 4.	Revenue > 50000 + Cash payment
sales_data[sales_data$Revenue > 50000 & sales_data$PaymentMethod == "Cash", ]

# 5.	High price + high quantity
sales_data[sales_data$Price > 20000 & sales_data$Quantity > 5, ]



