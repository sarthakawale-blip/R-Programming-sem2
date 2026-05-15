#Practical 4 - Data Management and Manipulation in R 

#Questions:
  
#Use following data file.

#Import dataset from CSV and Excel file
sales_data <- read.csv("…\\CSV_File\\sales_data.csv")
sales_data_Excel <- read_excel("…\\Excel_File\\sales_data.xlsx")


#Q1) Importing and Exploring Data from CSV file and Excel File

#a) Import dataset from CSV file and Excel file.
#b) Display first 10 rows.
#c) Display last 10 rows.
#d) Check structure of dataset.
#e) Display summary statistics. 

# Install packages
install.packages("readxl")
install.packages("writexl")

# Load libraries
library(readxl)
library(writexl)

# Import CSV file
sales_data <- read.csv("sales_data.csv")

# Import Excel file
library(readxl)

sales_data_Excel <- read_excel("C:/Users/SDBI/Desktop/sales_data.xlsx")

#a) Import dataset from CSV file and Excel file.
sales_data
sales_data_Excel

#b) Display first 10 rows. 
head(sales_data,10)

head(sales_data_Excel,10)

#c) Display last 10 rows
tail(sales_data,10)

tail(sales_data_Excel,10)

#d) Check structure of dataset
str(sales_data)

str(sales_data_Excel)

# e) Display summary statistics
summary(sales_data)

summary(sales_data_Excel)

#Q2) Exporting Data

#a) Export dataset to CSV file.
#b) Export dataset to Excel file.
#c) Export only Electronics category.
#d) Export customers from Pune.
#e) Export top 20 rows.

#a) Export dataset to CSV file
write.csv(sales_data,
          "output_data.csv",
          row.names=FALSE)

#b) Export dataset to Excel file
install.packages("writexl")
library(writexl)
write_xlsx(sales_data,
           "output_data.xlsx")

#c) Export only Electronics category
electronics_data <- subset(
  sales_data,
  Category == "Electronics"
)

write.csv(
  electronics_data,
  "electronics_data.csv",
  row.names = FALSE
)

#d) Export customers from Pune
pune_data <- subset(
  sales_data,
  City == "Pune"
)

write.csv(
  pune_data,
  "pune_customers.csv",
  row.names = FALSE
)

#e) Export top 20 rows
top20 <- head(sales_data, 20)

write.csv(
  top20,
  "top20.csv",
  row.names = FALSE
)

#Q3) Modifying Data and Subset Creation 

#1. Create Profit column.
#2. Rename FinalAmount column.
#3. Remove DiscountAmount column.
#4. Add Tax column.
#5. Select orders with Quantity > 5 by using subset creation. 


#1. Create Profit column.
sales_data$Profit <- sales_data$Revenue * 0.20

sales_data

#2. Rename FinalAmount column.
names(sales_data)[names(sales_data)=="FinalAmount"] <- "TotalAmount"

sales_data

#3) Remove DiscountAmount column
sales_data$DiscountAmount <- NULL

sales_data

#4) Add Tax column
sales_data$Tax <- sales_data$Revenue * 0.18

sales_data

#5) Select orders with Quantity > 5
subset(
  sales_data,
  Quantity > 5
)


#Q4) Column Selection
#1. Select CustomerName and Product columns.
#2. Select first 5 columns.
#3. Select numeric columns.
#4. Remove Discount column.
#5. Select specific columns using index. 

#1. Select CustomerName and Product columns.
sales_data[, c("City","Product")]

#2) Select first 5 columns
sales_data[, 1:5]

#3) Select numeric columns
sales_data[sapply(sales_data, is.numeric)]

#4) Remove Category column
sales_data$Category <- NULL

sales_data

#5) Select specific columns using index
sales_data[, c(2,3,5)]


#Q5) Sorting Data
#1. Sort by City alphabetically.
#2. Sort by City and Revenue
#3. Average price by city.
#4. Maximum order value by city.
#5. Total quantity sold by city.

#1. Sort by City alphabetically.
sales_data[
  order(sales_data$City),
]

#2. Sort by City and Revenue.
sales_data[
  order(sales_data$City,
        sales_data$Revenue),
]

#3) Average price by city
aggregate(
  Price ~ City,
  sales_data,
  mean
)

#4) Maximum revenue by city
aggregate(
  Revenue ~ City,
  sales_data,
  max
)

#5) Total quantity sold by city
aggregate(
  Quantity ~ City,
  sales_data,
  sum
)