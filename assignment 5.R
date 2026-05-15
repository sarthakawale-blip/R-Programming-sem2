# Create the synthetic data
sales_data <- data.frame(
  OrderID = 1:30,
  City = sample(c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"), 30, replace = TRUE),
  Category = sample(c("Electronics", "Clothing", "Home", "Toys"), 30, replace = TRUE),
  Product = sample(c("Laptop", "Shirt", "Lamp", "Action Figure", "Phone"), 30, replace = TRUE),
  Price = round(runif(30, 20, 500), 2),
  Quantity = sample(1:10, 30, replace = TRUE),
  Discount = round(runif(30, 0, 0.3), 2)
)

# Calculate Revenue (Price * Quantity minus Discount)
sales_data$Revenue <- round((sales_data$Price * sales_data$Quantity) * (1 - sales_data$Discount), 2)

# Save it as a CSV file in your working directory
write.csv(sales_data, "sales_data.csv", row.names = FALSE)

# Show the first few rows
head(sales_data)
# Load the necessary library
library(ggplot2)

# Load the dataset
# Replace the path with your actual file location
sales_data <- read.csv("sales_data.csv")

# --- Q1) Basic ggplot Charts ---

# a) Create scatter plot of Price vs Revenue
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point() +
  labs(title = "Scatter plot of Price vs Revenue")

# b) Create bar chart for City
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  labs(title = "Bar chart for City")

# c) Create histogram of Price
ggplot(sales_data, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(title = "Histogram of Price")

# d) Create boxplot of Revenue by Category
ggplot(sales_data, aes(x = Category, y = Revenue)) +
  geom_boxplot() +
  labs(title = "Boxplot of Revenue by Category")

# e) Create line chart for Revenue by OrderID
ggplot(sales_data, aes(x = OrderID, y = Revenue)) +
  geom_line() +
  labs(title = "Line chart for Revenue by OrderID")

# --- Q2) Customized Visualization ---

ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "steelblue") +                     # c) Change color of bars
  labs(title = "Total Orders by City",               # a) Add title
       x = "City Name",                              # b) Change axis labels
       y = "Number of Orders") +
  theme_minimal() +                                  # d) Change theme style
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # e) Rotate x-axis labels

# --- Q3) Multi-Variable Visualization ---

# a) Price vs Revenue colored by Category
ggplot(sales_data, aes(x = Price, y = Revenue, color = Category)) +
  geom_point()

# b) Price vs Quantity colored by Category
ggplot(sales_data, aes(x = Price, y = Quantity, color = Category)) +
  geom_point()

# c) Revenue vs Discount
ggplot(sales_data, aes(x = Discount, y = Revenue)) +
  geom_point()

# d) Top cities by revenue (Aggregation required)
city_revenue <- aggregate(Revenue ~ City, data = sales_data, sum)
ggplot(city_revenue, aes(x = reorder(City, -Revenue), y = Revenue)) +
  geom_bar(stat = "identity")

# e) Top products by sales (Assuming sales = Quantity)
product_sales <- aggregate(Quantity ~ Product, data = sales_data, sum)
ggplot(product_sales, aes(x = reorder(Product, -Quantity), y = Quantity)) +
  geom_bar(stat = "identity")

# --- Q4) Bar Chart, Scatter Plot, Histogram Analysis ---

# a) Number of orders per city (Bar Chart)
ggplot(sales_data, aes(x = City)) + geom_bar()

# b) Price vs Revenue (Scatter Plot)
ggplot(sales_data, aes(x = Price, y = Revenue)) + geom_point()

# c) Quantity vs Revenue (Scatter Plot)
ggplot(sales_data, aes(x = Quantity, y = Revenue)) + geom_point()

# d) Distribution of Quantity (Histogram)
ggplot(sales_data, aes(x = Quantity)) + geom_histogram(binwidth = 1)

# e) Histogram by Category
ggplot(sales_data, aes(x = Category)) + geom_bar() # (Categorical counts)

# --- Q5) Box Plot, Line Chart, Faceted Chart Analysis ---

# a) Revenue by Category (Box Plot)
ggplot(sales_data, aes(x = Category, y = Revenue)) + geom_boxplot()

# b) Price by Product (Box Plot)
ggplot(sales_data, aes(x = Product, y = Price)) + geom_boxplot()

# c) Revenue trend by OrderID (Line Chart)
ggplot(sales_data, aes(x = OrderID, y = Revenue)) + geom_line()

# d) Discount trend (Line Chart)
ggplot(sales_data, aes(x = OrderID, y = Discount)) + geom_line()

# e) City orders by category (Faceted Chart)
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  facet_wrap(~Category) +
  theme(axis.text.x = element_text(angle = 90))