# Practical 1 Solution - Introduction to R and Basic Operations


# Q1) Write an R program that: 
#     1.	Takes two numeric values from user. 
#     2.	Performs all arithmetic operations (+, −, *, /, power, modulus). 
#     3.	Stores the results in a vector. 
#     4.	Displays the maximum, minimum, and average value of the results. 

# Solution :

# First Number taken from User
a <- as.numeric(readline(prompt = "Enter a First number : "))

# Second Number taken from User
b <- as.numeric(readline(prompt = "Enter a second number : "))

# Arithmetic operations
add <- a + b
sub <- a - b
mul <- a * b
div <- a / b
power <- a ^ b
mod <- a %% b

# Store results in vector
results <- c(add, sub, mul, div, power, mod)

print(results)

# Numeric analysis
max_value <- max(results)
min_value <- min(results)
avg_value <- mean(results)

print(max_value)
print(min_value)
print(avg_value)


# Q2) Create an R program that:
#     1. Generates 10 random numbers between 1 and 100.
#     2. Calculates square root and then rounded value, ceiling, and floor for each square root value.
#     3. Stores results in a data frame.

# Solution :

# Generate random numbers
numbers <- sample(1:100, 10)

# Numeric functions
sqrt_values <- sqrt(numbers)
rounded_values <- round(sqrt_values)
ceiling_values <- ceiling(sqrt_values)
floor_values <- floor(sqrt_values)

# Create data frame
result_df <- data.frame(
  Numbers = numbers,
  SquareRoot = sqrt_values,
  Rounded = rounded_values,
  Ceiling = ceiling_values,
  Floor = floor_values
)

print(result_df)


# Q3) Write an R program that:
#     1. Takes a list of names.
#     2. Converts all names to uppercase.
#     3. Calculates the number of characters in each name.
#     4. Combines the name and its length into a formatted sentence.

# Solution :

names_list <- c("Rahul", "Sachin", "Amit", "Priya")

# Convert to uppercase
upper_names <- toupper(names_list)

# Count characters
name_length <- nchar(upper_names)

# Combine into sentence
result <- paste(upper_names, "has", name_length, "characters")

print(result)


# Q4) Write an R program that:
#     1. Creates a vector of numbers from 1 to 200.
#     2. Finds numbers that are divisible by 3 and 5.
#     3. Calculates square root and square of those numbers.
#     4. Displays results in a table format.

# Solution :

numbers <- 1:200

# Numbers divisible by 3 and 5
selected <- numbers[numbers %% 3 == 0 & numbers %% 5 == 0]

# Square and square root
square <- selected^2
square_root <- sqrt(selected)

# Create table
result_table <- data.frame(
  Number = selected,
  Square = square,
  SquareRoot = square_root
)

print(result_table)



# Q5) Write an R program to evaluate a quadratic equation 
#     for given a, b, c values and return real or complex roots.

# Formula : 
# For ax² + bx + c = 0
# Discriminant: D = b² − 4ac
# Roots: x = (-b ± √D) / 2a

# Solution :

a <- 1
b <- 4
c <- 5

D <- b^2 - 4*a*c   # Discriminant

root1 <- (-b + sqrt(as.complex(D))) / (2*a)
root2 <- (-b - sqrt(as.complex(D))) / (2*a)

cat("Root 1:", root1, "\n")
cat("Root 2:", root2, "\n")


#Q_6
#Generate number from 1 to 200
#Find numbers:
#Divisible by or 7
#Divisible by both 3 and 7

num <- (1:200)
num
x <- num[num%%3 ==0 | num%%7 ==0]
x

Y <- num[num%%3==0 & num%%7==0]
Y

#Q_7
x <- c(10, 20, NA, 30, NA, 40)
sum(x, na.rm = TRUE)
mean(x, na.rm = TRUE)
