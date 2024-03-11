# Course: Numerical Computing
# Lab 4
# Hamza El Sousi

# 040982818

# PART 1: Plotting

# Load necessary libraries
library(ggplot2)

# Define the original function
f <- function(x) log(1 + x)

# Define the approximation function with six terms
f6 <- function(x) x - x^2/2 + x^3/3 - x^4/4 + x^5/5 - x^6/6

# Generate data for plotting
x_values <- seq(-0.5, 1.5, by = 0.01)
data <- data.frame(
  x = x_values,
  original = f(x_values),
  approximation = f6(x_values)
)

# Plot
ggplot(data, aes(x)) +
  geom_line(aes(y = original, colour = "Original")) +
  geom_line(aes(y = approximation, colour = "Approximation")) +
  labs(y = "f(x)", title = "Original function vs Maclaurin Series Approximation") +
  theme_minimal()

# PART 2: Computing Series and Errors

# Input from the user
x <- as.numeric(readline(prompt = "Enter the value of x: "))

# Original function value
original_value <- log(1 + x)

# Initialize variables to store series value and errors
series_value <- 0
errors <- data.frame(n = integer(), AbsoluteError = numeric(), RelativeError = numeric())

# Compute series, absolute and relative errors for up to ten terms
for (n in 1:10) {
  term <- ((-1)^(n + 1) * x^n) / n
  series_value <- series_value + term
  absolute_error <- abs(original_value - series_value)
  relative_error <- ifelse(abs(original_value) > .Machine$double.eps, absolute_error / abs(original_value), NA)
  
  errors <- rbind(errors, data.frame(n = n, AbsoluteError = absolute_error, RelativeError = relative_error))
}

# Print the errors table
print(errors)

# Write the errors to a CSV file
write.csv(errors, "C:\\Users\\Hamza\\Desktop\\Algonquin college\\Semester Four\\Numerical Computing\\Lab4\\maclaurin_series_errors.csv", row.names = FALSE)
