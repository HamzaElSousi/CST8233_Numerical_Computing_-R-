# Hamza El Sousi
# 040982818


# Ensure the 'polynom' package is installed and loaded
if (!requireNamespace("polynom", quietly = TRUE)) {
  install.packages("polynom")
}
library(polynom)

# Given data points
x_values <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
f_values <- c(0, -1.921, 0, 3.584, 0, -6.718, 0)

# MyIntCal function definition
MyIntCal <- function(x, x_values, f_values) {
  n <- length(x_values)
  result <- 0
  for (i in 1:n) {
    L <- 1
    for (j in 1:n) {
      if (i != j) {
        L <- L * (x - x_values[j]) / (x_values[i] - x_values[j])
      }
    }
    result <- result + L * f_values[i]
  }
  return(result)
}

# Adjust plot settings or use RStudio's zoom feature to increase the plot area size
# Plot setup
par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))

# Plotting each Lagrange polynomial
for (i in 1:length(x_values)) {
  plot(x_values, f_values, type = "o", col = "red", main = paste("L", i), xlim = range(x_values), ylim = range(f_values))
  # Adjusted dummy plot line
  curve(MyIntCal(x, x_values, f_values), add = TRUE, col = "blue")
}

# Resetting plot parameters to default for the final plot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Plotting the final interpolating function
plot(x_values, f_values, type = "o", col = "blue", main = "Interpolating Function", xlim = range(x_values), ylim = range(f_values))
curve(MyIntCal(x, x_values, f_values), add = TRUE, col = "green")

# Finding the interpolating polynomial using poly.calc from the polynom package
pf_x <- poly.calc(x_values, f_values)

# Calculating f(15) and f(24) using MyIntCal
f_15_MyIntCal <- MyIntCal(15, x_values, f_values)
f_24_MyIntCal <- MyIntCal(24, x_values, f_values)

# Calculating f(15) and f(24) using the polynomial from poly.calc
f_15_pf_x <- predict(pf_x, 15)
f_24_pf_x <- predict(pf_x, 24)

# Printing the results
print(paste("f(15) using MyIntCal:", f_15_MyIntCal))
print(paste("f(24) using MyIntCal:", f_24_MyIntCal))
print(paste("f(15) using pf_x:", f_15_pf_x))
print(paste("f(24) using pf_x:", f_24_pf_x))
