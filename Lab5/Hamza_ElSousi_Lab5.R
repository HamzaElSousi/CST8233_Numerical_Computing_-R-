# Course: Numerical Computing 
# Hamza El Sousi
# 040982818
# Lab 5


# Check if 'readxl' package is installed; if not, install it
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

# Load necessary libraries
library(readxl)


# A more flexible file path
file_path <- "C:\\Users\\Hamza\\Desktop\\Algonquin college\\Semester Four\\Numerical Computing\\Lab5\\rocket.xlsx"

# Ensure the file path is correct and accessible
if (!file.exists(file_path)) {
  stop("File path is incorrect or the file is not accessible.")
}

# Read the rocket data from the Excel file
rocket_data <- read_excel(file_path)
xVec <- rocket_data[[1]] # Assuming the first column is time in seconds
yVec <- rocket_data[[2]] # Assuming the second column is distance in meters

# Function to calculate Taylor series for ln(x) around a=1
calculate_taylor_series <- function(x, n_terms) {
  series_value <- 0
  for (n in 1:n_terms) {
    term <- ((-1)^(n - 1) / n) * (x - 1)^n
    series_value <- series_value + term
    # Calculate absolute and relative errors
    actual_value <- log(x)
    absolute_error <- abs(actual_value - series_value)
    # Check to avoid division by zero
    if (actual_value == 0) {
      relative_error <- NA # Use NA to indicate the relative error is not applicable
    } else {
      relative_error <- absolute_error / abs(actual_value)
    }
    cat(sprintf("Term: %d, ln(x): %f, Absolute error: %f, Relative error: %f\n",
                n, series_value, absolute_error, ifelse(is.na(relative_error), NA, relative_error)))
  }
}


# Function to calculate the first derivative using CDD
calculate_velocity <- function(xVec, yVec) {
  velocity <- numeric(length = length(xVec))
  for (i in 2:(length(xVec)-1)) {
    velocity[i] <- (yVec[i+1] - yVec[i-1]) / (xVec[i+1] - xVec[i-1])
  }
  velocity <- velocity / 1000 # Convert velocity from m/s to km/s
  return(velocity)
}

# Function to calculate the second derivative (acceleration)
calculate_acceleration <- function(xVec, yVec) {
  acceleration <- numeric(length = length(xVec))
  for (i in 2:(length(xVec)-1)) {
    acceleration[i] <- (yVec[i+1] - 2*yVec[i] + yVec[i-1]) / ((xVec[i+1] - xVec[i])^2)
  }
  acceleration <- acceleration / 1000 # Convert acceleration from m/s^2 to km/s^2
  return(acceleration)
}

# Calculate velocity and acceleration
velocity <- calculate_velocity(xVec, yVec)
acceleration <- calculate_acceleration(xVec, yVec)

# Plotting velocity (excluding the first and last points)
plot(xVec[-c(1, length(xVec))], velocity[-c(1, length(velocity))], type = 'b', col = 'blue', xlab = 'Time (seconds)', ylab = 'Velocity (km/s)', main = 'Velocity of the Rocket as a Function of Time')

# Plotting acceleration (excluding the first and last points)
plot(xVec[-c(1, length(xVec))], acceleration[-c(1, length(acceleration))], type = 'b', col = 'red', xlab = 'Time (seconds)', ylab = 'Acceleration (km/s^2)', main = 'Acceleration of the Rocket as a Function of Time')

# below is to calculate Taylor series for a given x and n_terms
x <- as.numeric(readline(prompt = "Please enter the value of x: "))
n_terms <- 10
calculate_taylor_series(x, n_terms)
