# Course: Numerical Computing 
# Lab 3
# Hamza El Sousi
# 040982818


#Task 1: Define Polynomials
# Load the PolynomF package
library(PolynomF)

# Define two polynomials
poly1 <- polynomial(c(1, 2, 3))  # 1 + 2x + 3x^2
poly2 <- polynomial(c(2, -1, 4)) # 2 - x + 4x^2

# Print the defined polynomials
print(poly1)
print(poly2)


#Task 2: Perform Operations (Addition, Subtraction, Multiplication)
# Addition of polynomials
add_result <- poly1 + poly2
print(add_result)

# Subtraction of polynomials
sub_result <- poly1 - poly2
print(sub_result)

# Multiplication of polynomials
mul_result <- poly1 * poly2
print(mul_result)


#Task 3: Find Derivatives
# Derivative of poly1
derivative_poly1 <- deriv(poly1)
print(derivative_poly1)

# Derivative of poly2
derivative_poly2 <- deriv(poly2)
print(derivative_poly2)



#Task 4: Plot Polynomials and Their Derivatives
# Plot poly1 and its derivative
plot(poly1, main="Poly1 and Its Derivative")
lines(derivative_poly1, col="red")

# Plot poly2 and its derivative with adjusted x-axis range
plot(poly2, main="Poly2 and Its Derivative", xlim = range(0, 50))
lines(derivative_poly2, col="blue")


################################################################

#Task 1: Work with the airquality Dataset
# Load the airquality dataset
data(airquality)

# View the first few rows of the dataset
head(airquality)


#Task 2: Calculate Mean, Median, and Standard Deviation
# Calculate mean temperature for May
mean_may <- mean(airquality$Temp[airquality$Month == 5], na.rm = TRUE)
cat("Mean temperature for May:", mean_may, "\n")

# Calculate median temperature for June
median_june <- median(airquality$Temp[airquality$Month == 6], na.rm = TRUE)
cat("Median temperature for June:", median_june, "\n")

# Calculate standard deviation for July
sd_july <- sd(airquality$Temp[airquality$Month == 7], na.rm = TRUE)
cat("Standard deviation for July:", sd_july, "\n")

#Task 3: Calculate Probabilities using the Normal Distribution
# Calculate the probability of temperature being less than 80 degrees
prob_less_than_80 <- pnorm(80, mean = mean(airquality$Temp, na.rm = TRUE), sd = sd(airquality$Temp, na.rm = TRUE))
cat("Probability of temperature less than 80 degrees:", prob_less_than_80, "\n")

# Calculate the probability of temperature being between 70 and 85 degrees
prob_between_70_85 <- pnorm(85, mean = mean(airquality$Temp, na.rm = TRUE), sd = sd(airquality$Temp, na.rm = TRUE)) - 
  pnorm(70, mean = mean(airquality$Temp, na.rm = TRUE), sd = sd(airquality$Temp, na.rm = TRUE))
cat("Probability of temperature between 70 and 85 degrees:", prob_between_70_85, "\n")

#Task 4: Calculate Probabilities using the Z-Table

# Calculate the z-score for temperature 80 degrees
z_score_80 <- (80 - mean(airquality$Temp, na.rm = TRUE)) / sd(airquality$Temp, na.rm = TRUE)

# Calculate the probability of temperature being less than 80 degrees using the z-table
prob_less_than_80_z <- pnorm(z_score_80)
cat("Probability of temperature less than 80 degrees using the z-table:", prob_less_than_80_z, "\n")

# Calculate the z-scores for temperatures 70 and 85 degrees
z_score_70 <- (70 - mean(airquality$Temp, na.rm = TRUE)) / sd(airquality$Temp, na.rm = TRUE)
z_score_85 <- (85 - mean(airquality$Temp, na.rm = TRUE)) / sd(airquality$Temp, na.rm = TRUE)

# Calculate the probability of temperature being between 70 and 85 degrees using the z-table
prob_between_70_85_z <- pnorm(z_score_85) - pnorm(z_score_70)
cat("Probability of temperature between 70 and 85 degrees using the z-table:", prob_between_70_85_z, "\n")

# Task 5: Calculate Probability of Temperature Being Greater than a Value

# Define the temperature threshold (e.g., 75 degrees)
threshold_temperature <- 75

# Calculate the probability of temperature being greater than the threshold
prob_greater_than_threshold <- 1 - pnorm(threshold_temperature, mean = mean(airquality$Temp, na.rm = TRUE), sd = sd(airquality$Temp, na.rm = TRUE))
cat("Probability of temperature greater than", threshold_temperature, "degrees:", prob_greater_than_threshold, "\n")


