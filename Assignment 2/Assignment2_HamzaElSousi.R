# Course: Numerical Computing 
# Hamza El Sousi
# 040982818
# Assignment 2


# Set the correct working directory
setwd("C:/Users/Hamza/Desktop/Algonquin college/Semester Four/Numerical Computing/Assignment 2")

# Install necessary packages
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("minpack.lm", quietly = TRUE)) install.packages("minpack.lm")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(minpack.lm)

# Step 1: Menu System
repeat {
  cat("\n1. Calculate best fit\n2. Quit\nSelect an option: ")
  option <- as.integer(readline())
  
  if (option == 2) {
    cat("Exiting program.\n")
    break  # Exit the loop to end the program
  } else if (option == 1) {
    # Check if the file exists within this branch
    if (!file.exists("rocket.xlsx")) {
      cat("File 'rocket.xlsx' not found in ", getwd(), ". Please ensure the file is in the current directory.\n")
      next  # Skip to the next iteration to show the menu again
    }
    
    # Step 2: Load Data
    rocket_data <- read_excel("rocket.xlsx")  # Load the data
    
    # Ensure column names are correctly matched to your data structure
    names(rocket_data) <- c("t_sec", "d_meter")  # Adjust column names if necessary
    
    # Step 3: Non-linear Regression Analysis
    # Attempt fitting models with nlsLM for more robust handling
    control = nls.lm.control(maxiter = 100)  # Increase max iterations to 100
    
    power_model <- nlsLM(d_meter ~ a * t_sec^b, data = rocket_data, start = list(a = 1, b = 1), control = control)
    exp_model <- nlsLM(d_meter ~ a * exp(b * t_sec), data = rocket_data, start = list(a = 1, b = 1), control = control)
    
    
    # Step 4: Model Comparison and Extrapolation
    ssr_power <- sum(resid(power_model)^2)
    ssr_exp <- sum(resid(exp_model)^2)
    
    if (ssr_power < ssr_exp) {
      cat("Power model is the best fit.\n")
      best_model <- power_model
    } else {
      cat("Exponential model is the best fit.\n")
      best_model <- exp_model
    }
    
    # Ask for a time value and predict distance
    cat("Enter a time value for extrapolation: ")
    time_value <- as.numeric(readline())
    predicted_distance <- predict(best_model, newdata = data.frame(t_sec = time_value))
    cat("Predicted distance: ", predicted_distance, "\n")
    
    # Step 5: Plotting
    # Corrected Step 5: Plotting
    ggplot(rocket_data, aes(x = t_sec, y = d_meter)) +
      geom_point() +
      geom_line(aes(y = predict(best_model, newdata = rocket_data)), color = 'red') +
      labs(title = "Rocket Distance vs. Time", x = "Time (sec)", y = "Distance (meters)")
    
    # Use ggsave to save the last plot
    ggsave("best_fit.pdf")
    
    
  } else {
    cat("Invalid option. Please try again.\n")
  }
}

