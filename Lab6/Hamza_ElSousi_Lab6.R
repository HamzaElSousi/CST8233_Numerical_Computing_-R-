# Course: Numerical Computing 
# Hamza El Sousi
# 040982818
# Lab 6

library(ggplot2)

# Define the differential equation as a function
# This function represents the ODE dy/dt = -y * cos(t), defining the rate of change of y with respect to t
dydt <- function(t, y) {
  -y * cos(t)
}

# Implement Euler's method for solving ODEs
# Euler's method approximates solutions at discrete points by iterating over a step size 'h'
euler_method <- function(dydt, y0, t, h) {
  y <- numeric(length(t))  # Initialize a vector to store y values
  y[1] <- y0  # Set the initial condition
  for (i in 2:length(t)) {
    # Calculate y[i] based on the previous value and the derivative at that point
    y[i] <- y[i-1] + h * dydt(t[i-1], y[i-1])
  }
  return(y)  # Return the vector of y values
}


# Initial condition and time range
y0 <- 1.241
t_max <- 6

# Solve the ODE for different step sizes and prepare for plotting
plot_list <- list()
hs <- c(0.5, 0.25, 0.1)  # Different step sizes to be evaluated
for (h in hs) {
  t <- seq(0, t_max, by = h)  # Create a sequence of time points based on the current step size
  y <- euler_method(dydt, y0, t, h)  # Solve the ODE using Euler's method
  plot_list[[as.character(h)]] <- data.frame(t = t, y = y, h = as.character(h))  # Store results for plotting
}

# Combine data for plotting
plot_data <- do.call(rbind, plot_list)

# Compute the analytical solution for comparison
# The analytical solution is provided in the lab instructions
t_analytical <- seq(0, t_max, by = 0.01)
y_analytical <- 0.5 * exp(sin(t_analytical)) * 2 * exp(-sin(t_analytical))
analytical_data <- data.frame(t = t_analytical, y = y_analytical, h = 'Analytical')

# Combine Euler method and analytical solution data
plot_data <- rbind(plot_data, analytical_data)


# Plot the results using ggplot2
ggplot(plot_data, aes(x = t, y = y, color = h)) +
  geom_line() +
  scale_color_manual(values = c('0.5' = 'blue', '0.25' = 'green', '0.1' = 'red', 'Analytical' = 'black')) +
  theme_minimal() +
  labs(title = "Displacement over Time using Euler's Method and Analytical Solution",
       x = "Time (t)",
       y = "Displacement (y)",
       color = "Step Size / Solution") +
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10))

# Calculate and print errors for the step size h = 0.5
# This section calculates the absolute and relative errors between the Euler method and analytical solution
t_error <- seq(0, t_max, by = 0.5)
y_euler_error <- euler_method(dydt, y0, t_error, 0.5)
y_analytical_at_euler_points <- approx(t_analytical, y_analytical, xout = t_error)$y

absolute_errors <- abs(y_euler_error - y_analytical_at_euler_points)  # Calculate absolute errors
relative_errors <- absolute_errors / abs(y_analytical_at_euler_points)  # Calculate relative errors

# Print error statistics
cat("Maximum Absolute Error:", max(absolute_errors), "\n")
cat("Maximum Relative Error:", max(relative_errors), "\n")
cat("Mean Absolute Error:", mean(absolute_errors), "\n")
cat("Mean Relative Error:", mean(relative_errors), "\n")