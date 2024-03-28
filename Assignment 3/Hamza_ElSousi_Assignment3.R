# Course: Numerical Computing 
# Hamza El Sousi
# 040982818
# Assignment 3


# Function to calculate the derivative of y
dy <- function(t, y) {
  return(cos(4 * t) - 2 * y)
}

# Euler's Method
euler_method <- function(h, y0, t_end) {
  t <- seq(0, t_end, by = h)
  y <- numeric(length(t))
  y[1] <- y0
  for (i in 1:(length(t) - 1)) {
    y[i + 1] <- y[i] + h * dy(t[i], y[i])
  }
  return(list(time = t, y = y))
}

# Runge-Kutta 4th Order Method
rk4_method <- function(h, y0, t_end) {
  t <- seq(0, t_end, by = h)
  y <- numeric(length(t))
  y[1] <- y0
  for (i in 1:(length(t) - 1)) {
    k1 <- h * dy(t[i], y[i])
    k2 <- h * dy(t[i] + h/2, y[i] + k1/2)
    k3 <- h * dy(t[i] + h/2, y[i] + k2/2)
    k4 <- h * dy(t[i] + h, y[i] + k3)
    y[i + 1] <- y[i] + (k1 + 2*k2 + 2*k3 + k4) / 6
  }
  return(list(time = t, y = y))
}

# Exact Solution
exact_solution <- function(t) {
  return(0.1 * cos(4 * t) + 0.2 * sin(4 * t) + 2.9 * exp(-2 * t))
}

# Main function to be called for the assignment
ODEsolver <- function() {
  # Menu system and rest of the logic goes here
}

# Main function to be called for the assignment
ODEsolver <- function() {
  cat("Choose the method for solving the ODE:\n")
  cat("1. Euler’s Method\n")
  cat("2. Runge-Kutta 4th Order Method\n")
  
  # Assuming method choice is input by the user
  method_choice <- as.integer(readline(prompt = "Enter choice (1 or 2): "))
  
  cat("\nChoose step size 'h' (0.8, 0.2, 0.05)\n")
  
  # Assuming step size choice is input by the user
  step_size <- as.numeric(readline(prompt = "Enter step size: "))
  
  # Initial conditions
  y0 <- 3
  t_end <- 2
  
  # Solve the ODE based on the method chosen
  if(method_choice == 1) {
    results <- euler_method(step_size, y0, t_end)
  } else if(method_choice == 2) {
    results <- rk4_method(step_size, y0, t_end)
  } else {
    cat("Invalid method choice.\n")
    return()
  }
  
  # Calculate exact solution and relative error
  exact_vals <- sapply(results$time, exact_solution)
  relative_errors <- abs((exact_vals - results$y) / exact_vals) * 100
  
  # Display results
  cat("Time(s)\tExact Temp(C)\tEstimated Temp(C)\tRelative Error(%)\n")
  for (i in 1:length(results$time)) {
    cat(sprintf("%.2f\t%.3f\t\t%.3f\t\t\t%.2f\n", results$time[i], exact_vals[i], results$y[i], relative_errors[i]))
  }
}

#calling ODEsolver() to prompt user input and make computations
 ODEsolver()

