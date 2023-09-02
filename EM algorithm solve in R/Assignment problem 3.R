set.seed(47)  # Your exam roll number

# Generate the data
data <- rpois(20, lambda = 15);data
mean(data)
var(data)
missing_indices <- c(19, 20)
data[missing_indices] <- NA;data

# Initial values
initial_mean_guess <- 13  # Initial guess for the mean
initial_variance_guess <- 17  # Initial guess for the variance

# EM algorithm
max_iterations <- 100
convergence_threshold <- 0.05
iteration <- 1
previous_mean <- initial_mean_guess
previous_variance <- initial_variance_guess

# Initialize a vector to store mean and variance estimates at each iteration
mean_estimates <- numeric(max_iterations)
variance_estimates <- numeric(max_iterations)

# Condition 
while (iteration <= max_iterations) {
  # E-step: Impute missing values using the current mean and variance
  imputed_data <- ifelse(is.na(data), previous_mean, data)
  
  # M-step: Update mean and variance estimates using imputed data
  new_mean <- sum(imputed_data, na.rm = TRUE) / length(imputed_data)
  new_variance <- sum((imputed_data - new_mean)^2, na.rm = TRUE) / length(imputed_data)
  
  # Store the mean estimate for this iteration
  mean_estimates[iteration] <- new_mean
  variance_estimates[iteration] <- new_variance
  
  # Check for convergence
  if (abs(new_mean - previous_mean) < convergence_threshold &&
      abs(new_variance - previous_variance) < convergence_threshold) {
    break
  }
  
  previous_mean <- new_mean
  previous_variance <- new_variance
  iteration <- iteration + 1
}

# Results
results_list <- list(
  'Estimated Mean' = estimate_mean <- new_mean,
  'Estimated Variance' = estimate_varance <- new_variance,
  'Number of Iterations' = iterations <- iteration,
  'Data with Missing Values' = missing_value <-imputed_data,
  'All valid iterations mean'=mean_estimates <- mean_estimates[1:iteration],
  'All valid iterations variance'=variance_estimates <- variance_estimates[1:iteration]
)

print(results_list)
