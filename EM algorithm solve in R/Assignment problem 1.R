# Given data
data <- c(1, 5, 10, 4)
NA_value <- 2
initial_mean <- 3

# Combining known and missing data
data <- c(data, rep(NA, NA_value));data

# EM algorithm
max_iterations <- 100
convergence_threshold <- 0.05
iteration <- 1
previous_mean <- initial_mean

# Initialize a vector to store mean estimates at each iteration
mean_estimates <- numeric(max_iterations)

# Condition 
while (iteration <= max_iterations) {
  
  # E-step: Impute missing values using the current mean
  imputed_data <- ifelse(is.na(data), previous_mean,data)
  
  # M-step: Update mean estimate using imputed data
  new_mean <- mean(imputed_data)
  
  # Store the mean estimate for this iteration
  mean_estimates[iteration] <- new_mean
  
  # Check for convergence
  if (abs(new_mean - previous_mean) < convergence_threshold) {
    break
  }
  
  previous_mean <- new_mean
  iteration <- iteration + 1
}

# Results
results_list <- list(
  'Estimated Mean' = estimate_mean <- new_mean,
  'Number of Iterations' = iterations <- iteration,
  'Data with Missing Values' = missing_value <-imputed_data,
  'All valid iterations mean'=mean_estimates <- mean_estimates[1:iteration]
)

print(results_list)


