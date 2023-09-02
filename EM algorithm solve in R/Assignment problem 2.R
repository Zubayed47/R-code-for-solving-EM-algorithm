# Set the seed for reproducibility
set.seed(10)

# Generate the known data following a Poisson distribution with mean 9
known_data <- rpois(8, lambda = 9);known_data

# Create a dataset with missing values (two missing values)
data <- c(known_data, NA, NA);data
# Initial values
initial_mean_guess <- 47  # Use your exam roll number

# EM algorithm
max_iterations <- 100
convergence_threshold <- 0.05
iteration <- 1
previous_mean <- initial_mean_guess

# Initialize a vector to store mean estimates at each iteration
mean_estimates <- numeric(max_iterations)

# Condition 
while (iteration <= max_iterations) {
  
  # E-step: Impute missing values using the current mean
  imputed_data <- ifelse(is.na(data), previous_mean, data)
  
  # M-step: Update mean estimate using imputed data
  new_mean <- sum(imputed_data, na.rm = TRUE) / length(imputed_data)
  
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

