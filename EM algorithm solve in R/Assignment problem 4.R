set.seed(47)  # My exam roll number

# Generate the data
data <- rexp(10, rate = 1/15);data
missing_indices <- c(5, 6)
data[missing_indices] <- NA;data
result.mean <-  mean(data,na.rm = TRUE);result.mean

# Initial value
initial_mean_guess <- 8  # Initial guess for the mean

# EM algorithm
max_iterations <- 100
convergence_threshold <- 0.05
iteration <- 1
previous_mean <- initial_mean_guess

while (iteration <= max_iterations) {
  
  # E-step: Impute missing values using the current mean
  imputed_data <- ifelse(is.na(data), previous_mean, data)
  
  # M-step: Update mean estimate using imputed data
  new_mean <- mean(imputed_data, na.rm = TRUE)
  
  # Check for convergence
  if (abs(new_mean - previous_mean) < convergence_threshold) {
    break
  }
  
  previous_mean <- new_mean
  iteration <- iteration + 1
}


# Calculate unbiased estimates of the sample mean variance

# Results
results_list <- list(
  'Minimum Variance unbiased estimate of the sample Mean' = MVUESM <- new_mean,
  'Sample variance'= sample_variance <- sum((data - estimate_mean)^2, na.rm = TRUE) / (length(data) - 1),
  'Unbiased Estimate of the Variance of the Sample_Mean'= UEVSM<- sample_variance / length(data),
  'Number of Iterations' = iterations <- iteration,
  'Data with Missing Values' = missing_value <-imputed_data
)

print(results_list)

