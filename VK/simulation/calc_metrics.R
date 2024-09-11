# Load necessary libraries
library(dplyr)
library(lavaan)
library(purrr)


# Function to calculate coverage of confidence intervals
calculate_coverage <- function(fit, true_values) {
  if (is.null(fit) || !lavInspect(fit, "converged")) {
    return(NA)
  }
  
  # Extract parameter estimates with confidence intervals
  param_estimates <- parameterEstimates(fit)
  
  # Filter for regression paths (op == "~")
  param_estimates <- param_estimates %>%
    filter(op == "~")
  
  # Calculate coverage for each parameter in true_values$B
  coverage <- map_dbl(names(true_values$B), function(param) {
    parts <- unlist(strsplit(param, "~"))
    row <- param_estimates %>%
      filter(lhs == parts[1], rhs == parts[2])
    if (nrow(row) > 0) {
      row$ci.lower <= true_values$B[param] && row$ci.upper >= true_values$B[param]
    } else {
      NA
    }
  })
  
  # Return mean coverage across all parameters
  mean(coverage, na.rm = TRUE)
}

# Function to calculate empirical relative bias
calculate_relative_bias <- function(estimated_paths, true_values) {
  if (all(is.na(estimated_paths))) {
    return(NA)
  }
  
  # Align true values with estimated paths
  common_params <- intersect(names(estimated_paths), names(true_values$B))
  aligned_true_values <- true_values$B[common_params]
  aligned_estimated_paths <- estimated_paths[common_params]
  
  bias <- (aligned_estimated_paths - aligned_true_values) / aligned_true_values
  mean(bias, na.rm = TRUE)
}

# Function to calculate empirical relative RMSE
calculate_relative_rmse <- function(estimated_paths, true_values) {
  if (all(is.na(estimated_paths))) {
    return(NA)
  }
  
  # Align true values with estimated paths
  common_params <- intersect(names(estimated_paths), names(true_values$B))
  aligned_true_values <- true_values$B[common_params]
  aligned_estimated_paths <- estimated_paths[common_params]
  
  rmse <- sqrt(mean((aligned_estimated_paths - aligned_true_values)^2, na.rm = TRUE)) / mean(aligned_true_values)
  rmse
}

# Function to calculate Monte Carlo Standard Error for Bias
calculate_mcse_bias <- function(bias_list) {
  bias_list <- na.omit(bias_list)  # Remove NAs
  if (length(bias_list) == 0) {
    return(NA)
  }
  K <- length(bias_list)
  S_T2 <- var(bias_list)  # This is already S_T^2
  sqrt(S_T2 / K)
}

# Function to calculate Monte Carlo Standard Error for RMSE
calculate_mcse_rmse <- function(rmse_list) {
  rmse_list <- na.omit(rmse_list)  # Remove NAs
  if (length(rmse_list) == 0) {
    return(NA)
  }
  K <- length(rmse_list)
  rmse_mean <- mean(rmse_list)
  sqrt(sum((rmse_list - rmse_mean)^2) / (K * (K - 1)))
}