# calc_matrics.R

# Load necessary libraries
library(dplyr)
library(lavaan)

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
  coverage <- sapply(names(true_values$B), function(param) {
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
  bias <- (estimated_paths - true_values$B) / true_values$B
  mean(bias, na.rm = TRUE)
}

# Function to calculate empirical relative RMSE
calculate_relative_rmse <- function(estimated_paths, true_values) {
  if (all(is.na(estimated_paths))) {
    return(NA)
  }
  rmse <- sqrt(mean((estimated_paths - true_values$B)^2, na.rm = TRUE)) / mean(true_values$B)
  rmse
}

# Update calc_metrics.R
# Function to calculate Monte Carlo Standard Error for Bias
calculate_mcse_bias <- function(bias_list) {
  bias_list <- na.omit(bias_list)  # Remove NAs
  if (length(bias_list) == 0) {
    return(NA)
  }
  sd(bias_list) / sqrt(length(bias_list))
}

# Function to calculate Monte Carlo Standard Error for RMSE
calculate_mcse_rmse <- function(rmse_list) {
  rmse_list <- na.omit(rmse_list)  # Remove NAs
  if (length(rmse_list) == 0) {
    return(NA)
  }
  sd(rmse_list) / sqrt(length(rmse_list))
}