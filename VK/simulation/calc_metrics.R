# Load necessary libraries
library(dplyr)
library(lavaan)
library(purrr)

# Function to run analysis using SEM or SAM
run_analysis <- function(data, model_syntax, method = "SEM") {
  if (method == "SEM") {
    fit <- sem(model_syntax, data = as.data.frame(data))
  } else if (method == "gSAM") {
    fit <- sam(model_syntax, data = as.data.frame(data), sam.method = "global")
  } else if (method == "lSAM_ML") {
    fit <- sam(model_syntax, data = as.data.frame(data), sam.method = "local", struc.args = list(estimator = "ML"))
  } else if (method == "lSAM_ULS") { 
    fit <- sam(model_syntax, data = as.data.frame(data), sam.method = "local", struc.args = list(estimator = "ULS"))
  } else {
    stop("Unknown method specified")
  }
  return(fit)
}

# Function to fit model to population matrix
run_sanity_check <- function(model_type, model_syntax) {
  popmodel <- gen_pop_model_syntax(gen_mat(model_type = model_type))
  fit0 <- sem(model = popmodel, do.fit = FALSE)
  COV <- inspect(fit0, what = "implied")$cov[,]
  sanity_check_fit <- sem(sample.cov = COV, model = model_syntax, sample.nobs = 10^6)
  sanity_check_estimates <- coef(sanity_check_fit)
  names(sanity_check_estimates) <- paste0(names(sanity_check_estimates), "_pop")
  return(sanity_check_estimates)
}

# Function to check the sanity check results
check_sanity <- function(sanity_check_estimates, true_values) {
  comparison <- compare_sanity_check(sanity_check_estimates, true_values)
  max_difference <- max(comparison$Differences$Difference, na.rm = TRUE)
  return(list(
    MaxDifference = max_difference,
    Alarm = comparison$Alarm
  ))
}

# Function to compare sanity check estimates with true values
compare_sanity_check <- function(sanity_check_estimates, true_values, threshold = 0.1) {
  true_values_flat <- unlist(true_values)
  
  # Align true values with the sanity check estimates
  common_params <- intersect(names(sanity_check_estimates), names(true_values_flat))
  aligned_true_values <- true_values_flat[common_params]
  aligned_sanity_check_estimates <- sanity_check_estimates[common_params]
  
  differences <- abs(aligned_sanity_check_estimates - aligned_true_values)
  differences_df <- data.frame(
    Parameter = names(differences),
    TrueValue = aligned_true_values,
    SanityCheckEstimate = aligned_sanity_check_estimates,
    Difference = differences
  )
  
  # Check if any difference exceeds the threshold
  alarm <- any(differences > threshold)
  
  return(list(
    Differences = differences_df,
    Alarm = alarm
  ))
}

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
