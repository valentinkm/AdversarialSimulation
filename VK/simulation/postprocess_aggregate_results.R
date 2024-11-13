# -------------------- Performance Metrics Postprocessing ----------------------

# ------------------------------ Aggregated ------------------------------------

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

# Common function to calculate summary statistics for Study 1
calculate_summary_study1 <- function(detailed_results) {
  # Group and summarize the results
  summary <- detailed_results %>%
    group_by(model_type, N, reliability, method) %>%
    summarise(
      ConvergenceRate = mean(Converged[ImproperSolution == FALSE]),
      NonConvergenceCount = sum(!Converged[ImproperSolution == FALSE]),
      n_converged = sum(Converged[ImproperSolution == FALSE]),
      MeanCoverage = mean(Coverage[ImproperSolution == FALSE], na.rm = TRUE),
      MeanRelativeBias = mean(RelativeBias[ImproperSolution == FALSE], na.rm = TRUE),
      MeanRelativeRMSE = mean(RelativeRMSE[ImproperSolution == FALSE], na.rm = TRUE),
      MCSE_RelativeBias = calculate_mcse_bias(RelativeBias[ImproperSolution == FALSE]),
      MCSE_RelativeRMSE = calculate_mcse_rmse(RelativeRMSE[ImproperSolution == FALSE]),
      CI_RelativeBias_Lower = mean(RelativeBias[ImproperSolution == FALSE], na.rm = TRUE) - 1.96 * calculate_mcse_bias(RelativeBias[ImproperSolution == FALSE]),
      CI_RelativeBias_Upper = mean(RelativeBias[ImproperSolution == FALSE], na.rm = TRUE) + 1.96 * calculate_mcse_bias(RelativeBias[ImproperSolution == FALSE]),
      CI_RMSE_Lower = mean(RelativeRMSE[ImproperSolution == FALSE], na.rm = TRUE) - 1.96 * calculate_mcse_rmse(RelativeRMSE[ImproperSolution == FALSE]),
      CI_RMSE_Upper = mean(RelativeRMSE[ImproperSolution == FALSE], na.rm = TRUE) + 1.96 * calculate_mcse_rmse(RelativeRMSE[ImproperSolution == FALSE]),
      ImproperSolutionsCount = sum(ImproperSolution),
      .groups = 'drop'
    ) %>%
    arrange(model_type, N, reliability, method)
  
  return(summary)
}

# Common function to calculate summary statistics for Study 2
calculate_summary_study2 <- function(detailed_results) {
  # Define the paths in the estimation model
  estimation_model_paths <- c("f3~f1", "f3~f2", "f4~f1", "f4~f2", "f4~f3", "f5~f3", "f5~f4", "f5~f2")
  
  # Define the misspecified paths
  misspecified_paths <- c("f3~f1", "f4~f3", "f3~f2")
  
  # Process each row
  processed_results <- detailed_results %>%
    rowwise() %>%
    mutate(
      true_values = list(get_true_values(model_type, R_squared)),
      processed_data = list(
        tibble(
          parameter = estimation_model_paths,
          true_value = ifelse(parameter %in% names(true_values), true_values[parameter], 0),
          estimated_value = ifelse(parameter %in% names(EstimatedPaths), EstimatedPaths[parameter], NA)
        ) %>%
          filter(!parameter %in% misspecified_paths) %>%  # Exclude misspecified paths
          mutate(
            relative_bias = (estimated_value - true_value) / pmax(abs(true_value), 1e-6),
            relative_rmse = sqrt((estimated_value - true_value)^2) / pmax(abs(true_value), 1e-6)
          )
      )
    ) %>%
    unnest(processed_data)
  
  # Now calculate summary statistics
  summary <- processed_results %>%
    group_by(model_type, N, reliability, R_squared, method, b) %>%
    summarise(
      ConvergenceRate = mean(Converged[ImproperSolution == FALSE]),
      NonConvergenceCount = sum(!Converged[ImproperSolution == FALSE]),
      n_converged = sum(Converged[ImproperSolution == FALSE]),
      MeanCoverage = mean(Coverage[ImproperSolution == FALSE], na.rm = TRUE),
      MeanRelativeBias = mean(relative_bias[ImproperSolution == FALSE], na.rm = TRUE),
      MeanRelativeRMSE = mean(relative_rmse[ImproperSolution == FALSE], na.rm = TRUE),
      MCSE_RelativeBias = calculate_mcse_bias(relative_bias[ImproperSolution == FALSE]),
      MCSE_RelativeRMSE = calculate_mcse_rmse(relative_rmse[ImproperSolution == FALSE]),
      CI_RelativeBias_Lower = mean(relative_bias[ImproperSolution == FALSE], na.rm = TRUE) - 1.96 * calculate_mcse_bias(relative_bias[ImproperSolution == FALSE]),
      CI_RelativeBias_Upper = mean(relative_bias[ImproperSolution == FALSE], na.rm = TRUE) + 1.96 * calculate_mcse_bias(relative_bias[ImproperSolution == FALSE]),
      CI_RMSE_Lower = mean(relative_rmse[ImproperSolution == FALSE], na.rm = TRUE) - 1.96 * calculate_mcse_rmse(relative_rmse[ImproperSolution == FALSE]),
      CI_RMSE_Upper = mean(relative_rmse[ImproperSolution == FALSE], na.rm = TRUE) + 1.96 * calculate_mcse_rmse(relative_rmse[ImproperSolution == FALSE]),
      ImproperSolutionsCount = sum(ImproperSolution),
      .groups = 'drop'
    ) %>%
    arrange(model_type, N, reliability, R_squared, method, b)
  
  return(summary)
}

# ----------------- Study 1 ---------------------
summary_study_1 <- calculate_summary_study1(detailed_results_1_filtered)
saveRDS(summary_study_1, file = "../simulation/results/summary_study1.rds")

# ----------------- Study 2 ---------------------
summary_study_2 <- calculate_summary_study2(detailed_results_2_filtered)
saveRDS(summary_study_2, file = "../simulation/results/summary_study2.rds")

# ----------------- Study 3 ---------------------
summary_study_3 <- calculate_summary_study1(detailed_results_3_filtered)
saveRDS(summary_study_3, file = "../simulation/results/summary_study3.rds")
