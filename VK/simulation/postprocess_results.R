library(dplyr)
library(tidyr)
library(kableExtra)
library(ggplot2)

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
      MeanMaxSanityCheckDifference = mean(MaxSanityCheckDifference[ImproperSolution == FALSE], na.rm = TRUE),
      SanityCheckAlarmCount = sum(SanityCheckAlarm[ImproperSolution == FALSE], na.rm = TRUE),
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
  # Group and summarize the results
  summary <- detailed_results %>%
    group_by(model_type, N, reliability, R_squared, method, b) %>%
    summarise(
      ConvergenceRate = mean(Converged[ImproperSolution == FALSE]),
      NonConvergenceCount = sum(!Converged[ImproperSolution == FALSE]),
      n_converged = sum(Converged[ImproperSolution == FALSE]),
      MeanMaxSanityCheckDifference = mean(MaxSanityCheckDifference[ImproperSolution == FALSE], na.rm = TRUE),
      SanityCheckAlarmCount = sum(SanityCheckAlarm[ImproperSolution == FALSE], na.rm = TRUE),
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
    arrange(model_type, N, reliability, R_squared, method, b)
  
  return(summary)
}

# ----------------- Study 1 ---------------------
study_1 <- readRDS("../simulation/results/simulation_results_study1.rda")

detailed_results_1 <- study_1$DetailedResults
summary_1 <- calculate_summary_study1(detailed_results_1)
saveRDS(summary_1, file = "../simulation/results/summary_study1.rds")

# ----------------- Study 2 ---------------------
study_2 <- readRDS("../simulation/results/simulation_results_study2.rda")
detailed_results_2 <- study_2$DetailedResults
summary_2 <- calculate_summary_study2(detailed_results_2)
saveRDS(summary_2, file = "../simulation/results/summary_study2.rds")


# unique_warnings <- study_1$DetailedResults %>%
#   filter(method != "SEM") %>%
#   select(Warnings) %>%
#   distinct()
# 
# unique_warnings
