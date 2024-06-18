
# Study 1b

#Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim1b_results_raw.rds")
# Extract results

extract_results <- function(results_df_raw) {
  results_raw_combined <- results_df_raw %>%
    group_by(N, lambda, phi) %>%
    summarise(across(c(LSAM_ML, LSAM_ULS), ~list(unlist(.))), .groups = "drop")
  
  # Compute performance measures for each estimator
  results_metrics <- function(values, phi) {
    list(
      abs_bias = mean(abs(values - phi)),  
      rmse = sqrt(mean((values - phi)^2)),
      se_bias = sd(abs(values - phi)) / sqrt(length(values)),
      ci_lower = mean(abs(values - phi)) - qt(0.975, df = length(values) - 1) * sd(abs(values - phi)) / sqrt(length(values)), #correct calculation
      ci_upper = mean(abs(values - phi)) + qt(0.975, df = length(values) - 1) * sd(abs(values - phi)) / sqrt(length(values)) #correct calculation
      
    )
  }
  
  # Apply the performance metrics function to each estimator and create metrics columns
  metrics_list <- results_raw_combined %>%
    mutate(across(c(LSAM_ML, LSAM_ULS), 
                  ~map2(.x, phi, results_metrics), .names = "{.col}_metrics")) %>%
    select(-c(LSAM_ML, LSAM_ULS))  # Drop the original estimator columns
  
  return(metrics_list)
}

metrics_list <- extract_results(results_df_raw)

# Report Bias

report_bias <- function(metrics_list) {
  # Define the list of estimators
  estimators <- c("LSAM_ML_metrics", "LSAM_ULS_metrics")
  clean_estimators <- sub("_metrics", "", estimators)
  
  # Ensure unique values for N, lambda, and phi
  unique_ns <- unique(metrics_list$N)
  unique_lambdas <- unique(metrics_list$lambda)
  unique_phis <- unique(metrics_list$phi)
  
  # Process each N
  results_by_n <- map(set_names(unique_ns), ~{
    n_val <- .x
    
    # Create a list to hold results for each estimator
    estimator_results <- map(estimators, function(estimator) {
      
      # Filter data for the current N and estimator
      filtered_data <- metrics_list %>% filter(N == n_val)
      
      # Create a list to hold results for each lambda value
      lambda_results <- map(unique_lambdas, function(lambda_val) {
        filtered_lambda_data <- filtered_data %>% filter(lambda == lambda_val)
        
        # Create a list to hold results for each phi value
        phi_results <- map_dfc(unique_phis, function(phi_val) {
          metrics <- filtered_lambda_data %>% filter(phi == phi_val) %>% pull(estimator)
          metrics <- metrics[[1]]
          formatted_bias <- sprintf("%.3f [%.3f, %.3f]", metrics$abs_bias, metrics$ci_lower, metrics$ci_upper)
          set_names(formatted_bias, paste("phi", phi_val, sep = "_"))
        })
        
        tibble(lambda = lambda_val) %>% bind_cols(phi_results)
      })
      
      lambda_results <- bind_rows(lambda_results)
      set_names(lambda_results, c("lambda", paste("phi", unique_phis, sep = "_")))
    })
    
    set_names(estimator_results, clean_estimators)
  }, .options = furrr_options(seed = TRUE))  # Ensure reproducibility with seeds
  
  # Combine results by N and estimator
  results_by_n_estimator <- map(unique_ns, function(n_val) {
    map(clean_estimators, function(estimator) {
      results_by_n[[paste(n_val)]][[estimator]]
    })
  })
  
  # Set names for easier access
  names(results_by_n_estimator) <- paste("N", unique_ns, sep = "_")
  results_by_n_estimator <- map(results_by_n_estimator, function(n_results) {
    set_names(n_results, clean_estimators)
  })
  
  results_by_n_estimator
}

#Apply changed function
metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "LK/SimulationResultsProcessed/sim1b_metrics_list.rds")

### Report Bias
bias_ci <- suppressMessages(report_bias(metrics_list)) #get rid of annoying "new names" messages
saveRDS(bias_ci, file = "LK/SimulationResultsProcessed/sim1b_abs_bias_ci.rds")
