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

# Study 4

## Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim4_results_raw.rds")

# Extract results
phi <- 0.1

extract_results <- function(results_df_raw){
  
  results_raw_combined <- results_df_raw %>%
    group_by(N, DGM) %>%
    summarise(across(c(SEM_ML, SEM_ULS, LSAM_ML), 
                     ~list(unlist(.))), 
              .groups = "drop")
  
  #Compute performance measures
  results_metrics <- function(values) {
    list(
      abs_bias = mean(abs(values - phi)),
      rel_bias = mean(values - phi) / phi,
      rmse = sqrt(mean((values - phi)^2)),
      se_bias = sd(values - phi) / sqrt(length(values)),
      ci_lower = mean(values - phi) - qt(0.975, df = length(values) - 1) * sd(values - phi) / sqrt(length(values)),
      ci_upper = mean(values - phi) + qt(0.975, df = length(values) - 1) * sd(values - phi) / sqrt(length(values))
    )
  }
  
  metrics_list <- results_raw_combined %>%
    mutate(across(c(SEM_ML, SEM_ULS, LSAM_ML), ~map(.x, results_metrics), .names = "{.col}_metrics")) %>%
    select(-c(SEM_ML, SEM_ULS, LSAM_ML))  # Drop the original estimator columns
  
  return(metrics_list)
}




# Report Bias

report_bias <- function(metrics_list) {
  # Define the list of estimators
  estimators <- c("SEM_ML_metrics", "SEM_ULS_metrics", "LSAM_ML_metrics")
  
  # Ensure DGM values are uniquely identified
  unique_dgms <- unique(metrics_list$DGM)
  
  # Process each DGM
  results_by_dgm <- map(set_names(unique_dgms), ~{
    # Filter data for the current DGM
    filtered_data <- metrics_list %>% filter(DGM == .x)
    
    # Create a dataframe for each estimator and each N
    estimator_results <- map_dfr(estimators, ~{
      estimator_data <- filtered_data[[.x]]
      
      # Map each N to create formatted bias strings
      n_results <- map_dfc(set_names(unique(filtered_data$N)), ~{
        metrics <- estimator_data[[which(filtered_data$N == .x)]]
        formatted_bias <- sprintf("%.3f [%.3f, %.3f]", metrics$abs_bias, metrics$ci_lower, metrics$ci_upper)
        set_names(formatted_bias, paste("N", .x, sep = "_"))
      })
      
      # Add estimator names as the first column
      tibble(Estimator = sub("_metrics", "", .x)) %>% bind_cols(n_results)
    })
    
    # Name the dataframe with the current N value
    rename(estimator_results, set_names(names(estimator_results[-1]),paste("N", unique(filtered_data$N), sep = "_")))
  }, .options = furrr_options(seed = TRUE))  # Ensure reproducibility with seeds
  
  # Return a list of dataframes, one for each DGM
  set_names(results_by_dgm, paste("DGM", unique(metrics_list$DGM), sep = "_"))
}




#Apply changed function
metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "LK/SimulationResultsProcessed/sim4_metrics_list.rds")

### Report Bias
bias_ci <- suppressMessages(report_bias(metrics_list)) #get rid of annoying "new names" messages
saveRDS(bias_ci, file = "LK/SimulationResultsProcessed/sim4_abs_bias_ci.rds")



