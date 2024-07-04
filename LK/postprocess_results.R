# Specify the libraries to load
libraries <- c("lavaan", "purrr", "tidyverse", "furrr")
# Set the R mirror to the cloud mirror of RStudio
options(repos = "https://cloud.r-project.org/")

# Load the libraries
for (library_name in libraries) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name)
    library(library_name, character.only = TRUE)
  }
}

# Study 1

#Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim1_results_raw.rds")

#Set same fixed value for phi (necessary for computation)
phi <- 0.60

# Extract results

extract_results <- function(results_df_raw){
  #Compute performance measures
  results_metrics <- results_df_raw %>%
    group_by(N, rc, psi_value) %>%
    summarize(across(everything(),
                     list(
                       abs_bias = ~mean(abs(.x - phi)),              # Average absolute bias
                       rel_bias = ~mean(.x - phi) / phi,             # Relative bias
                       sd = ~sd(.x),                                 # Standard deviation of estimates
                       rmse = ~sqrt(mean((.x - phi)^2)),             # Root mean square error
                       se_bias = ~(sd(.x - phi))/ sqrt(unique(N)),      # SE of bias
                       ci_lower = ~((mean(.x - phi) / phi) - qt(0.975, df = unique(N) - 1) * (sd((.x - phi) / phi)) / sqrt(unique(N))),
                       ci_upper = ~((mean(.x - phi) / phi) + qt(0.975, df = unique(N) - 1) * (sd((.x - phi) / phi)) / sqrt(unique(N)))
                     )),  .groups = 'drop')
  
  # Split metrics by rc and psi_value into separate lists
  split_metrics <- results_metrics %>%
    group_by(rc, psi_value) %>%
    group_split() %>%
    set_names(map(., ~paste(unique(.x$rc), unique(.x$psi_value), sep="_")))
  
  # Define a function to transform each group into the desired format
  transform_group <- function(df_group) {
    df_group <- df_group %>%
      select(-rc, -psi_value) %>%
      pivot_longer(cols = starts_with(c("SEM_","LSAM_","GSAM_")), names_to = "method_metric", values_to = "value")
    
    # Creating nested lists for each metric
    list(
      abs_bias = df_group %>% filter(str_detect(method_metric, "abs_bias")) %>%
        pivot_wider(names_from = N, values_from = value),
      rel_bias = df_group %>% filter(str_detect(method_metric, "rel_bias")) %>%
        pivot_wider(names_from = N, values_from = value),
      sd = df_group %>% filter(str_detect(method_metric, "sd")) %>%
        pivot_wider(names_from = N, values_from = value),
      rmse = df_group %>% filter(str_detect(method_metric, "rmse")) %>%
        pivot_wider(names_from = N, values_from = value),
      se_bias = df_group %>% filter(str_detect(method_metric, "se_bias")) %>%
        pivot_wider(names_from = N, values_from = value),
      ci_lower = df_group %>% filter(str_detect(method_metric, "ci_lower")) %>%
        pivot_wider(names_from = N, values_from = value),
      ci_upper = df_group %>% filter(str_detect(method_metric, "ci_upper")) %>%
        pivot_wider(names_from = N, values_from = value)
    )
  }
  
  # Apply the transformation to each group and store the results
  metrics_list <- map(split_metrics, transform_group)
  
  return(metrics_list)
}


# Report Bias

report_bias <- function(metrics_list) {
  # Define a list to store results
  bias_list <- list()
  
  # Iterate over each condition in metrics_list
  for (condition in names(metrics_list)) {
    # Extract rel_bias for the current condition
    rel_bias <- metrics_list[[condition]]$rel_bias
    
    # Create the bias table for the current condition and store it in the list
    bias_list[[condition]] <- rel_bias %>%
      mutate(across(`50`:`1e+05`, ~sprintf("%.3f", .)))
  }
  
  return(bias_list)
}


#Apply changed function
metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "LK/SimulationResultsProcessed/sim1_metrics_list.rds")

### Report Bias
bias_ci <- report_bias(metrics_list)
saveRDS(bias_ci, file = "LK/SimulationResultsProcessed/sim1_rel_bias_ci.rds")



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
          formatted_bias <- sprintf("%.3f", metrics$abs_bias)
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

# Study 2

## Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim2_results_raw.rds")

#Set same fixed value for phi (necessary for computation)
phi <- 0.60

# Extract results

extract_results <- function(results_df_raw){
  #Compute performance measures
  results_metrics <- results_df_raw %>%
    group_by(N, cl, delta_value) %>%
    summarize(across(everything(),
                     list(
                       abs_bias = ~mean(abs(.x - phi)),              # Average absolute bias
                       rel_bias = ~mean(.x - phi) / phi,             # Relative bias
                       sd = ~sd(.x),                                 # Standard deviation of estimates
                       rmse = ~sqrt(mean((.x - phi)^2)),             # Root mean square error
                       se_bias = ~(sd(.x - phi))/ sqrt(unique(N)),      # SE of bias
                       ci_lower = ~((mean(.x - phi) / phi) - qt(0.975, df = unique(N) - 1) * (sd((.x - phi) / phi)) / sqrt(unique(N))),
                       ci_upper = ~((mean(.x - phi) / phi) + qt(0.975, df = unique(N) - 1) * (sd((.x - phi) / phi)) / sqrt(unique(N)))
                     )),  .groups = 'drop')
  
  # Split metrics by cl and delta_value into seperate lists
  split_metrics <- results_metrics %>%
    group_by(cl, delta_value) %>%
    group_split() %>%
    set_names(map(., ~ paste(unique(.x$cl), unique(.x$delta_value), sep = "_")))
  
  # Define a function to transform each group into the desired format
  transform_group <- function(df_group) {
    df_group <- df_group %>%
      select(-cl, -delta_value) %>%
      pivot_longer(cols = starts_with(c("SEM_","LSAM_","GSAM_")), names_to = "method_metric", values_to = "value") 
    
    # Creating nested lists for each metric
    list(
      abs_bias = df_group %>% filter(str_detect(method_metric, "abs_bias")) %>%
        pivot_wider(names_from = N, values_from = value),
      rel_bias = df_group %>% filter(str_detect(method_metric, "rel_bias")) %>%
        pivot_wider(names_from = N, values_from = value),
      sd = df_group %>% filter(str_detect(method_metric, "sd")) %>%
        pivot_wider(names_from = N, values_from = value),
      rmse = df_group %>% filter(str_detect(method_metric, "rmse")) %>%
        pivot_wider(names_from = N, values_from = value),
      se_bias = df_group %>% filter(str_detect(method_metric, "se_bias")) %>%
        pivot_wider(names_from = N, values_from = value),
      ci_lower = df_group %>% filter(str_detect(method_metric, "ci_lower")) %>%
        pivot_wider(names_from = N, values_from = value),
      ci_upper = df_group %>% filter(str_detect(method_metric, "ci_upper")) %>%
        pivot_wider(names_from = N, values_from = value)
    )
  }
  
  # Apply the transformation to each group and store the results
  metrics_list <- map(split_metrics, transform_group)
  
  return(metrics_list)
}


# Report Bias
### Same as study 1

report_bias <- function(metrics_list) {
  # Define a list to store results
  bias_list <- list()
  
  # Iterate over each condition in metrics_list
  for (condition in names(metrics_list)) {
    # Extract rel_bias for the current condition
    rel_bias <- metrics_list[[condition]]$rel_bias
    
    # Create the bias table for the current condition and store it in the list
    bias_list[[condition]] <- rel_bias %>%
      mutate(across(`50`:`1e+05`, ~sprintf("%.3f", .)))
  }
  
  return(bias_list)
}

#Apply changed function
metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "LK/SimulationResultsProcessed/sim2_metrics_list.rds")

### Report Bias
bias_ci <- report_bias(metrics_list)
saveRDS(bias_ci, file = "LK/SimulationResultsProcessed/sim2_rel_bias_ci.rds")


# Study 3

## Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim3_results_raw.rds")

#Set fixed value (necessary for calculation)
phi <- 0.60 

# Extract results

extract_results <- function(results_df_raw){
  #Compute performance measures
  results_metrics <- results_df_raw %>%
    group_by(N) %>%
    summarize(across(everything(),
                     list(
                       abs_bias = ~mean(abs(.x - phi)),              # Average absolute bias
                       rel_bias = ~mean(.x - phi) / phi,             # Relative bias
                       sd = ~sd(.x),                                 # Standard deviation of estimates
                       rmse = ~sqrt(mean((.x - phi)^2)),             # Root mean square error
                       se_bias = ~(sd(.x - phi))/ sqrt(unique(N)),      # SE of bias
                       ci_lower = ~((mean(.x - phi) / phi) - qt(0.975, df = unique(N) - 1) * (sd((.x - phi) / phi))/ sqrt(unique(N))),
                       ci_upper = ~((mean(.x - phi) / phi) + qt(0.975, df = unique(N) - 1) * (sd((.x - phi) / phi))/ sqrt(unique(N)))
                     )),  .groups = 'drop')
  
  
  # transform each group into the desired format
  transform_group <- results_metrics %>%
    pivot_longer(cols = starts_with(c("SEM_","LSAM_","GSAM_")), values_to = "value")
  
  # Creating nested lists for each metric
  metrics_list <-  list(
    abs_bias = transform_group %>% filter(str_detect(name, "abs_bias")) %>%
      pivot_wider(names_from = N, values_from = value),
    rel_bias = transform_group %>% filter(str_detect(name, "rel_bias")) %>%
      pivot_wider(names_from = N, values_from = value),
    sd = transform_group %>% filter(str_detect(name, "sd")) %>%
      pivot_wider(names_from = N, values_from = value),
    rmse = transform_group %>% filter(str_detect(name, "rmse")) %>%
      pivot_wider(names_from = N, values_from = value),
    se_bias = transform_group %>% filter(str_detect(name, "se_bias")) %>%
      pivot_wider(names_from = N, values_from = value),
    ci_lower = transform_group %>% filter(str_detect(name, "ci_lower")) %>%
      pivot_wider(names_from = N, values_from = value),
    ci_upper = transform_group %>% filter(str_detect(name, "ci_upper")) %>%
      pivot_wider(names_from = N, values_from = value)
  )
  
  return(metrics_list)
}


# Report bias

report_bias <- function(metrics_list) {
  # Define a list to store results
  bias_ci <- list()
  
  # Extract rel_bias
  rel_bias <- metrics_list$rel_bias
  
  # Create the bias_ci table and store it in the list
  bias_ci <- rel_bias %>%
    mutate(across(`50`:`1e+05`, ~sprintf("%.3f", .)))
  
  return(bias_ci)
}

#Apply changed function
metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "LK/SimulationResultsProcessed/sim3_metrics_list.rds")

### Report Bias
bias_ci <- report_bias(metrics_list)
saveRDS(bias_ci, file = "LK/SimulationResultsProcessed/sim3_rel_bias_ci.rds")


# Study 4

## Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim4_results_raw.rds")

# Apply fixed value (necessary for calculations)
phi <- 0.1

# Extract results

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
      se_bias = sd(abs(values - phi)) / sqrt(length(values)),
      ci_lower = mean(abs(values - phi)) - qt(0.975, df = length(values) - 1) * sd(abs(values - phi)) / sqrt(length(values)),
      ci_upper = mean(abs(values - phi)) + qt(0.975, df = length(values) - 1) * sd(abs(values - phi)) / sqrt(length(values))
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


# Study 4a

## Load raw results

results_df_raw <- readRDS("LK/SimulationResults/sim4a_results_raw.rds")

# Extract results

extract_results <- function(results_df_raw) {
  
  results_raw_combined <- results_df_raw %>%
    group_by(N, DGM, beta) %>%
    summarise(across(c(SEM_ML, SEM_ULS, LSAM_ML), 
                     ~list(unlist(.))), 
              .groups = "drop")
  
  # Compute performance measures for each estimator
  results_metrics <- function(values, beta) {
    list(
      abs_bias = mean(abs(values - beta)),  
      rel_bias = mean(values - beta) / beta,
      rmse = sqrt(mean(values - beta^2)),
      se_bias = sd(abs(values - beta)) / sqrt(length(values)),
      ci_lower = mean(abs(values - beta)) - qt(0.975, df = length(values) - 1) * sd(abs(values - beta)) / sqrt(length(values)),
      ci_upper = mean(abs(values - beta)) + qt(0.975, df = length(values) - 1) * sd(abs(values - beta)) / sqrt(length(values))
    )
  }
  
  # Apply the performance metrics function to each estimator and create metrics columns
  metrics_list <- results_raw_combined %>%
    mutate(across(c(SEM_ML, SEM_ULS, LSAM_ML), 
                  ~map2(.x, beta, results_metrics), .names = "{.col}_metrics")) %>%
    select(-c(SEM_ML, SEM_ULS, LSAM_ML))  # Drop the original estimator columns
  
  return(metrics_list)
}


# Report Bias

report_bias <- function(metrics_list) {
  # Define the list of estimators
  estimators <- c("SEM_ML_metrics", "SEM_ULS_metrics", "LSAM_ML_metrics")
  
  # Ensure DGM values are uniquely identified
  unique_dgms <- unique(metrics_list$DGM)
  unique_betas <- unique(metrics_list$beta)
  unique_ns <- unique(metrics_list$N)
  
  # Process each DGM
  results_by_dgm <- map(set_names(unique_dgms), ~{
    dgm <- .x
    
    # Create a list to hold results for each estimator
    estimator_results <- map(estimators, function(estimator) {
      
      # Filter data for the current DGM and estimator
      filtered_data <- metrics_list %>% filter(DGM == dgm)
      
      # Create a list to hold results for each beta value
      beta_results <- map(unique_betas, function(beta_val) {
        filtered_beta_data <- filtered_data %>% filter(beta == beta_val)
        
        # Create a list to hold results for each N
        n_results <- map_dfc(unique_ns, function(n_val) {
          metrics <- filtered_beta_data %>% filter(N == n_val) %>% pull(estimator)
          metrics <- metrics[[1]]
          formatted_bias <- sprintf("%.3f [%.3f, %.3f]", metrics$abs_bias, metrics$ci_lower, metrics$ci_upper)
          set_names(formatted_bias, paste("N", n_val, sep = "_"))
        })
        
        tibble(beta = beta_val) %>% bind_cols(n_results)
      })
      
      beta_results <- bind_rows(beta_results)
      set_names(beta_results, c("beta", paste("N", unique_ns, sep = "_")))
    })
    
    set_names(estimator_results, estimators)
  }, .options = furrr_options(seed = TRUE))  # Ensure reproducibility with seeds
  
  # Combine results by DGM and estimator
  results_by_dgm_estimator <- map(unique_dgms, function(dgm) {
    map(estimators, function(estimator) {
      results_by_dgm[[paste(dgm)]][[estimator]]
    })
  })
  
  # Set names for easier access
  names(results_by_dgm_estimator) <- paste("DGM", unique_dgms, sep = "_")
  results_by_dgm_estimator <- map(results_by_dgm_estimator, function(dgm_results) {
    set_names(dgm_results, estimators)
  })
  
  results_by_dgm_estimator
}


#Apply changed function
metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "LK/SimulationResultsProcessed/sim4a_metrics_list.rds")

### Report Bias
bias_ci <- suppressMessages(report_bias(metrics_list)) #get rid of annoying "new names" messages
saveRDS(bias_ci, file = "LK/SimulationResultsProcessed/sim4a_abs_bias_ci.rds")


