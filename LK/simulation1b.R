# First script for Simulation Study 1b


set.seed(1)


# Packages

## Specify the libraries to load
libraries <- c("lavaan", "purrr", "tidyverse", "furrr")
## Set the R mirror to the cloud mirror of RStudio
options(repos = "https://cloud.r-project.org/")

## Load the libraries
for (library_name in libraries) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name)
    library(library_name, character.only = TRUE)
  }
}


# Specify 2-factor-Model

model <- "

#Structural part
    FX =~ l1*X1 + l2*X2 + l3*X3
    FY =~ l4*Y1 + l5*Y2 + l6*Y3
    FX ~~ 1*FX    #Fixed latent variance
    FY ~~ 1*FY    #Fixed latent variance
    FX ~~ phi*FY
    phi < 0.99    #Phi between -1 and 1
    phi > -0.99

#Measurement part    
    X1 ~~ vX1*X1
    X2 ~~ vX2*X2    
    X3 ~~ vX3*X3
    Y1 ~~ vY1*Y1
    Y2 ~~ vY2*Y2    
    Y3 ~~ vY3*Y3
    l1 > 0.01     #only positive loadings
    l2 > 0.01    
    l3 > 0.01
    l4 > 0.01
    l5 > 0.01
    l6 > 0.01       
    vX1 > 0.01    #only positive variances
    vX2 > 0.01    
    vX3 > 0.01
    vY1 > 0.01
    vY2 > 0.01    
    vY3 > 0.01
                #residual correlations
    X1 ~~ Y1
    X2 ~~ Y2
    "


# Setup and Design


setup_design <- function() {
  
  #N
  N_sizes <- c(50, 100)
  
  # Factor loadings
  lambda_values <- c(0.4, 0.5, 0.6, 0.7, 0.8)
  
  # Factor correlations
  phi_values <- c(0, 0.2, 0.4, 0.6, 0.8)
  
  # Expand grid to create a data frame of all combinations
  design <- expand.grid(N = N_sizes, 
                        lambda = lambda_values, 
                        phi = phi_values)
  
  return(design)
}




# Data generating Mechanism

## Fixed values
### None

## Varying values

get_dgm <- function(lambda, phi) {
  # Factor loadings for all six manifest variables set to the specified lambda
  LAM <- matrix(c(lambda, lambda, lambda, lambda, lambda, lambda), nrow = 6, ncol = 2, byrow = TRUE)
  LAM[1:3, 2] <- 0  # Setting off-diagonal elements to 0
  LAM[4:6, 1] <- 0  
  
  # Factor correlation matrix
  PHI <- matrix(c(1, phi, phi, 1), nrow = 2, ncol = 2)
  
  # Theta
  error_variances <- 1 - lambda^2  
  THETA <- diag(c(rep(error_variances, 6)))  
  
  # residual correlations are directly specified 
  THETA[1, 4] <- THETA[4, 1] <- 0.12  
  THETA[2, 5] <- THETA[5, 2] <- 0.12 

  MLIST <-list(LAM = LAM, PHI = PHI, THETA = THETA)
  return(MLIST)
}


# Apply Syntax

apply_syntax <- function(MLIST) {
  
  LAM <- MLIST$LAM
  PHI <- MLIST$PHI
  THETA <- MLIST$THETA
  
  pop.model <- paste(
"#Structural part",
    paste("FX =~", LAM[1,1], "*X1 +", LAM[2,1], "*X2 +", LAM[3,1], "*X3"),
    paste("FY =~", LAM[4,2], "*Y1 +", LAM[5,2], "*Y2 +", LAM[6,2], "*Y3"),
    "FX ~~ 1*FX",    
    "FY ~~ 1*FY",    
    paste("FX ~~", PHI[1,2], "*FY"),
"#Measurement part",    
    paste("X1 ~~", THETA[1,1], "*X1"),
    paste("X2 ~~", THETA[2,2], "*X2"),
    paste("X3 ~~", THETA[3,3], "*X3"),
    paste("Y1 ~~", THETA[4,4], "*Y1"),
    paste("Y2 ~~", THETA[5,5], "*Y2"),
    paste("Y3 ~~", THETA[6,6], "*Y3"),
"Correlated residuals",
    paste("X1 ~~", THETA[1,4], "*Y1"),
    paste("X2 ~~", THETA[2,5], "*Y2"),
    sep = "\n"
  ) 
 
  return(pop.model)
}




# Simulate data

simulate_data <- function(N, lambda, phi) {

  dgm_params <- get_dgm(lambda, phi)
  
  pop.model <- apply_syntax(dgm_params)
  
  df_dat <- simulateData(pop.model, sample.nobs = N)
  
  return(df_dat)
}



# Planned Analysis

#Specify estimation methods of interest

estimators <- list(
  LSAM_ML = \(d) lavaan::sam(model, data=d, sam.method="local", estimator = "ML", std.lv= TRUE),
  LSAM_ULS = \(d)lavaan::sam(model, data=d, sam.method="local", estimator = "ULS", std.lv= TRUE)
)
# postprocess each model output
estimators <- modify(estimators, ~compose(\(e)filter(e, label == "phi")$est, parameterEstimates, .))
# apply all estimators to the same dataset
apply_estimators <- \(d) map(estimators, exec, d)

planned_analysis <- compose(apply_estimators, simulate_data)


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
      se_bias = sd(values - phi) / sqrt(length(values)),
      ci_lower = mean(values - phi) - qt(0.975, df = length(values) - 1) * sd(values - phi) / sqrt(length(values)),
      ci_upper = mean(values - phi) + qt(0.975, df = length(values) - 1) * sd(values - phi) / sqrt(length(values))
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



# Simulation Study

simulation_study_ <- function(design){
  all_steps <- mutate(design, !!!future_pmap_dfr(design, planned_analysis, .options = furrr_options(seed = TRUE)))
  all_steps
}

simulation_study <- function(design, k, seed = NULL) {
  # Define a function to run simulation_study_() safely
  safe_simulation <- function(design) {
    # Run the simulation function safely
    result <- quietly(safely(simulation_study_))(design)
    
    # Extract the output, errors, and warnings: Question: On which level do we want this? Do obtain the correct DF, triple indexing into safely() is  necessary.
    output <- if (!is.null(result$error)) NULL else result$result$result
    errors <- if (!is.null(result$error)) result$error else NULL
    warnings <- if (!is.null(result$warning)) result$warning else NULL
    messages <- result$message
    
    # Return a list with the output, errors, and warnings
    list(result = output, errors = errors, warnings = warnings, messages = messages)
  }
  
  # Run simulation_study_() k times, capturing errors and warnings
  results_list <- future_map(seq_len(k), ~safe_simulation(design), .options = furrr_options(seed = seed))
  
  # Extract result, errors, and warnings from the list
  results <- map_df(results_list, pluck, "result")
  errors <- map(results_list, pluck, "errors")
  warnings <- map(results_list, pluck, "warnings")
  messages <- map(results_list, pluck, "messages")
  
  # Combine results, errors, and warnings into a single data frame

  return(list(results = results, errors = errors, warnings = warnings, messages = messages))
}



# Run & safe simulation

### Set up design
design <- setup_design()

### Run & safe simulation
results_sim <- simulation_study(design, 1500, seed = TRUE)
saveRDS(results_sim, file = "sim1b_results_error.rds")

### Errors, warnings and messages?
errors <- results_sim$errors
warnings <- results_sim$warnings
messages <- results_sim$messages

### Output and extract results
results_df_raw <- results_sim$results
saveRDS(results_df_raw, file = "sim1b_results_raw.rds")

metrics_list <- extract_results(results_df_raw)
saveRDS(metrics_list, file = "sim1b_metrics_list.rds")

### Report Bias
bias_ci <- suppressMessages(report_bias(metrics_list)) #get rid of annoying "new names" messages
saveRDS(bias_ci, file = "sim1b_abs_bias_ci.rds")

