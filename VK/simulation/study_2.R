# study_2.R

# Load necessary scripts and libraries
source("gen_pop_data.R")
source("calc_metrics.R")  # Ensure this file contains the updated metric calculation functions
source("run_analysis.R")  # Source the helper functions
library(furrr)
library(parallel)
library(dplyr)
library(purrr)
library(progress)
library(lavaan)

# Check if the sam function is available
if (!exists("sam")) {
  stop("The sam function is not available. Please ensure the lavaan package is correctly loaded.")
}

# Generate seeds
parallel_seeds <- function(n, seed = NULL) {
  if (is.null(seed))
    stop("seed must be provided.")
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  purrr::accumulate(seq_len(n - 1), function(s, x) parallel::nextRNGStream(s),
                    .init = .Random.seed)
}

# Generate parameters grid with seeds
n_reps <- 2
params <- expand.grid(
  model_type = c("2.1", "2.2_exo", "2.2_endo", "2.2_both"),
  N = c(100, 400, 6400),
  reliability = c(0.3, 0.5, 0.7),
  method = c("SEM", "gSAM", "lSAM_ML_5", "lSAM_ULS_5", "lSAM_ML_3", "lSAM_ULS_3"),
  rep = 1:n_reps
) %>%
  mutate(seed = rep(parallel_seeds(n_reps, seed = 42), length.out = n()))

# Debug: Print first few rows of params to check method names
print(head(params,100))

# Set population values
B_true <- c(
  'f3~f1' = 0.1, 'f3~f2' = 0.1, 'f3~f4' = 0.1,
  'f4~f1' = 0.1, 'f4~f2' = 0.1,
  'f5~f3' = 0.1, 'f5~f4' = 0.1
)
true_values <- list(
  B = B_true
)

model_syntax_study2 <- "
    f1 =~ y1 + y2 + y3
    f2 =~ y4 + y5 + y6
    f3 =~ y7 + y8 + y9
    f4 =~ y10 + y11 + y12
    f5 =~ y13 + y14 + y15
    
    f3 ~ f1 + f2 + f4
    f4 ~ f1 + f2
    f5 ~ f3 + f4 + f2 + f3 + f4
"

# Update the check_sanity function
# Function to check the sanity check results
check_sanity <- function(sanity_check_estimates, true_values) {
  comparison <- compare_sanity_check(sanity_check_estimates, true_values)
  
  print("Sanity check comparison:")
  print(comparison)
  
  if (is.null(comparison$Differences)) {
    max_difference <- NA
  } else if (any(!is.na(comparison$Differences$Difference))) {
    max_difference <- max(comparison$Differences$Difference, na.rm = TRUE)
  } else {
    warning("comparison$Differences$Difference contains only missing values")
    max_difference <- -Inf
  }
  return(list(
    MaxDifference = max_difference,
    Alarm = comparison$Alarm
  ))
}

# Function to compare sanity check estimates with true values
compare_sanity_check <- function(sanity_check_estimates, true_values, threshold = 0.1) {
  true_values_flat <- as.numeric(true_values$B)
  names(true_values_flat) <- names(true_values$B)
  
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


# Study function
run_study_2 <- function(params) {
  safe_quiet_run_analysis <- safely(quietly(run_analysis))
  
  # Progress bar setup
  pb <- progress_bar$new(
    format = "  Running [:bar] :percent in :elapsed, ETA: :eta",
    total = nrow(params), clear = FALSE, width = 60
  )
  
  print("true_values structure:")
  print(true_values)
  
  # Run the simulations and analysis in parallel
  results <- future_pmap(params, function(model_type, N, reliability, method, rep, seed) {
    pb$tick() # Update progress bar
    set.seed(seed)
    data <- gen_pop_model_data(model_type, N, reliability)$data
    
    # Ensure method is a character
    method <- as.character(method)
    
    # Check the exact method being used
    cat("Running analysis with method:", method, "\n")
    cat("Type of method variable:", typeof(method), "\n")
    cat("Exact method value:", method, "\n")
    
    fit_result <- safe_quiet_run_analysis(data, model_syntax_study2, method)
    
    # Debug: Print fit result warnings and errors
    if (!is.null(fit_result$result$warnings)) {
      cat("Warnings for method", method, ":", toString(fit_result$result$warnings), "\n")
    }
    if (!is.null(fit_result$error)) {
      cat("Error for method", method, ":", toString(fit_result$error$message), "\n")
    }
    
    sanity_check_estimates <- run_sanity_check(model_type, model_syntax_study2)
    sanity_check_results <- check_sanity(sanity_check_estimates, true_values)
    
    warnings_detected <- fit_result$result$warnings
    improper_solution <- any(grepl("some estimated ov variances are negative", warnings_detected))
    
    if (!is.null(fit_result$result$result) && lavInspect(fit_result$result$result, "converged")) {
      PT <- parTable(fit_result$result$result)
      estimated_paths <- PT[PT$op == "~", "est"]
      names(estimated_paths) <- paste0(PT[PT$op == "~", "lhs"], "~", PT[PT$op == "~", "rhs"])
      
      # Calculate performance metrics
      coverage <- calculate_coverage(fit_result$result$result, true_values)
      relative_bias <- calculate_relative_bias(estimated_paths, true_values)
      relative_rmse <- calculate_relative_rmse(estimated_paths, true_values)
      
      list(
        Converged = 1, NonConverged = 0,
        EstimatedPaths = list(estimated_paths),
        SanityCheck = list(sanity_check_estimates),
        MaxSanityCheckDifference = sanity_check_results$MaxDifference,
        SanityCheckAlarm = sanity_check_results$Alarm,
        Coverage = coverage,
        RelativeBias = relative_bias,
        RelativeRMSE = relative_rmse,
        RelativeBiasList = list(relative_bias),
        RelativeRMSEList = list(relative_rmse),
        ImproperSolution = improper_solution,
        Warnings = toString(fit_result$result$warnings),
        Messages = toString(fit_result$result$messages),
        Errors = if (is.null(fit_result$error)) NA_character_ else toString(fit_result$error$message)
      )
    } else {
      list(
        Converged = 0, NonConverged = 1,
        EstimatedPaths = list(setNames(rep(NA, length(true_values$B)), names(true_values$B))),
        SanityCheck = list(sanity_check_estimates),
        MaxSanityCheckDifference = sanity_check_results$MaxDifference,
        SanityCheckAlarm = sanity_check_results$Alarm,
        Coverage = NA,
        RelativeBias = NA,
        RelativeRMSE = NA,
        RelativeBiasList = list(NA),
        RelativeRMSEList = list(NA),
        ImproperSolution = improper_solution,
        Warnings = toString(fit_result$result$warnings),
        Messages = toString(fit_result$result$messages),
        Errors = if (is.null(fit_result$error)) NA_character_ else toString(fit_result$error$message)
      )
    }
  }, .options = furrr_options(seed = params$seed))
  
  # Create dataframe for results
  results_df <- params %>%
    mutate(
      Converged = map_dbl(results, ~ .x$Converged),
      NonConverged = map_dbl(results, ~ .x$NonConverged),
      Warnings = map_chr(results, ~ .x$Warnings),
      Messages = map_chr(results, ~ .x$Messages),
      Errors = map_chr(results, ~ .x$Errors),
      EstimatedPaths = map(results, ~ .x$EstimatedPaths[[1]]),
      SanityCheck = map(results, ~ .x$SanityCheck[[1]]),
      MaxSanityCheckDifference = map_dbl(results, ~ .x$MaxSanityCheckDifference),
      SanityCheckAlarm = map_lgl(results, ~ .x$SanityCheckAlarm),
      Coverage = map_dbl(results, ~ .x$Coverage),
      RelativeBias = map_dbl(results, ~ .x$RelativeBias),
      RelativeRMSE = map_dbl(results, ~ .x$RelativeRMSE),
      RelativeBiasList = map(results, ~ .x$RelativeBiasList[[1]]),
      RelativeRMSEList = map(results, ~ .x$RelativeRMSEList[[1]]),
      ImproperSolution = map_lgl(results, ~ .x$ImproperSolution)
    )
  
  # Remove NAs from the lists before calculating MCSE
  relative_bias_list <- unlist(results_df$RelativeBiasList)
  relative_rmse_list <- unlist(results_df$RelativeRMSEList)
  
  relative_bias_list <- na.omit(relative_bias_list)
  relative_rmse_list <- na.omit(relative_rmse_list)
  
  # Calculate summary statistics
  summary_stats <- results_df %>%
    group_by(model_type, N, reliability, method) %>%
    summarise(
      ConvergenceRate = mean(Converged),
      NonConvergenceCount = sum(NonConverged),
      n_converged = sum(Converged),
      MeanMaxSanityCheckDifference = mean(MaxSanityCheckDifference, na.rm = TRUE),
      SanityCheckAlarmCount = sum(SanityCheckAlarm, na.rm = TRUE),
      MeanCoverage = mean(Coverage, na.rm = TRUE),
      MeanRelativeBias = mean(RelativeBias, na.rm = TRUE),
      MeanRelativeRMSE = mean(RelativeRMSE, na.rm = TRUE),
      MCSE_RelativeBias = calculate_mcse_bias(relative_bias_list),
      MCSE_RelativeRMSE = calculate_mcse_rmse(relative_rmse_list),
      ImproperSolutionsCount = sum(ImproperSolution, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(model_type, N, reliability, method)
  
  # Return summary statistics and detailed results
  list(Summary = summary_stats, DetailedResults = results_df)
}

# Run complete simulation study
cat("Starting simulation study 2...\n")
simulation_results <- run_study_2(params)
cat("Simulation study completed. Saving results...\n")

# Save with timestamp
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
filename <- paste0("simulation_results_", timestamp, ".rda")
save_results(simulation_results, filename)

cat("Results saved to:", file.path(results_dir, filename), "\n")
print(simulation_results$Summary)
