# study_1.R

# Load necessary scripts and libraries
source("gen_pop_data.R")
source("calc_metrics.R")
source("run_analysis.R")
library(furrr)
library(parallel)
library(dplyr)
library(purrr)
library(tidyr)
library(progress)
library(lavaan)

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
  seed = parallel_seeds(n_reps, seed = 42),
  model_type = c("1.1", "1.2", "1.3", "1.4"),
  N = c(100, 400, 6400),
  reliability = c(0.3, 0.5, 0.7),
  method = c("SEM", "gSAM", "lSAM_ML", "lSAM_ULS")
)


# Set population values
B_true <- c(
  'f3~f1' = 0.1, 'f3~f2' = 0.1, 'f3~f4' = 0.1,
  'f4~f1' = 0.1, 'f4~f2' = 0.1,
  'f5~f3' = 0.1, 'f5~f4' = 0.1, 'f5~f1' = 0.1
)
true_values <- list(
  B = B_true
)

model_syntax_study1 <- "    
    f1 =~ y1 + y2 + y3
    f2 =~ y4 + y5 + y6
    f3 =~ y7 + y8 + y9
    f4 =~ y10 + y11 + y12
    f5 =~ y13 + y14 + y15
    
    f3 ~ f1 + f2 + f4
    f4 ~ f1 + f2
    f5 ~ f3 + f4 + f1
"

# Study function
simulate_inner <- function(model_type, N, reliability, method) {
  safe_quiet_run_analysis <- safely(quietly(run_analysis))
  data <- gen_pop_model_data(model_type, N, reliability)$data
  fit_result <- safe_quiet_run_analysis(data, model_syntax_study1, method)
  sanity_check_estimates <- run_sanity_check(model_type, model_syntax_study1)
  
  # Add sanity check comparison
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
    
    return(list(
      Converged = 1, NonConverged = 0,
      EstimatedPaths = list(estimated_paths),
      SanityCheckEstimates = list(sanity_check_estimates),
      Coverage = coverage,
      RelativeBias = relative_bias,
      RelativeRMSE = relative_rmse,
      RelativeBiasList = list(relative_bias),
      RelativeRMSEList = list(relative_rmse),
      ImproperSolution = improper_solution,
      Warnings = toString(fit_result$result$warnings),
      Messages = toString(fit_result$result$messages),
      Errors = if (is.null(fit_result$error)) NA_character_ else toString(fit_result$error$message)
    ))
  } else {
    return(list(
      Converged = 0, NonConverged = 1,
      EstimatedPaths = list(setNames(rep(NA, length(true_values$B)), names(true_values$B))),
      SanityCheckEstimates = list(sanity_check_estimates),
      Coverage = NA,
      RelativeBias = NA,
      RelativeRMSE = NA,
      RelativeBiasList = list(NA),
      RelativeRMSEList = list(NA),
      ImproperSolution = improper_solution,
      Warnings = toString(fit_result$result$warnings),
      Messages = toString(fit_result$result$messages),
      Errors = if (is.null(fit_result$error)) NA_character_ else toString(fit_result$error$message)
    ))
  }
}

simulate_mid <- function(seed, design) {
  future_pmap(design, simulate_inner,
              .options = furrr_options(seed = rep.int(list(seed), nrow(design))))
}

simulate_outer <- function(chunk, chunk_params) {
  future_pmap(chunk_params, simulate_mid,
              .options = furrr_options(seed = NULL, scheduling = 1))
}

run_study_1 <- function(params, true_values) {
  # Run the simulations and analysis in parallel
  results_df <- suppress_specific_warning({
    params %>%
      mutate(chunk = row_number() %% 100) %>%
      nest(.by = c(chunk, seed), .key = 'design') %>%
      nest(.by = chunk, .key = 'chunk_params') %>%
      mutate(results = future_map2(chunk, chunk_params, simulate_outer, .options = furrr_options(seed=NULL))) %>%
      unnest(c(chunk_params, results)) %>%
      unnest(c(design, results)) %>%
      mutate(
        Converged = map_dbl(results, ~ .x$Converged),
        NonConverged = map_dbl(results, ~ .x$NonConverged),
        Warnings = map_chr(results, ~ .x$Warnings),
        Messages = map_chr(results, ~ .x$Messages),
        Errors = map_chr(results, ~ .x$Errors),
        EstimatedPaths = map(results, ~ .x$EstimatedPaths[[1]]),
        SanityCheckEstimates = map(results, ~ .x$SanityCheckEstimates[[1]]),
        Coverage = map_dbl(results, ~ .x$Coverage),
        RelativeBias = map_dbl(results, ~ .x$RelativeBias),
        RelativeRMSE = map_dbl(results, ~ .x$RelativeRMSE),
        RelativeBiasList = map(results, ~ .x$RelativeBiasList[[1]]),
        RelativeRMSEList = map(results, ~ .x$RelativeRMSEList[[1]]),
        ImproperSolution = map_lgl(results, ~ .x$ImproperSolution)
      )
  }, "no non-missing arguments to max; returning -Inf")
  
  print('Parallel computation done')
  
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
cat("Starting simulation study 1...\n")
simulation_results <- run_study_1(params, true_values)
cat("Simulation study completed. Saving results...\n")

# Save with timestamp
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
filename <- paste0("simulation_results_study1r", timestamp, ".rda")
save_results(simulation_results, filename)

cat("Results saved to:", file.path(results_dir, filename), "\n")
print(simulation_results$Summary)

