library(purrr)
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
# Common function to calculate summary statistics for Study 2
calculate_summary_study2 <- function(detailed_results) {
  # Define the paths in the estimation model
  estimation_model_paths <- c("f3~f1", "f3~f2", "f4~f1", "f4~f2", "f4~f3", "f5~f3", "f5~f4", "f5~f2")
  
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
      MeanMaxSanityCheckDifference = mean(MaxSanityCheckDifference[ImproperSolution == FALSE], na.rm = TRUE),
      SanityCheckAlarmCount = sum(SanityCheckAlarm[ImproperSolution == FALSE], na.rm = TRUE),
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
study_1 <- readRDS("../simulation/results/simulation_results_study1.rda")

detailed_results_1 <- study_1$DetailedResults
summary_1 <- calculate_summary_study1(detailed_results_1)
saveRDS(summary_1, file = "../simulation/results/summary_study1.rds")

# ----------------- Study 2 ---------------------
study_2 <- readRDS("../simulation/results/simulation_results_study2.rda")
detailed_results_2 <- study_2$DetailedResults
summary_2 <- calculate_summary_study2(detailed_results_2)
saveRDS(summary_2, file = "../simulation/results/summary_study2.rds")

row1 <- study_2$DetailedResults[1,]

row1$EstimatedPaths

######################
library(dplyr)
library(tidyr)
library(purrr)
library(progress)
library(R.utils)  # For withTimeout

options(dplyr.summarise.inform = FALSE)

# study_2 <- readRDS("../simulation/results/simulation_results_study2.rda")

source("gen_mat.R")

# Function to get true parameter values
get_true_values <- function(model_type, R_squared) {
  MLIST <- gen_mat(model_type, R_squared = R_squared)
  BETA <- MLIST$beta
  
  # Correctly specified paths
  B_true <- BETA[BETA != 0]
  param_names <- character(length(B_true))
  
  counter <- 1
  for (i in 1:nrow(BETA)) {
    for (j in 1:ncol(BETA)) {
      if (BETA[i, j] != 0) {
        param_names[counter] <- paste0("f", i, "~f", j)
        counter <- counter + 1
      }
    }
  }
  
  names(B_true) <- param_names
  
  # Add misspecified paths (always with true value 0)
  misspecified_paths <- c("f3~f1", "f4~f3", "f3~f2")
  for (path in misspecified_paths) {
    if (!(path %in% names(B_true))) {
      B_true[path] <- 0
    }
  }
  
  # Ensure all paths in the estimation model are included
  estimation_model_paths <- c("f3~f1", "f3~f2", "f4~f1", "f4~f2", "f4~f3", "f5~f3", "f5~f4", "f5~f2")
  for (path in estimation_model_paths) {
    if (!(path %in% names(B_true))) {
      B_true[path] <- 0
    }
  }
  
  return(B_true)
}

# Pre-calculate true values
true_values_list <- list()
unique_conditions <- study_2$DetailedResults %>%
  distinct(model_type, R_squared)

for (i in 1:nrow(unique_conditions)) {
  key <- paste(unique_conditions$model_type[i], unique_conditions$R_squared[i], sep = "_")
  true_values_list[[key]] <- get_true_values(unique_conditions$model_type[i], unique_conditions$R_squared[i])
}

# Process a single row
process_row <- function(row) {
  if (row$ImproperSolution || row$Converged == 0) {
    return(tibble(
      model_type = row$model_type,
      N = row$N,
      reliability = row$reliability,
      R_squared = row$R_squared,
      method = row$method,
      b = row$b,
      parameter = NA_character_,
      true_value = NA_real_,
      estimated_value = NA_real_,
      relative_bias = NA_real_,
      relative_rmse = NA_real_
    ))
  }
  
  true_values <- true_values_list[[paste(row$model_type, row$R_squared, sep = "_")]]
  estimated_paths <- row$EstimatedPaths
  
  all_params <- names(true_values)
  
  tibble(
    model_type = row$model_type,
    N = row$N,
    reliability = row$reliability,
    R_squared = row$R_squared,
    method = row$method,
    b = row$b,
    parameter = all_params,
    true_value = true_values,
    estimated_value = estimated_paths[all_params]
  ) %>%
    mutate(
      relative_bias = (estimated_value - true_value) / pmax(abs(true_value), 1e-6),
      relative_rmse = ((estimated_value - true_value) / pmax(abs(true_value), 1e-6))^2
    )
}

# Calculate the total number of chunks
total_rows <- nrow(study_2$DetailedResults)
chunk_size <- 100000  # Increased chunk size for efficiency
n_chunks <- ceiling(total_rows / chunk_size)

all_aggregated <- list()

# Process all chunks
for (i in 1:n_chunks) {
  chunk_start_time <- Sys.time()
  cat(sprintf("\nProcessing chunk %d of %d\n", i, n_chunks))
  
  start_idx <- (i - 1) * chunk_size + 1
  end_idx <- min(i * chunk_size, nrow(study_2$DetailedResults))
  
  chunk_data <- study_2$DetailedResults[start_idx:end_idx, ]
  
  tryCatch({
    withTimeout({
      processed_chunk <- chunk_data %>%
        rowwise() %>%
        do(process_row(.)) %>%
        ungroup()
      
      # Aggregate results within the chunk
      chunk_aggregated <- processed_chunk %>%
        filter(!is.na(parameter)) %>%  # Remove rows for skipped cases
        group_by(model_type, N, reliability, R_squared, method, b, parameter) %>%
        summarise(
          n = n(),
          sum_bias = sum(relative_bias, na.rm = TRUE),
          sum_bias_squared = sum(relative_bias^2, na.rm = TRUE),
          sum_rmse = sum(relative_rmse, na.rm = TRUE),
          sum_rmse_squared = sum(relative_rmse, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Count total cases and included cases
      total_cases <- nrow(chunk_data)
      included_cases <- sum(!chunk_data$ImproperSolution & chunk_data$Converged == 1)
      
      # Add these counts to chunk_aggregated
      chunk_aggregated <- chunk_aggregated %>%
        mutate(
          total_cases = total_cases,
          included_cases = included_cases
        )
      
      # Save chunk results
      saveRDS(chunk_aggregated, file = sprintf("../simulation/results/chunk_aggregated_%d.rds", i))
      
      # Add to all_aggregated list
      all_aggregated[[i]] <- chunk_aggregated
      
    }, timeout = 3600)  # 1 hour timeout for each chunk
  }, error = function(e) {
    cat(sprintf("Error in chunk %d: %s\n", i, e$message))
  })
  
  chunk_end_time <- Sys.time()
  chunk_duration <- as.numeric(difftime(chunk_end_time, chunk_start_time, units = "secs"))
  cat(sprintf("Chunk %d processed in %.2f seconds\n", i, chunk_duration))
}

# Combine results
cat("Combining results...\n")
combined_results <- bind_rows(all_aggregated)

# Calculate final summary statistics
cat("Calculating final summary statistics...\n")
summary_stats <- combined_results %>%
  group_by(model_type, N, reliability, R_squared, method, b, parameter) %>%
  summarise(
    n = sum(n),
    total_cases = sum(total_cases),
    included_cases = sum(included_cases),
    MeanRelativeBias = sum(sum_bias) / n,
    MeanRelativeRMSE = sqrt(sum(sum_rmse_squared) / n),
    MCSE_RelativeBias = sqrt(abs((sum(sum_bias_squared) - (sum(sum_bias)^2 / n)) / (n * (n - 1)))),
    MCSE_RelativeRMSE = sqrt(abs((sum(sum_rmse_squared) - (sum(sum_rmse)^2 / n)) / (n * (n - 1)))),
    .groups = 'drop'
  ) %>%
  mutate(
    InclusionRate = included_cases / total_cases,
    CI_RelativeBias_Lower = MeanRelativeBias - 1.96 * MCSE_RelativeBias,
    CI_RelativeBias_Upper = MeanRelativeBias + 1.96 * MCSE_RelativeBias,
    CI_RMSE_Lower = pmax(0, MeanRelativeRMSE - 1.96 * MCSE_RelativeRMSE),
    CI_RMSE_Upper = MeanRelativeRMSE + 1.96 * MCSE_RelativeRMSE
  ) %>%
  arrange(model_type, N, reliability, R_squared, method, b, parameter)

# Save the final results
cat("Saving final results...\n")
saveRDS(summary_stats, file = "../simulation/results/parameter_wise_summary_study2_full.rds")

print("Head of summary statistics:")
print(head(summary_stats))

cat("Processing completed.\n")

# Print summary of processing
cat("\nProcessing Summary:\n")
cat(sprintf("Total rows processed: %d\n", total_rows))
cat(sprintf("Chunk size: %d\n", chunk_size))
cat(sprintf("Number of chunks: %d\n", n_chunks))
cat(sprintf("Total processing time: %.2f hours\n", as.numeric(difftime(Sys.time(), chunk_start_time, units = "hours"))))

####################################################################

params <- readRDS("results/parameter_wise_summary_study2_full.rds")

params1 <- readRDS("results/chunk_aggregated_1.rds")
unique(params1$parameter)

unique(params$parameter)

View(params1)

library(ggplot2)
library(dplyr)
library(tidyr)

# Define misspecified paths
misspecified_paths <- c("f3~f1", "f4~f3", "f3~f2")

# Filter and prepare the data
plot_data <- summary_stats %>%
  filter(R_squared == 0.4, b == 5, model_type == "2.1") %>%
  select(N, reliability, method, parameter, MeanRelativeBias) %>%
  mutate(
    N = factor(N, levels = c("100", "400", "6400")),
    reliability = factor(reliability, levels = c("0.3", "0.5", "0.7")),
    method = factor(method, levels = c("SEM", "gSAM", "lSAM_ML", "lSAM_ULS")),
    path_type = ifelse(parameter %in% misspecified_paths, "Misspecified", "Correctly specified")
  ) %>%
  # Reorder parameters to group correctly specified and misspecified paths
  arrange(path_type, parameter) %>%
  mutate(parameter = factor(parameter, levels = unique(parameter)))

# Create the plot
ggplot(plot_data, aes(x = method, y = parameter)) +
  geom_tile(aes(fill = MeanRelativeBias), color = "white") +
  geom_text(aes(label = sprintf("%.2f", MeanRelativeBias)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limits = c(-1, 1), oob = scales::squish) +
  facet_grid(
    rows = vars(N, reliability),
    cols = vars(method),
    scales = "free",
    space = "free"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8, 
                               color = ifelse(levels(plot_data$parameter) %in% misspecified_paths, "red", "black")),
    strip.text = element_text(size = 10),
    panel.spacing = unit(0.3, "lines"),
    legend.position = "bottom"
  ) +
  # Add a rectangle to separate correctly specified and misspecified paths
  geom_rect(data = data.frame(ymin = which(plot_data$path_type == "Misspecified")[1] - 0.5,
                              ymax = Inf,
                              xmin = -Inf,
                              xmax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "gray90", alpha = 0.5, inherit.aes = FALSE) +
  # Ensure the tiles and text are on top of the rectangle
  geom_tile(aes(fill = MeanRelativeBias), color = "white") +
  geom_text(aes(label = sprintf("%.2f", MeanRelativeBias)), size = 3) +
  labs(
    title = "Mean Relative Bias by Parameter, Condition, and Method",
    subtitle = "Model Type 2.1, R-squared = 0.4, 5 measurement blocks\nMisspecified paths (in red) shown with gray background",
    x = "Method",
    y = "Parameter",
    fill = "Mean Relative Bias"
  )
