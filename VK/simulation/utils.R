library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(R.utils)
source("gen_mat.R")

standardize_lavaan_warning <- function(warning) {
  if (is.na(warning) || warning == "") return(NA_character_)
  
  warnings <- str_split(warning, ", ")[[1]]
  
  standardized_warnings <- sapply(warnings, function(w) {
    if (str_detect(w, "The variance-covariance matrix of the estimated parameters \\(vcov\\)")) {
      w <- str_replace(w, "\\(= .+\\)", "(= [VALUE])")
      w <- str_replace(w, "is (close to|smaller than) zero", "is [RELATION] zero")
      return("lavaan WARNING: The variance-covariance matrix of the estimated parameters (vcov) does not appear to be positive definite! The smallest eigenvalue is smaller than or close to zero. This may be a symptom that the model is not identified.")
    } else if (str_detect(w, "some estimated lv variances are negative")) {
      return("lavaan WARNING: some estimated lv variances are negative")
    } else {
      return(w)
    }
  })
  
  paste(unique(standardized_warnings), collapse = ", ")
}

process_study_warnings <- function(detailed_results, study_number) {
  warnings_summary <- detailed_results %>%
    mutate(
      Warnings = map_chr(Warnings, standardize_lavaan_warning),
      Errors = map_chr(Errors, standardize_lavaan_warning),
      MessageType = case_when(
        !is.na(Warnings) ~ "Warning",
        !is.na(Errors) ~ "Error",
        TRUE ~ NA_character_
      ),
      Message = coalesce(Warnings, Errors)
    ) %>%
    filter(!is.na(MessageType)) %>%
    mutate(
      method = case_when(
        str_detect(method, "lSAM_ULS") ~ "lSAM-ULS",
        TRUE ~ method
      ),
      Study = paste("Study", study_number)
    ) %>%
    group_by(Study, model_type, N, reliability, method, seed, MessageType, Message) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(count = pmin(count, 1)) %>%
    group_by(Study, model_type, N, reliability, method, MessageType, Message) %>%
    summarise(count = sum(count), .groups = 'drop') %>%
    arrange(desc(count))
  
  return(warnings_summary)
}

# ----
library(dplyr)

filter_and_summarize <- function(detailed_results, study_number) {
  
  # Ensure variables are logical and properly handle ImproperSolution and OtherEstimationIssue
  detailed_results <- detailed_results %>% 
    mutate(
      Converged = as.logical(Converged),
      ImproperSolution = case_when(
        grepl("variances are negative", Warnings, fixed = TRUE) ~ TRUE,
        TRUE ~ as.logical(ImproperSolution)
      ),
      OtherEstimationIssue = case_when(
        is.na(Warnings) | Warnings == "" ~ FALSE,
        grepl("variances are negative", Warnings, fixed = TRUE) ~ FALSE,
        TRUE ~ TRUE
      )
    )
  
  # Determine grouping variables based on study number
  if (study_number %in% c(1, 3)) {
    grouping_vars <- c("model_type", "N", "reliability", "method")
  } else if (study_number == 2) {
    grouping_vars <- c("model_type", "N", "reliability", "R_squared", "method")
  }
  
  # Summarize the data
  results_summary <- detailed_results %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      ConvergedCount = sum(Converged),
      NotConvergedCount = sum(!Converged),
      ImproperSolutionCount = sum(Converged & ImproperSolution),
      .groups = "drop"
    )
  
  # Save summary
  saveRDS(results_summary, file = sprintf("../simulation/results/convergence_rate%d.rds", study_number))
  
  # Filter out improper and not converged rows
  filtered_results <- detailed_results %>%
    filter(
      Converged == TRUE,
      ImproperSolution == FALSE,
      OtherEstimationIssue == FALSE
    )
  
  return(filtered_results)
}



# ------------------- Helper functions for metric calculation-------------------

get_true_values <- function(study, model_type, R_squared = NULL, for_aggregation = FALSE) {
  MLIST <- gen_mat(model_type, R_squared = R_squared)
  BETA <- MLIST$beta
  
  if (study == 1 || study == 3) {
    required_params <- c("f3~f1", "f3~f2", "f3~f4", "f4~f1", "f4~f2", "f5~f3", "f5~f4", "f5~f1")
    if (for_aggregation && study == 1) {
      required_params <- setdiff(required_params, "f3~f4")
    }
  } else if (study == 2) {
    required_params <- c("f3~f1", "f3~f2", "f4~f1", "f4~f2", "f4~f3", "f5~f3", "f5~f4", "f5~f2")
    if (for_aggregation) {
      required_params <- setdiff(required_params, c("f3~f1", "f4~f3", "f3~f2"))
    }
  } else {
    stop("Invalid study number")
  }
  
  true_values <- numeric(length(required_params))
  names(true_values) <- required_params
  
  for (param in required_params) {
    parts <- strsplit(param, "~")[[1]]
    i <- as.numeric(substr(parts[1], 2, 2))
    j <- as.numeric(substr(parts[2], 2, 2))
    true_values[param] <- BETA[i, j]
  }
  
  return(true_values)
}

# Helper function to filter estimated parameters
filter_estimated_params <- function(estimated_paths, study_number) {
  if (study_number == 1) {
    return(estimated_paths[names(estimated_paths) != "f3~f4"])
  } else if (study_number == 2) {
    return(estimated_paths[!names(estimated_paths) %in% c("f3~f1", "f4~f3", "f3~f2")])
  }
  return(estimated_paths)
}

# ------ Calculate metrics aggregated over all regressors in one model ---------
process_row <- function(row, study_number) {
  if (study_number %in% c(1, 3)) {
    true_values <- get_true_values(study_number, row$model_type)
    estimated_paths <- row$EstimatedPaths
    
    tibble(
      model_type = row$model_type,
      N = row$N,
      reliability = row$reliability,
      method = row$method,
      parameter = names(estimated_paths),
      true_value = true_values[names(estimated_paths)],
      estimated_value = unlist(estimated_paths),
      is_misspecified = FALSE,
      Coverage = row$Coverage  # Add this line
    )
  } else if (study_number == 2) {
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
        is_misspecified = NA,
        Coverage = NA_real_  # Add this line
      ))
    }
    
    true_values <- get_true_values(study_number, row$model_type, row$R_squared)
    estimated_paths <- row$EstimatedPaths
    misspecified_paths <- c("f3~f1", "f4~f3", "f3~f2")
    
    tibble(
      model_type = row$model_type,
      N = row$N,
      reliability = row$reliability,
      R_squared = row$R_squared,
      method = row$method,
      b = row$b,
      parameter = names(true_values),
      true_value = true_values,
      estimated_value = estimated_paths[names(true_values)],
      is_misspecified = parameter %in% misspecified_paths,
      Coverage = row$Coverage  # Add this line
    )
  }
}


# Function to calculate metrics for a group of replications
calculate_metrics <- function(group, study_number) {
  K <- nrow(group)
  is_misspecified <- first(group$is_misspecified)
  true_value <- first(group$true_value)
  
  results <- group %>%
    summarise(
      true_value = first(true_value),
      mean_estimate = if(n() > 0) mean(estimated_value, na.rm = TRUE) else NA_real_,
      var_estimate = if(n() > 0) var(estimated_value, na.rm = TRUE) else NA_real_,
      n = n()
    )
  
  # Absolute metrics (for all studies and parameters)
  results <- results %>%
    mutate(
      bias = mean_estimate - true_value,
      mse = mean((group$estimated_value - true_value)^2),
      rmse = sqrt(mse),
      mcse_bias = sqrt(var_estimate / K),
      mcse_rmse = sqrt(var(sqrt((group$estimated_value - true_value)^2)) * (K - 1) / K)
    )
  
  # Conditional calculation of relative metrics
  if (abs(true_value) > 1e-10) {  # Avoid division by zero or very small numbers
    results <- results %>%
      mutate(
        relative_bias = bias / true_value,
        relative_rmse = sqrt((bias^2 + var_estimate) / true_value^2),
        mcse_relative_bias = sqrt(var_estimate / (K * true_value^2)),
        mcse_relative_rmse = sqrt(var(sqrt(((group$estimated_value - true_value) / true_value)^2)) * (K - 1) / K)
      )
  } else {
    results <- results %>%
      mutate(
        relative_bias = NA_real_,
        relative_rmse = NA_real_,
        mcse_relative_bias = NA_real_,
        mcse_relative_rmse = NA_real_
      )
  }
  
  results
}

# --------------------- Parameter-wise metrics computation ---------------------

process_study_parameterwise <- function(study_data, study_number, chunk_size = 100000) {
  process_chunk <- function(chunk_data, study_number) {
    processed_chunk <- chunk_data %>%
      rowwise() %>%
      do(process_row(., study_number)) %>%
      ungroup()
    
    if (study_number == 2) {
      grouped_chunk <- processed_chunk %>%
        filter(!is.na(parameter)) %>% 
        group_by(model_type, N, reliability, R_squared, method, b, parameter)
    } else {
      grouped_chunk <- processed_chunk %>%
        filter(!is.na(parameter)) %>% 
        group_by(model_type, N, reliability, method, parameter)
    }
    
    summarised_chunk <- grouped_chunk %>%
      summarise(
        n = n(),
        TrueValue = first(true_value),
        SumEstimate = sum(estimated_value, na.rm = TRUE),
        SumSquaredEstimate = sum(estimated_value^2, na.rm = TRUE),
        MeanCoverage = mean(Coverage, na.rm = TRUE),
        .groups = 'drop'
      )
    
    return(summarised_chunk)
  }
  
  if (study_number %in% c(1, 3)) {
    results <- process_chunk(study_data, study_number)
  } else if (study_number == 2) {
    total_rows <- nrow(study_data)
    n_chunks <- ceiling(total_rows / chunk_size)
    all_results <- vector("list", n_chunks)
    
    for (i in 1:n_chunks) {
      chunk_start_time <- Sys.time()
      cat(sprintf("\nProcessing chunk %d of %d for Study %d\n", i, n_chunks, study_number))
      
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, total_rows)
      
      chunk_data <- study_data[start_idx:end_idx, ]
      
      all_results[[i]] <- process_chunk(chunk_data, study_number)
      
      chunk_end_time <- Sys.time()
      chunk_duration <- as.numeric(difftime(chunk_end_time, chunk_start_time, units = "secs"))
      cat(sprintf("Chunk %d processed in %.2f seconds\n", i, chunk_duration))
    }
    
    results <- bind_rows(all_results) %>%
      group_by(model_type, N, reliability, R_squared, method, b, parameter) %>%
      summarise(
        n = sum(n),
        TrueValue = first(TrueValue),
        SumEstimate = sum(SumEstimate),
        SumSquaredEstimate = sum(SumSquaredEstimate),
        MeanCoverage = mean(MeanCoverage, na.rm = TRUE),  # Add this line
        .groups = 'drop'
      )
  }
  
  # Calculate final metrics
  final_results <- results %>%
    mutate(
      MeanEstimate = SumEstimate / n,
      VarEstimate = (SumSquaredEstimate - (SumEstimate^2 / n)) / (n - 1),
      Bias = MeanEstimate - TrueValue,
      RMSE = sqrt(VarEstimate + Bias^2),
      MCSE_Bias = sqrt(VarEstimate / n),
      MCSE_RMSE = sqrt(VarEstimate / (2 * n)),
      RelativeBias = if_else(abs(TrueValue) > 1e-10, Bias / TrueValue, NA_real_),
      RelativeRMSE = if_else(abs(TrueValue) > 1e-10, RMSE / abs(TrueValue), NA_real_),
      MCSE_RelativeBias = if_else(abs(TrueValue) > 1e-10, MCSE_Bias / abs(TrueValue), NA_real_),
      MCSE_RelativeRMSE = if_else(abs(TrueValue) > 1e-10, MCSE_RMSE / abs(TrueValue), NA_real_),
      Coverage = MeanCoverage
    ) %>%
    select(-SumEstimate, -SumSquaredEstimate, -MeanCoverage)
  
  return(final_results)
}

# ------------------ Aggregated across parameter metric computation ------------
aggregate_results <- function(paramwise_results, study_number) {
  # Determine grouping variables based on study number
  if (study_number %in% c(1, 3)) {
    grouping_vars <- c("model_type", "N", "reliability", "method")
    correctly_specified_params <- c("f3~f1", "f3~f2", "f4~f1", "f4~f2", "f5~f3", "f5~f4", "f5~f1")
  } else if (study_number == 2) {
    grouping_vars <- c("model_type", "N", "reliability", "R_squared", "method", "b")
    correctly_specified_params <- c("f4~f1", "f4~f2", "f5~f3", "f5~f4", "f5~f2")
  } else {
    stop("Invalid study number")
  }
  
  # Filter to only include correctly specified parameters
  filtered_results <- paramwise_results %>%
    filter(parameter %in% correctly_specified_params)
  
  # Apply Study 2-specific filtering
  if (study_number == 2) {
    filtered_results <- filtered_results %>%
      filter(!(b == 3 & method %in% c("SEM", "gSAM")))
  }
  
  exclude_from_mean <- c("n", "total_cases", "included_cases", "TrueValue")
  
  # Get the names of numeric columns to aggregate
  numeric_cols <- names(filtered_results)[sapply(filtered_results, is.numeric)]
  
  # Remove grouping variables and excluded variables from numeric_cols
  numeric_cols <- setdiff(numeric_cols, c(grouping_vars, exclude_from_mean))
  
  # Create a list of summary expressions
  summary_exprs <- list()
  
  # Add expressions for excluded variables
  for (col in exclude_from_mean) {
    if (col %in% names(filtered_results)) {
      summary_exprs[[col]] <- expr(first(!!sym(col)))
    }
  }
  
  # Add mean expressions for numeric columns
  for (col in numeric_cols) {
    if (col %in% c("Bias", "RelativeBias")) {
      # Use mean of absolute values for Bias and RelativeBias
      summary_exprs[[paste0("mean_", col)]] <- expr(mean(abs(!!sym(col)), na.rm = TRUE))
    } else {
      summary_exprs[[paste0("mean_", col)]] <- expr(mean(!!sym(col), na.rm = TRUE))
    }
  }
  
  # Add n_parameters to summary expressions
  summary_exprs$n_parameters <- expr(n())
  
  # Aggregate results
  aggregated_results <- filtered_results %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(!!!summary_exprs, .groups = "drop")
  
  # Calculate overall_InclusionRate if possible
  if (all(c("included_cases", "total_cases") %in% names(aggregated_results))) {
    aggregated_results <- aggregated_results %>%
      mutate(overall_InclusionRate = included_cases / total_cases)
  }
  
  # Rename mean_Coverage to average_coverage for studies 1 and 3
  if (study_number %in% c(1, 3)) {
    aggregated_results <- aggregated_results %>%
      rename(average_coverage = mean_Coverage)
  }
  
  return(aggregated_results)
}
