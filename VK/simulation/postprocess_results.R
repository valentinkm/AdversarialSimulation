# ------------------------------ Read Results -------------------------------
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(progress)
library(R.utils)
source("gen_mat.R")

study_1 <- readRDS("../simulation/results/simulation_results_study1r20240814170807.rda")
study_2 <- readRDS("../simulation/results/simulation_results_study2r20240814172211.rda")
study_3 <- readRDS("../simulation/results/simulation_results_study3r20240814172328.rda")

detailed_results_1 <- study_1$DetailedResults
detailed_results_2 <- study_2$DetailedResults
detailed_results_3 <- study_3$DetailedResults

# ---------------- Errors and Warnings ----------------------
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

# Function to process results
process_results <- function(detailed_results, study_number) {
  detailed_results %>%
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
    mutate(count = pmin(count, 1)) %>%  # Ensure we count each unique condition only once
    group_by(Study, model_type, N, reliability, method, MessageType, Message) %>%
    summarise(count = sum(count), .groups = 'drop') %>%
    arrange(desc(count))
}

# Process unique warnings and errors
process_unique_messages <- function(detailed_results) {
  warnings <- unique(detailed_results$Warnings)
  errors <- unique(detailed_results$Errors)
  all_messages <- c(warnings, errors)
  all_messages <- all_messages[!is.na(all_messages) & all_messages != ""]
  unique(map_chr(all_messages, standardize_lavaan_warning))
}

messages_1 <- process_unique_messages(detailed_results_1)

# consider only lSAM with single measurement blocks for each factor 
# for simplicity:
detailed_results_2_b5 <- detailed_results_2 %>% 
  filter(b == 5)
messages_2 <- process_unique_messages(detailed_results_2_b5)

messages_3 <- process_unique_messages(detailed_results_3)

all_unique_messages <- unique(c(messages_1, messages_2, messages_3))

combined_list <- tibble(
  ID = seq_along(all_unique_messages),
  Message = all_unique_messages
) %>%
  mutate(Message = str_wrap(Message, width = 80))

# Create id_map
id_map <- setNames(combined_list$ID, all_unique_messages)

# Process summaries
summary_warnings_1 <- process_results(detailed_results_1, 1)
summary_warnings_2 <- process_results(detailed_results_2, 2)
summary_warnings_3 <- process_results(detailed_results_3, 3)

combined_summary <- bind_rows(summary_warnings_1, summary_warnings_2, summary_warnings_3) %>%
  mutate(MessageID = id_map[Message]) %>%
  select(-Message) %>%
  rename(
    `Model Type` = model_type,
    `Message ID` = MessageID,
    `Reliability` = reliability,
    `Method` = method,
    `Count` = count
  )

saveRDS(combined_list, "processed_error_list.rds")
saveRDS(combined_summary, "processed_error_summary.rds")

# ------------------------------- Excluded Cases -------------------------------
detailed_results_1 <- detailed_results_3 %>%
  mutate(ImproperSolution = case_when(
    grepl("variances are negative", Warnings, fixed = TRUE) ~ 1,
    TRUE ~ as.numeric(ImproperSolution)
  ),
  OtherEstimationIssue = case_when(
    is.na(Warnings) | Warnings == "" ~ 0, 
    grepl("variances are negative", Warnings, fixed = TRUE) ~ 0,
    TRUE ~ 1
  )
)

detailed_results_2 <- detailed_results_2 %>%
  mutate(ImproperSolution = case_when(
    grepl("variances are negative", Warnings, fixed = TRUE) ~ 1,
    TRUE ~ as.numeric(ImproperSolution)
  ),
  OtherEstimationIssue = case_when(
    is.na(Warnings) | Warnings == "" ~ 0, 
    grepl("variances are negative", Warnings, fixed = TRUE) ~ 0,
    TRUE ~ 1
  )
)

detailed_results_3 <- detailed_results_3 %>%
  mutate(ImproperSolution = case_when(
    grepl("variances are negative", Warnings, fixed = TRUE) ~ 1,
    TRUE ~ as.numeric(ImproperSolution)
  ),
  OtherEstimationIssue = case_when(
    is.na(Warnings) | Warnings == "" ~ 0, 
    grepl("variances are negative", Warnings, fixed = TRUE) ~ 0,
    TRUE ~ 1
  )
)

detailed_results_1_filtered <- detailed_results_1 %>%
  filter(
    ImproperSolution != 1,
    Converged != 0,
    OtherEstimationIssue != 1
  )

detailed_results_2_filtered <- detailed_results_2 %>%
  filter(
    ImproperSolution != 1,
    Converged != 0,
    OtherEstimationIssue != 1
  )

detailed_results_3_filtered <- detailed_results_3 %>%
  filter(
    ImproperSolution != 1,
    Converged != 0,
    OtherEstimationIssue != 1
  )


# ------------------- Function to get true parameter values --------------------

get_true_values <- function(study, model_type, R_squared = NULL) {
  MLIST <- gen_mat(model_type, R_squared = R_squared)
  BETA <- MLIST$beta
  
  # Define the specific parameters we want to extract for each study
  if (study == 1 || study == 3) {
    required_params <- c("f3~f1", "f3~f2", "f3~f4", "f4~f1", "f4~f2", "f5~f3", "f5~f4", "f5~f1")
  } else if (study == 2) {
    required_params <- c("f3~f1", "f3~f2", "f4~f1", "f4~f2", "f4~f3", "f5~f3", "f5~f4", "f5~f2")
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

# ------------------- Correct Bias and RMSE calculation for Study 1 ------------

# Function to calculate coverage of confidence intervals
calculate_coverage <- function(fit, true_values) {
  if (is.null(fit) || !lavInspect(fit, "converged")) {
    return(NA)
  }
  
  param_estimates <- parameterEstimates(fit)
  param_estimates <- param_estimates %>% filter(op == "~")
  
  coverage <- map_dbl(names(true_values), function(param) {
    parts <- unlist(strsplit(param, "~"))
    row <- param_estimates %>%
      filter(lhs == parts[1], rhs == parts[2])
    if (nrow(row) > 0) {
      row$ci.lower <= true_values[param] && row$ci.upper >= true_values[param]
    } else {
      NA
    }
  })
  
  mean(coverage, na.rm = TRUE)
}

# Function to calculate empirical relative bias
calculate_relative_bias <- function(estimated_paths, true_values) {
  if (all(is.na(estimated_paths))) {
    return(NA)
  }
  
  common_params <- intersect(names(estimated_paths), names(true_values))
  aligned_true_values <- true_values[common_params]
  aligned_estimated_paths <- estimated_paths[common_params]
  
  bias <- (aligned_estimated_paths - aligned_true_values) / aligned_true_values
  mean(bias, na.rm = TRUE)
}

# Function to calculate empirical relative RMSE
calculate_relative_rmse <- function(estimated_paths, true_values) {
  if (all(is.na(estimated_paths))) {
    return(NA)
  }
  
  common_params <- intersect(names(estimated_paths), names(true_values))
  aligned_true_values <- true_values[common_params]
  aligned_estimated_paths <- estimated_paths[common_params]
  
  rmse <- sqrt(mean((aligned_estimated_paths - aligned_true_values)^2, na.rm = TRUE)) / mean(aligned_true_values)
  rmse
}

# Recalculate metrics for each row
detailed_results_1 <- detailed_results_1 %>%
  rowwise() %>%
  mutate(
    true_values = list(get_true_values(study = 1, model_type = model_type, R_squared = R_squared)),
    RecalculatedCoverage = if (!is.null(results) && "result" %in% names(results)) {
      calculate_coverage(results$result, true_values)
    } else {
      NA
    },
    RecalculatedRelativeBias = calculate_relative_bias(EstimatedPaths, true_values),
    RecalculatedRelativeRMSE = calculate_relative_rmse(EstimatedPaths, true_values)
  ) %>%
  ungroup()

# --------------------- Parameter Wise Metric Calculation ----------------------

options(dplyr.summarise.inform = FALSE)

# Process a single row
process_row <- function(row, study) {
  if (study == 1 || study == 3) {
    true_values <- get_true_values(study, row$model_type)
    estimated_paths <- row$EstimatedPaths
    
    tibble(
      model_type = row$model_type,
      N = row$N,
      reliability = row$reliability,
      method = row$method,
      parameter = names(estimated_paths),
      true_value = true_values[names(estimated_paths)],
      estimated_value = unlist(estimated_paths)
    ) %>%
      mutate(
        bias = abs(estimated_value - true_value),
        rmse = (estimated_value - true_value)^2
      )
  } else if (study == 2) {
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
        bias = NA_real_,
        rmse = NA_real_
      ))
    }
    
    true_values <- get_true_values(study, row$model_type, row$R_squared)
    estimated_paths <- row$EstimatedPaths
    
    all_params <- names(true_values)
    misspecified_paths <- c("f3~f1", "f4~f3", "f3~f2")
    
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
        bias = ifelse(parameter %in% misspecified_paths,
                      abs(estimated_value - true_value),
                      (estimated_value - true_value) / abs(true_value)),
        rmse = ifelse(parameter %in% misspecified_paths,
                      (estimated_value - true_value)^2,
                      ((estimated_value - true_value) / abs(true_value))^2)
      )
  }
}

# Main processing function
process_study <- function(study_data, study_number, chunk_size = 100000) {
  if (study_number == 1 || study_number == 3) {
    total_rows <- nrow(study_data)
    processed_data <- study_data %>%
      rowwise() %>%
      do(process_row(., study_number)) %>%
      ungroup() %>%
      group_by(model_type, N, reliability, method, parameter) %>%
      summarise(
        n = n(),
        MeanAbsBias = mean(bias, na.rm = TRUE),
        MeanAbsRMSE = sqrt(mean(rmse, na.rm = TRUE)),
        MCSE_AbsBias = sd(bias, na.rm = TRUE) / sqrt(n()),
        MCSE_AbsRMSE = sd(sqrt(rmse), na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      ) %>%
      mutate(
        CI_AbsBias_Lower = MeanAbsBias - 1.96 * MCSE_AbsBias,
        CI_AbsBias_Upper = MeanAbsBias + 1.96 * MCSE_AbsBias,
        CI_AbsRMSE_Lower = pmax(0, MeanAbsRMSE - 1.96 * MCSE_AbsRMSE),
        CI_AbsRMSE_Upper = MeanAbsRMSE + 1.96 * MCSE_AbsRMSE
      ) %>%
      arrange(model_type, N, reliability, method, parameter)
    
    saveRDS(processed_data, file = sprintf("../simulation/results/parameter_wise_summary_study%d_full.rds", study_number))
    
    cat(sprintf("\nProcessing completed for Study %d.\n", study_number))
    cat(sprintf("Total rows processed: %d\n", total_rows))
    
    return(processed_data)
  } else if (study_number == 2) {
    total_rows <- nrow(study_data)
    n_chunks <- ceiling(total_rows / chunk_size)
    all_aggregated <- list()
    
    for (i in 1:n_chunks) {
      chunk_start_time <- Sys.time()
      cat(sprintf("\nProcessing chunk %d of %d for Study %d\n", i, n_chunks, study_number))
      
      start_idx <- (i - 1) * chunk_size + 1
      end_idx <- min(i * chunk_size, total_rows)
      
      chunk_data <- study_data[start_idx:end_idx, ]
      
      tryCatch({
        withTimeout({
          processed_chunk <- chunk_data %>%
            rowwise() %>%
            do(process_row(., study_number)) %>%
            ungroup()
          
          chunk_aggregated <- processed_chunk %>%
            filter(!is.na(parameter)) %>%
            group_by(model_type, N, reliability, R_squared, method, b, parameter) %>%
            summarise(
              n = n(),
              sum_bias = sum(bias, na.rm = TRUE),
              sum_bias_squared = sum(bias^2, na.rm = TRUE),
              sum_rmse = sum(rmse, na.rm = TRUE),
              sum_rmse_squared = sum(rmse^2, na.rm = TRUE),
              .groups = 'drop'
            )
          
          total_cases <- nrow(chunk_data)
          included_cases <- sum(!chunk_data$ImproperSolution & chunk_data$Converged == 1)
          
          chunk_aggregated <- chunk_aggregated %>%
            mutate(
              total_cases = total_cases,
              included_cases = included_cases
            )
          
          saveRDS(chunk_aggregated, file = sprintf("../simulation/results/study%d_chunk_aggregated_%d.rds", study_number, i))
          
          all_aggregated[[i]] <- chunk_aggregated
          
        }, timeout = 3600)
      }, error = function(e) {
        cat(sprintf("Error in chunk %d: %s\n", i, e$message))
      })
      
      chunk_end_time <- Sys.time()
      chunk_duration <- as.numeric(difftime(chunk_end_time, chunk_start_time, units = "secs"))
      cat(sprintf("Chunk %d processed in %.2f seconds\n", i, chunk_duration))
    }
    
    combined_results <- bind_rows(all_aggregated)
    
    summary_stats <- combined_results %>%
      group_by(model_type, N, reliability, R_squared, method, b, parameter) %>%
      summarise(
        n = sum(n),
        total_cases = sum(total_cases),
        included_cases = sum(included_cases),
        MeanBias = sum(sum_bias) / n,
        MeanRMSE = sqrt(sum(sum_rmse_squared) / n),
        MCSE_Bias = sqrt(abs((sum(sum_bias_squared) - (sum(sum_bias)^2 / n)) / (n * (n - 1)))),
        MCSE_RMSE = sqrt(abs((sum(sum_rmse_squared) - (sum(sum_rmse)^2 / n)) / (n * (n - 1)))),
        .groups = 'drop'
      ) %>%
      mutate(
        InclusionRate = included_cases / total_cases,
        CI_Bias_Lower = MeanBias - 1.96 * MCSE_Bias,
        CI_Bias_Upper = MeanBias + 1.96 * MCSE_Bias,
        CI_RMSE_Lower = pmax(0, MeanRMSE - 1.96 * MCSE_RMSE),
        CI_RMSE_Upper = MeanRMSE + 1.96 * MCSE_RMSE
      ) %>%
      arrange(model_type, N, reliability, R_squared, method, b, parameter)
    
    saveRDS(summary_stats, file = sprintf("../simulation/results/parameter_wise_summary_study%d_full.rds", study_number))
    
    cat(sprintf("\nProcessing completed for Study %d.\n", study_number))
    cat(sprintf("Total rows processed: %d\n", total_rows))
    cat(sprintf("Number of chunks: %d\n", n_chunks))
    cat(sprintf("Total processing time: %.2f hours\n", as.numeric(difftime(Sys.time(), chunk_start_time, units = "hours"))))
    
    return(summary_stats)
  }
}

# study1_results <- process_study(detailed_results_1, 1)
# study2_results <- process_study(detailed_results_2, 2)
# study3_results <- process_study(detailed_results_3, 3)

saveRDS(process_study(detailed_results_1, 1), 
        file = "../simulation/results/parameter_wise_summary_study1_full.rds")
saveRDS(process_study(detailed_results_2, 2), 
        file = "../simulation/results/parameter_wise_summary_study2_full.rds")
saveRDS(process_study(detailed_results_3, 3), 
        file = "../simulation/results/parameter_wise_summary_study3_full.rds")



