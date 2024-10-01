source("utils.R")

process_study1 <- function(file_path) {
  print("Reading results file")
  study_1 <- readRDS(file_path)
  detailed_results_1 <- study_1$DetailedResults
  rm(study_1)
  gc()
  
  # Process warnings and errors
  summary_warnings_1 <- process_study_warnings(detailed_results_1, 1)
  
  # Filter and summarize
  print("computing convergence rate and filtering")
  detailed_results_1 <- filter_and_summarize(detailed_results_1, 1)
  
  # Calculate parameter-wise metrics
  print("Calculate parameter-wise metrics")
  study1_paramwise <- process_study_parameterwise(detailed_results_1, 1)
  
  # Calculate metrics aggregated across parameters
  print("Calculate metrics aggregated across parameters")
  study1_aggregated <- aggregate_results(study1_paramwise, 1)
  
  # Save with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  # Define filenames with timestamp for study 1
  paramwise_filename <- paste0("../simulation/results_test/parameter_wise_summary_study1_", timestamp, ".rda")
  aggregated_filename <- paste0("../simulation/results_test/aggregated_summary_study1_", timestamp, ".rda")
  
  # Save results with the new filenames
  saveRDS(study1_paramwise, file = paramwise_filename)
  saveRDS(study1_aggregated, file = aggregated_filename)
  
  # Display the saved file paths
  cat("Study 1 results saved to:\n",
      paramwise_filename, "\n",
      aggregated_filename, "\n")
  
  # Clean up environment
  rm(detailed_results_1, study1_paramwise, study1_aggregated)
  gc()
  
  print("Study 1 processing completed.")
  
  return(summary_warnings_1)
}