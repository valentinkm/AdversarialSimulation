source("utils.R")

process_study2 <- function(file_path) {
  print("Reading results file")
  study_2 <- readRDS(file_path)
  detailed_results_2 <- study_2$DetailedResults
  rm(study_2)
  gc()
  
  # Process warnings and errors
  summary_warnings_2 <- process_study_warnings(detailed_results_2, 2)
  
  # Filter and summarize
  print("computing convergence rate and filtering")
  detailed_results_2 <- filter_and_summarize(detailed_results_2, 2)
  
  # Calculate parameter-wise metrics
  print("Calculate parameter-wise metrics")
  study2_paramwise <- process_study_parameterwise(detailed_results_2, 2)
  
  # Calculate metrics aggregated across parameters
  print("Calculate metrics aggregated across parameters")
  study2_aggregated <- aggregate_results(study2_paramwise, 2)
  
  # Save with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  # Define filenames with timestamp for study 1
  paramwise_filename <- paste0("../simulation/results_replic/parameter_wise_summary_study2_", timestamp, ".rda")
  aggregated_filename <- paste0("../simulation/results_replic/aggregated_summary_study2_", timestamp, ".rda")
  
  # Save results with the new filenames
  saveRDS(study2_paramwise, file = paramwise_filename)
  saveRDS(study2_aggregated, file = aggregated_filename)
  
  # Display the saved file paths
  cat("Study 2 results saved to:\n",
      paramwise_filename, "\n",
      aggregated_filename, "\n")
  
  rm(detailed_results_2, study2_aggregated, study2_paramwise)
  gc()
  
  print("Study 2 processing completed.")
  
  return(summary_warnings_2)
}