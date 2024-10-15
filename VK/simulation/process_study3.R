source("utils.R")

process_study3 <- function(file_path) {
  print("Reading results file")
  study_3 <- readRDS(file_path)
  detailed_results_3 <- study_3$DetailedResults
  rm(study_3)
  gc()
  
  # Process warnings and errors
  print("Processing warnings and errors")
  summary_warnings_3 <- process_study_warnings(detailed_results_3, 3)
  
  # Filter and summarize
  print("computing convergence rate and filtering")
  detailed_results_3 <- filter_and_summarize(detailed_results_3, 3)
  
  # Calculate parameter-wise metrics
  print("Calculate parameter-wise metrics")
  study3_paramwise <- process_study_parameterwise(detailed_results_3, 3)
  
  # Calculate metrics aggregated across parameters
  print("Calculate metrics aggregated across parameters")
  study3_aggregated <- aggregate_results(study3_paramwise, 3)
  
  # Save with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  # Define filenames with timestamp for study 1
  paramwise_filename <- paste0("../simulation/results_replic/parameter_wise_summary_study3_", timestamp, ".rds")
  aggregated_filename <- paste0("../simulation/results_replic/aggregated_summary_study3_", timestamp, ".rds")
  
  # Save results with the new filenames
  saveRDS(study3_paramwise, file = paramwise_filename)
  saveRDS(study3_aggregated, file = aggregated_filename)
  
  # Display the saved file paths
  cat("Study 3 results saved to:\n",
      paramwise_filename, "\n",
      aggregated_filename, "\n")
  
  rm(detailed_results_3, study3_paramwise, study3_aggregated)
  gc()
  
  print("Study 3 processing completed.")
  
  return(summary_warnings_3)
}