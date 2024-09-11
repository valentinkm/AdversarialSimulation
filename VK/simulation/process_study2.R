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
  
  # Save results
  saveRDS(study2_paramwise, file = "../simulation/results/parameter_wise_summary_study2.rds")
  saveRDS(study2_aggregated, file = "../simulation/results/aggregated_summary_study2.rds")
  cat("study 2 results saved to: \n
      /simulation/results/parameter_wise_summary_study2.rds \n
      /simulation/results/aggregated_summary_study2.rds")
  
  rm(detailed_results_2, study2_aggregated, study2_paramwise)
  gc()
  
  print("Study 2 processing completed.")
  
  return(summary_warnings_2)
}