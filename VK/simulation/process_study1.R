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
  
  # Save results
  saveRDS(study1_paramwise, file = "../simulation/results/parameter_wise_summary_study1.rds")
  saveRDS(study1_aggregated, file = "../simulation/results/aggregated_summary_study1.rds")
  cat("study 1 results saved to: \n
      /simulation/results/parameter_wise_summary_study1.rds \n
      /simulation/results/aggregated_summary_study1.rds")
  
  rm(detailed_results_1, study1_paramwise, study1_aggregated)
  gc()
  
  print("Study 1 processing completed.")
  
  return(summary_warnings_1)
}