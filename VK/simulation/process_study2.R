source("utils.R")

process_study2 <- function() {
  print("Reading results file")
  study_2 <- readRDS("../simulation/results/simulation_results_study2.rda")
  detailed_results_2 <- study_2$DetailedResults
  rm(study_2)
  gc()
  
  # Process warnings and errors
  summary_warnings_2 <- process_study_warnings(detailed_results_2, 2)
  
  # Adjust improper solutions and track estimation general issues
  print("Adjust improper solutions and track estimation general issues")
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
  
  detailed_results_2 <- detailed_results_2 %>%
    filter(
      ImproperSolution != 1,
      Converged != 0,
      OtherEstimationIssue != 1
    )
  
  # Calculate parameter-wise metrics
  print("Calculate parameter-wise metrics")
  study2_paramwise <- process_study_parameterwise(detailed_results_2, 2)
  
  
  # Calculate metrics aggregated across parameters
  print("Calculate metrics aggregated across parameters")
  study2_aggregated <- aggregate_results(study2_paramwise, 2)
  
  # Save results
  saveRDS(study2_paramwise, file = "../simulation/results/parameter_wise_summary_study2.rds")
  saveRDS(study2_aggregated, file = "../simulation/results/aggregated_summary_study2.rds")
  cat("study 1 results saved to: \n
      /simulation/results/parameter_wise_summary_study2.rds \n
      /simulation/results/parameter_wise_summary_study2.rd")
  
  rm(detailed_results_2, study2_aggregated, study2_paramwise)
  gc()
  
  print("Study 2 processing completed.")
  
  return(summary_warnings_2)
}