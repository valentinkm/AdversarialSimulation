source("utils.R")

process_study3 <- function() {
  print("Reading results file")
  study_3 <- readRDS("../simulation/results/simulation_results_study3.rda")
  detailed_results_3 <- study_3$DetailedResults
  rm(study_3)
  gc()
  
  # Process warnings and errors
  print("Processing warnings and errors")
  summary_warnings_3 <- process_study_warnings(detailed_results_3, 3)
  
  # Adjust improper solutions and track estimation general issues
  print("Adjust improper solutions and track estimation general issues")
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
  
  detailed_results_3 <- detailed_results_3 %>%
    filter(
      ImproperSolution != 1,
      Converged != 0,
      OtherEstimationIssue != 1
    )
  
  # Calculate parameter-wise metrics
  print("Calculate parameter-wise metrics")
  study3_paramwise <- process_study_parameterwise(detailed_results_3, 3)
  
  # Calculate metrics aggregated across parameters
  print("Calculate metrics aggregated across parameters")
  study3_aggregated <- aggregate_results(study3_paramwise, 3)
  
  # Save results
  saveRDS(study3_paramwise, file = "../simulation/results/parameter_wise_summary_study3.rds")
  saveRDS(study3_aggregated, file = "../simulation/results/aggregated_summary_study3.rds")
  cat("study 1 results saved to: \n
      /simulation/results/parameter_wise_summary_study3.rds \n
      /simulation/results/parameter_wise_summary_study3.rd")
  
  rm(detailed_results_3, study3_paramwise, study3_aggregated)
  gc()
  
  print("Study 3 processing completed.")
  
  return(summary_warnings_3)
}