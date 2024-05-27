# Load necessary libraries
library(dplyr)
library(lavaan)

# Function to calculate coverage of confidence intervals
calculate_coverage <- function(fit, true_values) {
  if (is.null(fit) || !lavInspect(fit, "converged")) {
    return(NA)
  }
  # Extract parameter estimates with confidence intervals
  param_estimates <- parameterEstimates(fit)
  # Filter for the paths of interest
  param_estimates <- param_estimates %>%
    filter(lhs %in% c("f3", "f4", "f5") & op == "~")
  
  coverage <- sapply(names(true_values$B), function(param) {
    parts <- unlist(strsplit(param, "~"))
    row <- param_estimates %>%
      filter(lhs == parts[1], rhs == parts[2])
    if (nrow(row) > 0) {
      row$ci.lower <= true_values$B[param] && row$ci.upper >= true_values$B[param]
    } else {
      NA
    }
  })
  mean(coverage, na.rm = TRUE)
}
