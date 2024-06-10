# setup.R

# Load necessary libraries
library(MASS)
library(dplyr)
library(tidyr)
library(future)
library(furrr)
library(lavaan)
library(purrr)
library(parallel)
library(Matrix)


plan(multisession, workers = parallel::detectCores() - 1)

# Generate seeds
generate_seeds <- function(n, seed) {
  set.seed(seed)
  sample.int(.Machine$integer.max, n)
}

# Directory to save results
results_dir <- "results"

# Ensure the results directory exists
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Function to save results
save_results <- function(results, filename) {
  saveRDS(results, file.path(results_dir, filename))
}
