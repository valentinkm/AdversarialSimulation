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
library(pryr)  # For monitoring memory usage

# Function to log memory usage
log_memory_usage <- function() {
  mem <- mem_used()
  cat("Current memory usage:", mem, "\n")
}

# Check if running on Tardis cluster
is_on_tardis <- function() {
  grepl("tardis", Sys.info()["nodename"])
}

# Set up the environment
RNGkind("L'Ecuyer-CMRG")

if (is_on_tardis()) {
  library(future.batchtools)
  plan(list(
    tweak(batchtools_slurm,
          template = "/home/rstudio/simulation/.batchtools.slurm.singularity.tmpl",
          resources = list(ncpus = 1, memory = '2000M', walltime = 600, partition = 'short')
    )
  ))
} else {
  plan(multisession, workers = parallel::detectCores() - 1)
}

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

# Log initial memory usage
log_memory_usage()
