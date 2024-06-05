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
library(future.batchtools)

# Set up the environment
RNGkind("L'Ecuyer-CMRG")


# Always use Tardis plan

plan(list(
  tweak(batchtools_slurm,
        template = "simulation/.batchtools.slurm.singularity.tmpl",
        resources=list(ncpus=1, memory='200m', walltime=600, partition='short') # use short instead of quick
  )
))

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
