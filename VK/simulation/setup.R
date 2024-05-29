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

# check if running on Tardis cluster
is_on_tardis <- function() {
  grepl("tardis", Sys.info()["nodename"])
}

# Set up the environment
RNGkind("L'Ecuyer-CMRG")

if (is_on_tardis()) {
  library(future.batchtools)
  plan(list(
    #tweak(cluster, workers="tardis.mpib-berlin.mpg.de"),
    tweak(batchtools_slurm,
          # workers = 1, # not necessary
          template = "/home/your_username/.batchtools.slurm.singularity.tmpl",
          resources=list(ncpus=1, memory='200m', walltime=600, partition='short') # use short instead of quick
    )
  ))
} else {
  plan(multisession, workers = detectCores() - 1)
}

# Generate seeds
generate_seeds <- function(n, seed) {
  set.seed(seed)
  sample.int(.Machine$integer.max, n)
}

# Directory to save results
results_dir <- if (is_on_tardis()) "/path/to/cluster/results" else "results"

# Ensure the results directory exists
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Function to save results
save_results <- function(results, filename) {
  saveRDS(results, file.path(results_dir, filename))
}


