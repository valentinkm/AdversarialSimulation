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
  grepl("login", Sys.info()["nodename"])
}

# Set up the environment
RNGkind("L'Ecuyer-CMRG")

if (is_on_tardis()) {
  library(future.batchtools)
  #tardis_login <- parallelly::makeClusterPSOCK(port='random', user='kriegmair',
  #					       worker='tardis.mpib-berlin.mpg.de',
  #					       rscript=c('/opt/software/R/4.4.0/bin/Rscript'))
  cores_per_job <- 8
  plan(list(
    #tweak(cluster, workers=tardis_login),
    tweak(batchtools_slurm,
          template = "/home/mpib/kriegmair/.batchtools.slurm.tmpl",
          resources=list(ncpus=cores_per_job, memory=2048, walltime=864000, partition='long', 
			 container='/home/mpib/kriegmair/adversarialsimulation_latest.sif')),
    tweak(multisession, workers=cores_per_job)
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
results_dir <- if (is_on_tardis()) "/home/mpib/kriegmair/AdversarialSimulation/results" else "results"

# Ensure the results directory exists
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}

# Function to save results
save_results <- function(results, filename) {
  saveRDS(results, file.path(results_dir, filename))
}

