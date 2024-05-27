# setup
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

# Set up the environment
RNGkind("L'Ecuyer-CMRG")
plan(multisession, workers = detectCores() - 1)

# Generate seeds
generate_seeds <- function(n, seed) {
  set.seed(seed)
  sample.int(.Machine$integer.max, n)
}

# Save common variables
n_reps_study_1 <- 10
initial_seed <- 42
seeds <- generate_seeds(n_reps_study_1, initial_seed)

save(seeds, file = "seeds.RData")
