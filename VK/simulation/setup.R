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
