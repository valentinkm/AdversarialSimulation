library(furrr)
library(future)

# List of R scripts to run
r_scripts <- c("simulation1.R", "simulation1b.R", "simulation2.R", "simulation3.R", "simulation4.R", "simulation4a.R")

# Set up parallel processing
plan(multisession, workers = parallel::detectCores())

# Function to source a script
run_script <- function(script) {
  source(script)
}

# Run all scripts in parallel
future_map(r_scripts, run_script)

# Optionally, save results to a file or list if needed
