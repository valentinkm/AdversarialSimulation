library(furrr)
library(future)

# List of R scripts to run
r_scripts <- c("LK/simulation1.R", "LK/simulation1b.R", "LK/simulation2.R", "LK/simulation3.R", "LK/simulation4.R", "LK/simulation4a.R")

# Set up parallel processing
plan(multisession, workers = parallel::detectCores())

# Function to source a script
run_script <- function(script) {
  source(script)
}

# Run all scripts in parallel
future_map(r_scripts, run_script)

# Optionally, save results to a file or list if needed
