libraries <- c("GPArotation", "CDM", "miceadds", "TAM", "sirt", "lavaan", "dplyr", "tidyr", "purrr", "tidyverse", "future", "furrr")
# Set the R mirror to the cloud mirror of RStudio
options(repos = "https://cloud.r-project.org/")

# Load the libraries
for (library_name in libraries) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name)
    library(library_name, character.only = TRUE)
  }
}

# List of R scripts to run
r_scripts <- c("simulation1.R", "simulation2.R")

# Set up parallel processing
plan(multisession, workers = parallel::detectCores())

# Function to source a script
run_script <- function(script) {
  source(script)
}

# Run all scripts in parallel
furrr::future_map(r_scripts, run_script)

# Optionally, save results to a file or list if needed
