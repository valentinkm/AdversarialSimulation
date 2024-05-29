libraries <- c("purrr", "future", "furrr","future.batchtools")
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
r_scripts <- c("simulation1.R", "simulation1b.R", "simulation2.R", "simulation3.R", "simulation4.R", "simulation4a.R")

# Set up processing with multisession
plan(multisession, workers = parallel::detectCores())


# Function to source a script
run_script <- function(script) {
  system(paste("Rscript", script))
}

# Run all scripts
future_map(r_scripts, run_script)
