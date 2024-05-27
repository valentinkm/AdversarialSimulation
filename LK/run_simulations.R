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
r_scripts <- c("LK/simulation1.R", "LK/simulation1b.R")

# Set up processing
plan(multisession, workers = parallel::detectCores())

#tweak(batchtools_slurm)
#plan(tweak(batchtools_slurm,
#           workers = 100,
#           template = missing,
#           resources=list(ncpus=1,
#                          memore='700m',
#                          walltime=6600,
#                          partition=c('gpu'))
#           ))

# Function to source a script
run_script <- function(script) {
  system(paste("Rscript", script))
}

# Run all scripts
future_map(r_scripts, run_script)
