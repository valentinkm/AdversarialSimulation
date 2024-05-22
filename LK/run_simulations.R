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
install.packages("future.batchtools")
library(future.batchtools)

plan(tweak(batchtools_slurm, workers = parallel::detectCores()))
#Erstmal: Skript läuft durch mit multisession, wir sagen Michael Krause was wir wollen, damit wir über Tardis an unsere Ergbnisfiles bekommen

#Tabellen nach rds dateien noch mit knitr::kable schön machen für markdown, eventuell kableExtra, auf cran im internet pdf
#Futures: primär repetitions! Nicht den rest

#Berechnungen für metrics ohne future. Future nur für repetitions (was Aaron gemacht hat und für model estimation)!!
#Alles andere normale map() funktionen, dementsprechend auch da nur seed in furrr.options

#seeds und seed jetzt nur so wie in simulation 1

# Function to source a script
run_script <- function(script) {
  source(script)
}

# Run all scripts in parallel
furrr::future_map(r_scripts, run_script)

# Optionally, save results to a file or list if needed
