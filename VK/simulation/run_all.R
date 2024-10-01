# run_all.R

# Run all studies

# setup
source("setup.R")

# Run Study 1
source("study_1.R")

gc()

# Run Study 2
source("setup.R")
source("study_2.R")

gc()

# Run Study 3
source("setup.R")
source("study_3.R")
gc()

# close multisession workers
plan(sequential)

# Postprocessing
# source("main_postprocessing.R")