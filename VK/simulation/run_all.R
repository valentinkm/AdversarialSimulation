# run_all.R
source("renv/activate.R")

# Run all studies

# setup
source("setup.R")

# Run Study 1
source("study_1.R")

rm(list = ls(all.names = TRUE))
gc()

# Run Study 2
source("setup.R")
source("study_2.R")

rm(list = ls(all.names = TRUE))
gc()

# Run Study 3
source("setup.R")
source("study_3.R")

rm(list = ls(all.names = TRUE))
gc()

# close multisession workers
plan(sequential)

# Postprocessing
source("main_postprocessing.R")