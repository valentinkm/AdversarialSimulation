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
r_scripts <- c("simulation1.R", "simulation1b.R")

# Set up processing
##Terminal innerhalb tardis:
##tmux
## srun -c40 --mem 30gb --pty --time 7-0:0:0 --partition long /bin/bash      #zum testen: --time 24:0:0 --partition test austauschen
###(strg c bricht ab)

##Warten bis Job fertig ist, dann Sprung in shell, da einfach, wenn ohne Docker image: 
#Rscipt run_simulations.R

#Mit docker, wieder im terminal
#ins directory rein navigaten

#apptainer shell datei.sif
#Dann Rscript run_simulations.R

###############falls connection weg ist:
#Strg b+d und ein anderer noch zum hin und her springen
#tmux 
#########################

##alles zusammen in einem, geht auch!!!!!
##im Terminal, im login node:
##in den LK Ordner rein mit cd
##sbatch -c32 --mem 100gb --wrap "apptainer exec datei.sif Rscript run_simulations.R"

##es erscheint über ls eine file, die den terminal output anzeigt (library output) mit:
###cat filename (slurm...)

##Am Ende downloaden immer einfach die ERgebnisfiles in Tardis. Ich kann sobald es läuft alles schließen!



plan(multisession, workers = parallel::detectCores())


# Function to source a script
run_script <- function(script) {
  system(paste("Rscript", script))
}

# Run all scripts
future_map(r_scripts, run_script)
