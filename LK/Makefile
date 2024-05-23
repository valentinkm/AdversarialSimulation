# Define the R scripts to run
R_SCRIPTS := simulation1.R simulation2.R

# Default target: run all simulations
all: run_simulations

# Target to run all R scripts
run_simulations:
	@for script in $(R_SCRIPTS); do \
		Rscript $$script; \
	done

