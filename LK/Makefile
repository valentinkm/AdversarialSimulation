# Define the R scripts to run
R_SCRIPTS := simulation1.R simulation1b.R simulation2.R simulation3.R simulation4.R simulation4a.R

# Define the final Quarto document
FINAL_DOC := ../results.qmd

# Default target: run all simulations and render the final document
all: run_simulations render_document

# Target to run all R scripts
run_simulations:
	Rscript -e "source('run_simulations.R')"

# Target to render the final Quarto document
render_document:
	@echo "Rendering final Quarto document..."
	quarto render $(FINAL_DOC)
