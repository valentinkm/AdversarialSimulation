# Variables
QMD_FILE = thesis.qmd
R_SCRIPT = Rscript

# Default Target
all: restore render

# Restore R environment
restore:
	@echo "Restoring R environment with renv..."
	$(R_SCRIPT) -e "renv::restore()"

# Render the Quarto file
render:
	@echo "Rendering Quarto document..."
	quarto render $(QMD_FILE)

# Clean target
clean:
	@echo "Cleaning up intermediate files..."
	rm -rf *.log *.aux *.out