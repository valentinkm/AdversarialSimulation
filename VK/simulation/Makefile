RUN_ALL_SCRIPT = run_all.R
RESULTS_FILE = simulation_results.rda

all: run_simulation

run_simulation:
	Rscript $(RUN_ALL_SCRIPT)

clean:
	rm -f *.Rout $(RESULTS_FILE)
