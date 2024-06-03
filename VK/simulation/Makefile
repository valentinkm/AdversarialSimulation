RUN_ALL_SCRIPT = run_all.R
LOG_FILE = results/simulation.log

all: run_simulation

run_simulation:
	Rscript $(RUN_ALL_SCRIPT) > $(LOG_FILE) 2>&1

clean:
	rm -f *.Rout $(LOG_FILE) results/*.rda
