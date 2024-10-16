# AdversarialSimulation

## Preregistrations

Preregistered Simulation protocol by Kosanke: [![DOI](https://zenodo.org/badge/754060177.svg)](https://zenodo.org/doi/10.5281/zenodo.10792671)

Preregistered Simulation protocol Kriegmair: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11458547.svg)](https://doi.org/10.5281/zenodo.11458547)


## Thesis by Kriegmair

The `VK/thesis` subdirectory contains all files generating the Master's thesis by Kriegmair using data from the `VK/simulation/results` directory.

## Simulations by Kriegmair

The `VK/simulation` subdirectory contains the code and Docker setup to replicate the studies conducted in the adversarial simulation by Kriegmair. For local (demonstrative or full) replication use the following instructions:

### Prerequisites

- **Docker**: Ensure you have Docker installed on your system. You can download and install Docker [here](https://www.docker.com/get-started).

### Setup Instructions

#### 1. Clone the Repository

First, clone this repository to your local machine and navigate to the simulation directory:

```bash
git clone https://github.com/yourusername/adversarial-simulation.git
cd adversarial-simulation/VK/simulation
```

#### 2. Build the Docker Image

```bash
docker build -t simulation_image .
```

#### 3. Run Simulation in Docker Container

```bash
docker run --rm -v "$(pwd)/results_replic:/simulation/results_replic" simulation_image
```

This command will run the individual studies (study 1 & study 2) as well as the "joint" study (study 3) and save the results in the `results_replic/` directory. The default number of replications is set to 2 for each study. 

#### 4. Additional Information

The R environment is managed using `renv`, and the exact package versions are recorded in the renv.lock file.