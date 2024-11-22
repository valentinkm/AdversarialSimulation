# AdversarialSimulation

[![Minimal Simulation Replication](https://github.com/valentinkm/AdversarialSimulation/actions/workflows/simulation.yml/badge.svg)](https://github.com/valentinkm/AdversarialSimulation/actions/workflows/simulation.yml)


## Thesis

The `VK/thesis` subdirectory contains all files generating the thesis by Kriegmair using simulation study results data from the `VK/simulation/results` directory. The results are available [here](https://valentinkm.github.io/AdversarialSimulation/thesis/thesis.pdf). For raw results files, please contact the authors directly as they exceed the GitHub LFS free storage limit.

The rendering of the thesis (including data visualization) is managed using Docker and `renv` with the `VK/thesis/renv.lock` file and can be replicated [here](https://github.com/valentinkm/AdversarialSimulation/actions/workflows/publish-thesis.yml) with a click on "Run workflow". Alternatively, with R (v. 4.4), [quarto](https://quarto.org/docs/get-started/) and `renv` installed use the following command inside the cloned repository to render the thesis locally:

```bash
cd VK/thesis
make
```

The `VK/simulation/results` directory contains all processed results of the simulations conducted by Collaborator A (Kriegmair). Results and reproducibility instructions of the studies conducted by Collaborator B (Kosanke) are available in the same repository in the `LK` subdirectory. For preprocessed (large) results files, please contact the authors directly as they exceed the GitHub LFS free storage limit.

The `VK/simulation` subdirectory contains the source code and Docker setup to replicate the studies conducted in the Adversarial Simulation by Collaborator A. A simple minimal proof of reproducibility (with two repetitions) can be triggered via a designated GitHub action [here](https://github.com/valentinkm/AdversarialSimulation/actions/workflows/simulation.yml), press "Run workflow" and view the results on a new pull request after about 20 Minutes. For a local replication use the following instructions:

1. Prerequisites: Ensure you have Docker installed on your system. You can download and install Docker [here](https://www.docker.com/get-started).

2. Clone the Repository: In your command line or terminal, clone the [Adversarial Simulation](https://github.com/valentinkm/AdversarialSimulation) repository to your local machine and navigate to the simulation directory:

    ```bash
    git clone https://github.com/valentinkm/adversarial-simulation.git
    ```
3. Inside the cloned repository build and run the simulation in a Docker container:

    ```bash
    cd VK/simulation
    make
    ```

4. Run Simulation in Docker Container:

    ```bash
    make
    ```
    This command will run the individual studies (study 1 & study 2) as well as the "joint" study (study 3) and save the results in the `results_replic/` directory. The default number of replications is set to 2 for each study. 

5. Additional Information: The R environment for the simulation is also managed using `renv`, and the exact package versions are recorded in the `VK/simulation/renv.lock` file.

## Preregistration

Preregistered Simulation protocol by Kosanke: [![DOI](https://zenodo.org/badge/754060177.svg)](https://zenodo.org/doi/10.5281/zenodo.10792671)

Preregistered Simulation protocol Kriegmair: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11458547.svg)](https://doi.org/10.5281/zenodo.11458547)

## Presentation

View the presentation slides [here](https://valentinkm.github.io/AdversarialSimulation/presentation/LIP_presentation.html#/title-slide)