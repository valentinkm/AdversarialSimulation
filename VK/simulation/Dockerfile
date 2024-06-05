FROM ubuntu:22.04

# Install R and dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    make \
    wget

# Install R from CRAN to get the latest version
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 'E298A3A825C0D65DFD57CBB651716619E084DAB9' && \
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/' && \
    apt-get update && \
    apt-get install -y r-base

# renv
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/')"

# Copy all files from VK/simulation to the working directory
COPY VK/simulation /home/rstudio/simulation

# Copy the batchtools SLURM template
COPY VK/simulation/.batchtools.slurm.singularity.tmpl /home/rstudio/simulation/.batchtools.slurm.singularity.tmpl

# setup working directory
WORKDIR /home/rstudio/simulation

# Check the contents of the working directory to ensure renv.lock is present
RUN ls -l /home/rstudio/simulation

# restore environment
RUN R -e "renv::restore(library = '/usr/local/lib/R/site-library')"

# Default command to run all simulations using make
CMD ["make"]

# 1) put slurm template in the docker.

# 2) pull the container from github published container as apptainer

# 3) sbatch -c 1 --mem 16gb --time 7-0:0:0 --partition= long --wrap "apptainer run foo.sif"
# this is the command to run the container on the cluster