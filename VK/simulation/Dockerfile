FROM rocker/r-ver:4.4.0

# system dependencies for the R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    make

# renv
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/')"

# Copy all files from VK/simulation to the working directory
COPY VK/simulation /home/rstudio/simulation

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