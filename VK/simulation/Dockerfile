FROM ubuntu:22.04

# Install necessary dependencies and R
RUN apt-get update && \
    apt-get install -y software-properties-common dirmngr gnupg \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev make wget slurm-client && \
    apt-get clean

# Set non-interactive frontend to avoid tzdata prompt
ENV DEBIAN_FRONTEND=noninteractive

# Add CRAN repository and key
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | gpg --dearmor > /etc/apt/trusted.gpg.d/cran-ubuntu.gpg && \
    add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/' && \
    apt-get update && \
    apt-get install -y r-base

# renv
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/')"

# Copy all files from VK/simulation to the working directory
COPY VK/simulation /home/rstudio/simulation

# Set up working directory
WORKDIR /home/rstudio/simulation

# Check the contents of the working directory to ensure renv.lock is present
RUN ls -l /home/rstudio/simulation

# Restore environment
RUN R -e "renv::restore(library = '/usr/local/lib/R/site-library')"

# Default command to run all simulations using make
# CMD ["make"]
