FROM rocker/r-ver:4.4.0

# Install system dependencies for the R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    make

# Install renv
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/')"

# Copy the project files into the Docker image
COPY VK/simulation /home/rstudio/simulation

# Set the working directory
WORKDIR /home/rstudio/simulation

# Restore the renv environment
RUN R -e "renv::restore(library = '/usr/local/lib/R/site-library')"

# Default command to run all simulations using make
CMD ["make"]