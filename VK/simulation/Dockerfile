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

# files
COPY . /home/rstudio/simulation

# setup working directory
WORKDIR /home/rstudio/simulation

# restore environment
RUN R -e "renv::restore(library = '/usr/local/lib/R/site-library')"

# Default command to run all simulations using make
CMD ["make"]