# Use the R 4.4 base image from Rocker
FROM rocker/r-ver:4.4

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff5-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libglpk40 \
    libglpk-dev \
    libmagick++-dev \
    libbz2-dev \
    liblzma-dev \
    libzstd-dev \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /simulation

# Copy the entire project into the image
COPY . /simulation

# Install renv and restore packages
RUN Rscript -e 'install.packages("renv"); renv::restore()'

# Set the entrypoint to run your simulation
CMD ["Rscript", "run_all.R"]
