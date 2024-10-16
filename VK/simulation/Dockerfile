# Use the R 4.4 base image from Rocker
FROM rocker/r-ver:4.4

# Accept UID and GID as build arguments, defaulting to 1000 if not provided
ARG USER_ID=1000
ARG GROUP_ID=1000

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

# Create a group and user with the specified UID and GID
RUN addgroup --gid $GROUP_ID appuser && \
    adduser --disabled-password --gecos "" --uid $USER_ID --gid $GROUP_ID appuser

# Set the working directory
WORKDIR /simulation

# Copy the entire project into the image
COPY . /simulation

# Change ownership of the /simulation directory to appuser
RUN chown -R appuser:appuser /simulation

# Set environment variables for renv to avoid permission issues
ENV RENV_PATHS_ROOT=/simulation/renv
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

# Install renv and restore packages
RUN Rscript -e 'install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore()'

# (Optional) List installed packages
RUN Rscript -e 'installed.packages()[,c("Package", "Version")]'

# Switch to the new user
USER appuser

# Set the entrypoint to run your simulation
CMD ["Rscript", "run_all.R"]
