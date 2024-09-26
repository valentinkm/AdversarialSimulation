# Use rocker/r-ver:4.4.0 as the base image to match your local R version
FROM rocker/r-ver:4.4.0

# Set environment variables to avoid prompts during installation
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

# Install system dependencies
RUN apt-get update && apt-get install -y \
    software-properties-common \
    apt-utils \
    build-essential \
    wget \
    curl \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    pkg-config \
    ca-certificates \
    fontconfig \
    libcairo2-dev \
    libgdk-pixbuf2.0-dev \
    librsvg2-dev \
    xz-utils \
    perl \
    tar \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

# Install TeX Live 2023 (full installation)
RUN apt-get update && apt-get install -y texlive-full

# Install Quarto CLI version 1.4.549
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.549/quarto-1.4.549-linux-amd64.deb \
    && dpkg -i quarto-1.4.549-linux-amd64.deb \
    && rm quarto-1.4.549-linux-amd64.deb

# Install renv
RUN install2.r --error --skipinstalled renv

# Copy your project files into the Docker image
COPY VK/ /home/rstudio/VK/
COPY LK/ /home/rstudio/LK/

# Set the working directory to your thesis directory
WORKDIR /home/rstudio/VK/thesis

# Restore R packages using renv
RUN Rscript -e "renv::restore(prompt = FALSE)"

# Default command to render your thesis
CMD ["quarto", "render", "thesis.qmd"]
