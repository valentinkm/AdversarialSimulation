FROM rocker/r-ver:4.4.0

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

# Install system dependencies, including libgit2-dev for git2r support
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
    librsvg2-bin \
    xz-utils \
    perl \
    tar \
    gnupg \
    libgit2-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

# Install TeX Live for LaTeX support
RUN apt-get update && apt-get install -y texlive-full

# Install Quarto CLI
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.549/quarto-1.4.549-linux-amd64.deb \
    && dpkg -i quarto-1.4.549-linux-amd64.deb \
    && rm quarto-1.4.549-linux-amd64.deb

# Install R package renv
RUN install2.r --error --skipinstalled renv

# Verify that rsvg-convert is available and on the path
RUN which rsvg-convert

# Set up working directories and copy project files
COPY VK/ /home/rstudio/VK/
COPY LK/ /home/rstudio/LK/

WORKDIR /home/rstudio/VK/thesis

# Restore R dependencies using renv
RUN Rscript -e "renv::restore(prompt = FALSE)"

# Add the directory containing rsvg-convert to the system path (if necessary)
ENV PATH="/usr/bin:${PATH}"

# Render the Quarto document, disabling SVG-to-PDF conversion as a temporary fallback option
CMD ["quarto", "render", "thesis.qmd"]
