# Base image
FROM rocker/r-ver:4.2.0

# Install system dependencies and LaTeX
RUN apt-get update && apt-get install -y \
    wget \
    gdebi-core \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libtiff5-dev \
    libjpeg-dev \
    zlib1g-dev \
    pandoc \
    texlive-full \
    texlive-fonts-recommended \
    texlive-latex-extra \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto CLI
RUN wget -O quarto.deb https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    gdebi --non-interactive quarto.deb && \
    rm quarto.deb

# Install renv
RUN R -e "install.packages('renv', repos='https://cran.rstudio.com')"

# Copy your project files into the Docker image
COPY VK/ /VK/
COPY LK/ /LK/

# Set working directory where renv.lock is located
WORKDIR /VK/thesis

# Restore R packages using renv
RUN R -e "renv::restore()"

# Default command to render your thesis
CMD ["quarto", "render", "thesis.qmd"]