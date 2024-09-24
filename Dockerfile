# Base image
FROM rocker/r-ver:4.2.0

# required system dependencies and LaTeX
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

# install quarto
RUN wget -O quarto.deb https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    gdebi --non-interactive quarto.deb && \
    rm quarto.deb

# install R libraries
RUN Rscript -e 'install.packages(c( \
    "knitr", \
    "rmarkdown", \
    "dplyr", \
    "tidyr", \
    "kableExtra", \
    "ggplot2", \
    "data.table", \
    "ggh4x" \
  ), repos = "https://cran.rstudio.com")'


# copy all files belonging to vk
COPY VK/ /VK/

# copy results report of Kosanke
COPY LK/results.qmd /LK/results.qmd
COPY LK/images/ /LK/images/

# working directory at root
WORKDIR /

# RUN quarto render thesis.qmd
