FROM rocker/r-ver:4.2.0

# Install required packages and dependencies
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
    texlive-latex-base \
    texlive-latex-extra \
    texlive-fonts-recommended \
    && rm -rf /var/lib/apt/lists/*


# Install Quarto
RUN wget -O quarto.deb https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    gdebi --non-interactive quarto.deb && \
    rm quarto.deb

# Install R packages
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


RUN Rscript -e 'library(ggh4x)'

# Copy the thesis files
COPY VK/thesis/ thesis/
COPY VK/simulation/results/ simulation/results/

# Copy the bib and csl files into VK directory
COPY VK/bibliography.bib .
COPY VK/apa.csl .

WORKDIR /thesis
RUN quarto render
