FROM rocker/r-ver:4.2.0

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
    texlive-latex-base \
    texlive-latex-extra \
    texlive-fonts-recommended \
    texlive-xetex \
    && rm -rf /var/lib/apt/lists/*

RUN wget -O quarto.deb https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    gdebi --non-interactive quarto.deb && \
    rm quarto.deb

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

RUN tlmgr option repository https://mirror.ctan.org/systems/texlive/tlnet && \
    tlmgr update --self --all

WORKDIR /thesis

COPY VK/thesis/ thesis/
COPY VK/simulation/results/ simulation/results/
COPY VK/bibliography.bib .
COPY VK/apa.csl .


RUN quarto render
