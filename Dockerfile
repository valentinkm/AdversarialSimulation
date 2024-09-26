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
    texlive-fonts-recommended \
    texlive-latex-extra \
    && rm -rf /var/lib/apt/lists/*

RUN wget -O quarto.deb https://quarto.org/download/latest/quarto-linux-amd64.deb && \
    gdebi --non-interactive quarto.deb && \
    rm quarto.deb

RUN R -e "install.packages('renv', repos='https://cran.rstudio.com')"

COPY VK/ /VK/
COPY LK/ /LK/

WORKDIR /VK/thesis

RUN R -e "renv::restore()"

CMD ["quarto", "render", "thesis.qmd"]