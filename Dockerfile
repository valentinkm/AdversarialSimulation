# Use rocker/r-ver as the base image, which includes R but not TeX Live
FROM rocker/r-ver:4.2.2

# Set the build date (optional)
ARG BUILD_DATE=2024-05-28

# Set the working directory
WORKDIR /home/rstudio

# Update packages and install additional system dependencies
RUN apt-get update -y && apt-get install -y \
    rsync \
    wget \
    gdebi-core \
    libcurl4-openssl-dev \
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
    && rm -rf /var/lib/apt/lists/*

# Install TinyTeX
RUN Rscript -e "install.packages('tinytex'); tinytex::install_tinytex()"

# Ensure tlmgr is in the PATH
ENV PATH="/root/bin:${PATH}"

# Install additional LaTeX packages using TinyTeX
RUN tlmgr install \
    collection-latexrecommended \
    collection-latex \
    collection-fontsrecommended \
    collection-fontsextra \
    collection-pictures \
    pgf \
    tikzsymbols \
    xkeyval

# Install required R packages
RUN install2.r --error --skipinstalled \ 
    gert \ 
    here \ 
    patchwork \ 
    qrcode \ 
    showtext \ 
    svglite \ 
    xaringanthemer \
    furrr \ 
    future \
    future.batchtools \ 
    future.apply \
    remotes \
    lavaan \
    purrr \
    tidyverse \
    knitr \
    kableExtra \
    sessioninfo \
    rmarkdown \
    dplyr \
    tidyr \
    ggplot2 \
    data.table \
    ggh4x

# Install Quarto CLI
RUN wget https://quarto.org/download/latest/quarto-linux-amd64.deb \
    && dpkg -i quarto-linux-amd64.deb \
    && rm quarto-linux-amd64.deb

# Copy your project files into the Docker image
COPY VK/ /home/rstudio/VK/
COPY LK/ /home/rstudio/LK/

# Set the working directory to your thesis directory
WORKDIR /home/rstudio/VK/thesis

CMD ["quarto", "render", "thesis.qmd"]
