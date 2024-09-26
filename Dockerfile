# Use rocker/verse, which includes R, RStudio, and TeX Live
FROM rocker/verse:4.2.2

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

# Install additional LaTeX packages
RUN tlmgr install \
    collection-latexrecommended \
    libertine \
    pdfpages \
    lualatex-math \
    luatexbase \
    titling \
    pdfx \
    luatex85 \
    colorprofiles \
    multirow \
    float \
    pgf

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

# Install Quarto CLI (already included in rocker/verse, but update if necessary)
RUN /rocker_scripts/install_quarto.sh

# Copy your project files into the Docker image
COPY VK/ /home/rstudio/VK/
COPY LK/ /home/rstudio/LK/

# Set the working directory to your thesis directory
WORKDIR /home/rstudio/VK/thesis

CMD ["quarto", "render", "thesis.qmd"]
