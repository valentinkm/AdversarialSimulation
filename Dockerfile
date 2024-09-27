FROM rocker/r-ver:4.4.0

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC


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
    && rm -rf /var/lib/apt/lists/*


RUN apt-get update && apt-get install -y texlive-full

RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.4.549/quarto-1.4.549-linux-amd64.deb \
    && dpkg -i quarto-1.4.549-linux-amd64.deb \
    && rm quarto-1.4.549-linux-amd64.deb

RUN install2.r --error --skipinstalled renv

COPY VK/ /home/rstudio/VK/
COPY LK/ /home/rstudio/LK/

WORKDIR /home/rstudio/VK/thesis

RUN Rscript -e "renv::restore(prompt = FALSE)"

CMD ["quarto", "render", "thesis.qmd"]
