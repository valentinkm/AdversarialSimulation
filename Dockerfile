FROM rocker/verse:4.3.1

# Install necessary dependencies
RUN apt-get update && \
    apt-get install -y \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libgit2-dev \
        make \
        wget \
        slurm-client \
        texlive-base \
        texlive-latex-extra \
        texlive-fonts-recommended \
        texlive-science \
        texlive-xetex \
        texlive-bibtex-extra \
        texlive-pictures \
        texlive-latex-recommended \
        texlive-fonts-extra \
        lmodern

# Update tlmgr and install LaTeX packages
RUN tlmgr option repository ctan && \
    tlmgr update --self && \
    tlmgr install \
        float \
        geometry \
        caption \
        tikz \
        koma-script \
        pdflscape \
        afterpage \
        lscape \
        xcolor \
        booktabs \
        longtable \
        multirow \
        adjustbox \
        tcolorbox \
        titlesec \
        fontspec \
        pgfplots \
        fontawesome5

# Install Quarto CLI
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.3.450/quarto-1.3.450-linux-amd64.deb && \
    dpkg -i quarto-1.3.450-linux-amd64.deb && \
    rm quarto-1.3.450-linux-amd64.deb

# Install renv
RUN R -e "install.packages('renv', repos='http://cran.rstudio.com/')"

# Copy your simulation and thesis files into the Docker image
COPY VK/simulation /home/rstudio/simulation
COPY VK/thesis /home/rstudio/thesis

# Set the working directory to your thesis folder
WORKDIR /home/rstudio/thesis

# Restore the renv environment
RUN R -e "renv::restore()"

# Default command to render the thesis
CMD ["quarto", "render", "thesis.qmd"]