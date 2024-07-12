# Use the rocker/verse image as the base image
FROM rocker/verse:latest

# Install necessary R packages
RUN R -e "install.packages(c('dplyr', 'tidyr', 'kableExtra', 'ggplot2', 'knitr'), repos='http://cran.rstudio.com/')"

# Install Quarto
RUN wget https://quarto.org/download/latest/quarto-linux-amd64.deb
RUN dpkg -i quarto-linux-amd64.deb

# Create directories and copy the necessary files into the container
RUN mkdir -p /root/project/analyze_results
RUN mkdir -p /root/project/simulation/results

COPY analyze_results/analyze_results_study1.qmd /root/project/analyze_results/
COPY analyze_results/analyze_results_study2.qmd /root/project/analyze_results/
COPY simulation/results/summary_study1.rds /root/project/simulation/results/
COPY simulation/results/summary_study2.rds /root/project/simulation/results/

# Set the working directory
WORKDIR /root/project/analyze_results

# Render the .qmd files
CMD ["sh", "-c", "quarto render analyze_results_study1.qmd && quarto render analyze_results_study2.qmd"]
