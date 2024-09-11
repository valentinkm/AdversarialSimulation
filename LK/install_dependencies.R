# Set the R mirror to the cloud mirror of RStudio
options(repos = "https://cloud.r-project.org/")

# Specify the libraries to load
libraries <- c(
  "gert", "here", "patchwork", "qrcode", "showtext", "svglite", "xaringanthemer",
  "furrr", "future", "future.batchtools", "future.apply",
  "lavaan", "purrr", "tidyverse", "knitr", "kableExtra", "sessioninfo", 
  "ggplot2", "data.table"
)

# Load the libraries
for (library_name in libraries) {
  if (!require(library_name, character.only = TRUE)) {
    install.packages(library_name)
    library(library_name, character.only = TRUE)
  }
}
