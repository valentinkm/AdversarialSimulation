FROM rocker/verse:4.2.2
ARG BUILD_DATE=2024-05-28
WORKDIR /home/rstudio
RUN apt-get update -y && apt-get install -y rsync
RUN tlmgr install collection-latexrecommended libertine pdfpages lualatex-math luatexbase titling pdfx luatex85 colorprofiles multirow float
RUN /rocker_scripts/install_quarto.sh
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
  sessioninfo
RUN installGithub.r \ 
  aaronpeikert/repro@5075336
