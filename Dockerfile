FROM rocker/verse:4.1.3
ARG BUILD_DATE=2022-12-13
WORKDIR /home/rstudio
RUN apt-get update -y && apt-get install -y rsync
RUN tlmgr install collection-latexrecommended libertine pdfpages lualatex-math luatexbase titling pdfx luatex85 colorprofiles
RUN /rocker_scripts/install_quarto.sh
RUN install2.r --error --skipinstalled \ 
  gert \ 
  here \ 
  patchwork \ 
  qrcode \ 
  showtext \ 
  svglite \ 
  xaringanthemer
RUN installGithub.r \ 
  aaronpeikert/repro@5075336