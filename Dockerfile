FROM rstudio/plumber:v1.2.0

LABEL maintainer='Jason Brown'

RUN apt-get update && \
    apt-get install -y \
        build-essential=12.10ubuntu1 \
        libcurl4-openssl-dev=8.5.0-2ubuntu10.6 \
        libfontconfig1-dev=2.15.0-1.1ubuntu2 \
        libfreetype-dev=2.13.2+dfsg-1build3 \
        libfribidi-dev=1.0.13-3build1 \
        libgit2-dev=1.7.2+ds-1ubuntu3 \
        libharfbuzz-dev=8.3.0-2build2 \
        libjpeg-dev=8c-2ubuntu11 \
        libpng-dev=1.6.43-5build1 \
        libssl-dev=3.0.13-0ubuntu3.5 \
        libtiff5-dev=4.5.1+git230720-4ubuntu2.2 \
        libzmq3-dev=4.3.5-1build2 \
        zlib1g-dev=1:1.3.dfsg-3.1ubuntu2.1 \
        pandoc=3.1.3+ds-2

# Use apt-cache to get a dependency file version and add to the list above if necessary
# RUN apt-cache policy pandoc

ARG MRAN=2024-11-01

# Install package dependencies here
RUN R -e "install.packages('devtools', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('dplyr', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('htmltools', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('htmlwidgets', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('plotly', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('data.table', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('httr', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('purrr', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('rlang', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('manipulateWidget', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
# Install additional packages here
 && R -e "install.packages('tcplfit2', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('stringr', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('crosstalk', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')"

RUN R -e "install.packages('stringr', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')" \
 && R -e "install.packages('tidyr', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')"

RUN R -e "install.packages('ggiraph', repos='https://packagemanager.posit.co/cran/${MRAN}', method='libcurl')"

ADD . /tcplPlot
RUN R -e "devtools::install_local('/tcplPlot',upgrade = 'never')"


EXPOSE 8006

ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(rev(commandArgs())[1]);pr$handle('GET', '/_ping', function(req, res) {res$setHeader('Content-Type', 'application/json');res$status <- 200L;res$body <- '';res}); pr$run(host='0.0.0.0', port=8006, swagger = TRUE)"]

# Check the /_ping endpoint every 30 seconds.
# --start-period=30s <- add start period after timeout once docker is upgraded
HEALTHCHECK --interval=30s --timeout=5s \ 
  CMD curl --silent --fail http://0.0.0.0:8000/_ping || exit 1

CMD ["/tcplPlot/R/tcplplotapi.R"]
