FROM rocker/shiny:latest

LABEL maintainer="mvergom@upv.edu.es"

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libxt-dev \
    libglpk-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libv8-dev \
    libudunits2-dev \
    libgmp3-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'DT', 'readxl', 'visNetwork', 'cyjShiny', 'dplyr', 'jsonlite'), repos = 'https://cloud.r-project.org')"

RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager', repos = 'https://cloud.r-project.org'); BiocManager::install('MORE', ask = FALSE)"
RUN R -e "BiocManager::install('clusterProfiler', ask = FALSE)"

COPY ./mi_app_shiny /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
