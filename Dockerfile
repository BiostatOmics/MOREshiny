# Base R Shiny image
FROM rocker/shiny

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
        libzmq3-dev \
        libglpk-dev && \
    rm -rf /var/lib/apt/lists/*
    
    
# Make a directory in the container
RUN mkdir /home/shiny-app

# Install Renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Copy Renv files and Shiny app
WORKDIR /home/shiny-app/

RUN mkdir -p renv
COPY app.R app.R
COPY helpers.R helpers.R
COPY renv.lock renv.lock
COPY www www
COPY .Rprofile  .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Restore the R environment
RUN R -e "renv::restore()"

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
#CMD Rscript /home/shiny-app/app.R
CMD ["Rscript", "/home/shiny-app/app.R"]