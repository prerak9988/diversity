# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
# install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    # libcairo2-dev \
    # libsqlite3-dev \
    # libmariadbd-dev \
    # libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean


## create working directory
WORKDIR /diversity

# copy necessary files
#COPY --chown=node:node . ./
COPY renv.lock /diversity/renv.lock
COPY app /diversity/app


# install renv & restore packages
# Also, Installing packages via Cloud Build doesn't support docker buildkit. Hence using this approach
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/diversity/app', host = '0.0.0.0', port = 3838)"]