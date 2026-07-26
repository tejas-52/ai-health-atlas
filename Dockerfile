# Production Dockerfile for AI Health Atlas
FROM rocker/shiny-verse:4.3.2

# Install system dependencies required for R packages (sf, RSQLite, openssl)
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libsqlite3-dev \
    libxml2-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install specific R package versions for the dashboard
RUN R -e "install.packages(c('bslib', 'shinyWidgets', 'leaflet', 'plotly', 'DT', 'RSQLite', 'sf', 'shinyjs', 'openssl'), repos='https://cloud.r-project.org/')"

# Remove default app and copy ours
RUN rm -rf /srv/shiny-server/*
COPY . /srv/shiny-server/

# Set file permissions for Shiny server user
RUN chown -R shiny:shiny /srv/shiny-server

# Expose Shiny port
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
