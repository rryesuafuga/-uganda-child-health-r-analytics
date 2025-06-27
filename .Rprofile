FROM rocker/shiny:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'plotly', \
    'tidyverse', \
    'DT', \
    'randomForest', \
    'glmnet', \
    'MatchIt', \
    'broom', \
    'viridis' \
), repos='https://cloud.r-project.org/')"

# Create app directory
RUN mkdir -p /srv/shiny-server/app

# Copy app files
COPY app.R /srv/shiny-server/app/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/app
RUN chmod -R 755 /srv/shiny-server/app

# Create log directory with proper permissions
RUN mkdir -p /var/log/shiny-server
RUN chown -R shiny:shiny /var/log/shiny-server

# Create a custom shiny-server.conf for Hugging Face Spaces
RUN echo "run_as shiny; \
server { \
  listen 7860; \
  location / { \
    site_dir /srv/shiny-server/app; \
    log_dir /var/log/shiny-server; \
    directory_index on; \
  } \
}" > /etc/shiny-server/shiny-server.conf

# Make sure the shiny user owns the config
RUN chown shiny:shiny /etc/shiny-server/shiny-server.conf

# Expose port
EXPOSE 7860

# Switch to shiny user
USER shiny

# Run shiny server
CMD ["/usr/bin/shiny-server"]
