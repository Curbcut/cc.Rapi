# Use the official R base image
FROM rocker/r-ver:latest

# Install required system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libgit2-dev \
    libpq-dev \
    libxml2-dev

# Install remotes package to allow installation from GitHub
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"

# Install cc.Rapi from GitHub and other required packages
RUN R -e "remotes::install_github('Curbcut/cc.Rapi')"

# Expose the port the API will run on
EXPOSE 8000

CMD ["R", "-e", "cc.Rapi::run_api(host = '0.0.0.0', port = 8000)"]
