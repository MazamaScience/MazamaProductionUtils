FROM rocker/tidyverse:3.5.1
MAINTAINER Jonathan Callahan <jonathan@mazamascience.com>

################################################################################
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    apt-utils \
    procps \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    optparse \
    sys

# Latest updates frrom GitHub
RUN installGithub.r \
    MazamaScience/MazamaCoreUtils \
    MazamaScience/MazamaProductionUtils


