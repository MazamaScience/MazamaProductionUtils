FROM rocker/tidyverse:3.5.1
MAINTAINER Jonathan Callahan <jonathan@mazamascience.com>

################################################################################
# Extra R packages from CRAN
RUN install2.r --error -r "https://cran.rstudio.com" \
    sys

# Latest updates frrom GitHub
RUN installGithub.r \
    mazamascience/mazamaproductionutils


