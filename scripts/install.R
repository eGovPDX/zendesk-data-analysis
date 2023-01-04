#### PACKAGES ####
# This script is run as part of the Docker build stage,
# so packages don't have to be re-installed every time.

install.packages(c(
  "janitor"
))
