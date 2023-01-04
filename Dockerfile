FROM rocker/tidyverse:4.2.2

WORKDIR /app/

COPY ./scripts/install.R /app/scripts/
RUN ["Rscript", "/app/scripts/install.R"]

COPY ./scripts/ /app/scripts/

WORKDIR /app/scripts/
