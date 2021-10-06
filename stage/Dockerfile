FROM rocker/tidyverse:latest
RUN install2.r rio plumber bigrquery
COPY ["./api.R", "./api.R"]
ENTRYPOINT exec R -e "plumber::pr_run(plumber::pr('api.R'),port=as.numeric(Sys.getenv('PORT')))"
