FROM rocker/tidyverse:latest
RUN install2.r rio plumber bigrquery
COPY ["./api.R", "./api.R"]
ENTRYPOINT exec R -e "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"
