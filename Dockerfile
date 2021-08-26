FROM rocker/rstudio

RUN /rocker_scripts/install_shiny_server.sh
COPY . /pkgs/Future

RUN Rscript -e "install.packages(c('remotes'))"
RUN Rscript -e "remotes::install_deps('/pkgs/Future', dependencies = TRUE)"
RUN Rscript -e "install.packages('/pkgs/Future/', repos = NULL)"

RUN mkdir -p /srv/shiny-server
RUN echo "Future::runFutureApp()" >> /srv/shiny-server/app.R

