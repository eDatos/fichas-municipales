FROM rocker/shiny-verse:4.1.2

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    pandoc 
  
# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "rmarkdown", \
              "ggplot2", \
              "knitr", \
              "data.table", \
              "plotly", \
              "scales", \
              "htmlwidgets", \
              "sf", \
              "fuzzyjoin", \
              "XML", \
              "tidyverse", \
              "tmap", \
              "magrittr", \
              "ggpol", \
              "jsonlite", \
              "tibble", \
              "plyr", \
              "dplyr", \
              "funr", \
              "shinyscreenshot", \
              "xml2", \
              "tidyverse", \
              "bslib", \
              "RCurl", \
              "webshot" \
            ) \
          , repos = "https://cran.rediris.es")'

# copy the app directory into the image
# COPY ./* /srv/shiny-server/sta-mun
COPY ./conf/shiny-server.conf /etc/shiny-server

RUN mkdir -p /usr/local/lib/R/site-library
# run app
#CMD ["R", "-e", "shiny::runApp()"]

CMD ["/usr/bin/shiny-server"]
