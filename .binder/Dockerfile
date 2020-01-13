FROM rocker/binder:3.6.0
LABEL maintainer='Unknown'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN install2.r --error \
    --deps TRUE \
    igraph \
    visNetwork \
    here 

#RUN wget https://github.com/laderast/igraphTutorial/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2020-01-13/')); devtools::install_deps(dependencies=TRUE)"

#RUN rm DESCRIPTION.1; exit 0
