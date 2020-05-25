FROM jvail/plantgl-jupyter:v3.3.1

ARG NB_USER=jovyan
ARG NB_UID=1000

RUN R -e "install.packages(c('VGAM', 'multcomp', 'combinat'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
COPY . /home/${NB_USER}
USER root
RUN chown -R ${NB_UID} /home/${NB_USER}
WORKDIR /home/${NB_USER}
RUN python3 setup.py develop

USER ${NB_USER}
