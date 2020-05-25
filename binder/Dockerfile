FROM jvail/plantgl-jupyter:v3.2
COPY . /home/jovyan
USER root
RUN chown -R 1000 /home/jovyan
RUN cd /home/jovyan && python3 setup.py develop
USER jovyan
