FROM node:12-buster-slim AS base

# https://mybinder.readthedocs.io/en/latest/tutorials/dockerfile.html
#ARG NB_USER=user
ARG NB_UID=1000
#ENV USER ${NB_USER}
#ENV NB_UID ${NB_UID}
#ENV HOME /home/${NB_USER}

#RUN adduser --disabled-password \
#    --gecos "Default user" \
#    --uid ${NB_UID} \
#    ${NB_USER}
COPY . ${HOME}
USER root
RUN chown -R ${NB_UID} ${HOME}

WORKDIR /build

RUN apt-get update && apt-get install -y  git build-essential cmake \
    sudo python libboost-all-dev python-dev libboost-python-dev python3-pip python3-pyqt5  pyqt5-dev-tools \
    zlib1g-dev libpng-dev libjpeg-dev libbz2-dev \
    libgl-dev libglu1-mesa libglu1-mesa-dev libgl1-mesa-dev libgl1-mesa-glx libgl1-mesa-dri freeglut3-dev \
    qtbase5-dev libqt5x11extras5-dev libqhull-dev libcgal-dev libann-dev bison libeigen3-dev flex

RUN git clone  https://github.com/google/draco.git  && cd draco  && \
    git checkout tags/1.3.6  && \
    mkdir build && cd build && cmake -DBUILD_SHARED_LIBS=ON ../ && make -j4 && make install

FROM base as python
RUN pip3 install virtualenv toml matplotlib pandas future qtconsole numpy jupyter jupyterlab && jupyter labextension install --no-build @jupyter-widgets/jupyterlab-manager
RUN pip3 install git+https://github.com/fredboudon/deploy.git --user
RUN pip3 install git+https://github.com/fredboudon/mtg.git

FROM python as plantgl
RUN git clone  https://github.com/jvail/plantgl.git  && cd plantgl && \
    git checkout plantgl-jupyter && \
    mkdir build && cd build && cmake -DCMAKE_BUILD_TYPE=Release ../ && \
    make -j4 && make install && \
    cd .. && pip3 install .

FROM plantgl as lpy
RUN git clone https://github.com/jvail/lpy.git && cd lpy && \
    git checkout plantgl-jupyter && \
    mkdir build && cd build && cmake -DCMAKE_BUILD_TYPE=Release -DPLANTGL_ROOT=../plantgl/build-cmake ../ -DBOOST_INCLUDEDIR=/usr/include/boost && \
    make -j4 && make install && \
    cd .. && pip3 install .

FROM lpy as plantgl-jupyter
RUN git clone https://github.com/jvail/plantgl-jupyter.git
RUN cd plantgl-jupyter && npm install && npm run build && \
    pip3 install -e . && \
    jupyter nbextension install --sys-prefix --overwrite --py pgljupyter && \
    jupyter nbextension enable --sys-prefix --py pgljupyter && \
    jupyter labextension install . && \
    python3 -m ipykernel install --name pgl-jupyter

FROM plantgl-jupyter as vmango
RUN cd ${HOME} && python3 setup.py develop
USER ${NB_UID}
