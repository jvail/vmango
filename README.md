# setup env

```console
conda create -y -n lpy3 -c fredboudon -c conda-forge openalea.lpy openalea.mtg r
```

# build mangosim

```console
python setup.py develop
```

# run mangosim

```console
cd src\openalea\vmango\simulation
ipython
%gui qt5
%run runsimu.py
```


# jupyter

```console
conda create -n jupyter -y -c fredboudon -c conda-forge openalea.lpy openalea.mtg r jupyterlab
```