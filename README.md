[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fredboudon/vmango.git/add-binder)

# setup env

```console
conda create -y -n vmango -c fredboudon -c conda-forge openalea.lpy openalea.mtg r toml future rpy2
conda activate vmango
```

# build V-Mango

dev version - use imports from source tree

```console
python setup.py develop
```

use imports from openalea.vmango site package

```console
python setup.py install
```

# run V-Mango in lpy gui
```console
lpy
```
* open src\openalea\vmango\simulation\mango_simulation.lpy in lpy
* click 'Rewind'
* click 'Run'

# run V-Mango in ipython

```console
cd src\openalea\vmango\simulation
ipython
%gui qt5
from openalea.lpy import *
l = Lsystem('src/openalea/vmango/simulation/mango_simulation.lpy')
lstring = l.iterate()
l.plot(lstring)
```

# jupyter

```console
conda activate vmango
conda install -y -c conda-forge jupyterlab
python -m ipykernel install --user --name vmango
jupyter lab
```
