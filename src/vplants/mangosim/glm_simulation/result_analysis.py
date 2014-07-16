from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.tools import *

def generate_mtg():
    from openalea.lpy import Lsystem
    from openalea.mtg.algo import union
    g = None
    for tree in xrange(5):
        l = Lsystem('mango_glm.lpy', {'TREE':tree,'TIMESTEP': 30} )
        l.iterate()
        resmtg = l.resultmtg
        if g is None:
            g = resmtg
        else:
            g = union(g,resmtg)
    return g

g = generate_mtg()
setMtgStyle(eSimulatedMtg)
check_burst_date_distribution(g)
#check_burst_dates(g)n              