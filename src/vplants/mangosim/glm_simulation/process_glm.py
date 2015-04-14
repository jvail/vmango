import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
import os
from os.path import join
from vplants.mangosim.util_path import get_glm_mtg_repository, mtgfname

lsysfile = 'mango_glm.lpy'


def generate_mtg(trees = range(5), params = dict()):
    from openalea.lpy import Lsystem
    from openalea.mtg.algo import union
    g = None
    for tree in trees:
        nparams = params.copy()
        nparams.update({'TREE':tree,'TIMESTEP': 180,'EXPORT_TO_MTG':True})
        l = Lsystem(lsysfile,  nparams)
        l.iterate()
        resmtg = l.resultmtg
        if g is None:
            g = resmtg
        else:
            g = union(g,resmtg)
    return g


def generate_mtgs(trees = range(5), 
                  params = dict(), 
                  seeds = range(10), 
                  optionname = None):
    outputdir = get_glm_mtg_repository(trees, params, optionname, lsysfile)
    if not os.path.exists(outputdir): os.makedirs(outputdir)
    for seed in seeds:
      fname = mtgfname.format(str(seed).zfill(4))
      if not os.path.exists(join(outputdir,fname)):  
        print 'Generate trees with seed',seed
        nparams = params.copy()
        nparams.update({'SEED':seed})
        try :
            mtg = generate_mtg(trees, nparams)
        except :
            import traceback
            traceback.print_exc()
            continue      
        dump_obj(mtg,fname, outputdir)
        print "Write "+repr(str(join(outputdir,fname)))


def generate_all(nb = 1000, withindelaymethods = [eMonthGlmForWithin, eDeltaGlmForWithin, eDeltaPoissonForWithin]):
    for withindelaymethod in withindelaymethods:
        generate_mtgs(params = {'WITHINDELAYMETHOD' : withindelaymethod},seeds = range(nb))



if __name__ == '__main__' :
   generate_all(withindelaymethods =[eDeltaGlmForWithin])
