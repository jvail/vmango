from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from builtins import map
from builtins import str
from builtins import range
from past.utils import old_div
from importlib import reload
import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
import os
from os.path import join
from vplants.mangosim.util_path import get_glm_mtg_repository, mtgfname

lsysfile = 'mango_glm.lpy'


def generate_mtg(trees = list(range(5)), params = dict()):
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


def generate_mtgs(trees = list(range(5)), 
                  params = dict(), 
                  seeds = list(range(10)), 
                  optionname = None):
    outputdir = get_glm_mtg_repository(trees, params, optionname, lsysfile)
    if not os.path.exists(outputdir): os.makedirs(outputdir)
    for seed in seeds:
      fname = mtgfname.format(str(seed).zfill(4))
      if not os.path.exists(join(outputdir,fname)):  
        print('Generate trees with seed',seed)
        nparams = params.copy()
        nparams.update({'SEED':seed})
        try :
            mtg = generate_mtg(trees, nparams)
        except :
            import traceback
            traceback.print_exc()
            continue      
        dump_obj(mtg,fname, outputdir)
        print("Write "+repr(str(join(outputdir,fname))))


def generate_all(nb = 1000, withindelaymethods = [eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin]):
    for withindelaymethod in withindelaymethods:
        generate_mtgs(params = {'WITHINDELAYMETHOD' : withindelaymethod}, seeds = list(range(nb)))

def generate_all_restriction(nb = 1000):
    for withindelaymethod in withindelaymethods:
        generate_mtgs(params = {'WITHINDELAYMETHOD' : withindelaymethod}, seeds = list(range(nb)))


def _generate(params):
        print('python process_glm.py '+params)
        os.system('python process_glm.py '+params)

def process_set_of_simulations(paramvalueslist):
    from multiprocessing import Pool

    paramvalueslist = [' '.join(map(str,param)) for param in paramvalueslist]
    #print paramvalueslist
    # _generate(paramvalueslist[2])
    pool = Pool(processes=26)
    pool.map(_generate,paramvalueslist)


def process_null_models(nb=1000):
    import itertools
    params = list(itertools.product(list(range(nb)),['ESTIMATIONTYPE'],['eNullGlm'],['WITHINDELAYMETHOD'],['eMonthMultiVariateForWithin', 'eDeltaMultiVariateForWithin', 'eDeltaPoissonForWithin']))

    process_set_of_simulations(params)


def process_restricted_models(nb=1000):
    import itertools
    params = list(itertools.product(list(range(nb)),['ESTIMATIONTYPE'],['eSelectedGlm'],['WITHINDELAYMETHOD'],['eMonthMultiVariateForWithin'],['FACTORRESTRICTION'],['eBurstDateRestriction', 'ePositionARestriction', 'ePositionAncestorARestriction', 'eNatureFRestriction']))

    process_set_of_simulations(params)


if __name__ == '__main__' :
    import sys
    if len(sys.argv) > 1:
        seed = int(sys.argv[1])
        params = dict()
        paramcmd = sys.argv[2:]
        assert len(paramcmd) % 2 == 0
        for i in range(old_div(len(paramcmd),2)):
            params[paramcmd[2*i]] = eval(paramcmd[2*i+1])
        generate_mtgs(seeds=[seed],params=params)
    else:
        process_restricted_models(1000)
        #process_null_models()
