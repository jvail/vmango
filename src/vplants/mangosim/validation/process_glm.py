import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
import os
from os.path import join, basename, dirname
from vplants.mangosim.util_path import get_glm_mtg_repository, mtgfname

lsysfile = '../simulation/mango_simulation.lpy'


def generate_mtg(trees = range(3), params = dict()):
    from openalea.lpy import Lsystem
    from openalea.mtg.algo import union
    g = None
    for tree in trees:
        print 'Generate tree', tree
        nparams = params.copy()
        nparams.update({'TREE':tree,'TIMESTEP': 180,'EXPORT_TO_MTG':True, 'WITH_GLM':True})
        l = Lsystem(basename(lsysfile),  nparams)
        l.iterate()
        resmtg = l.resultmtg
        if g is None:
            g = resmtg
        else:
            g = union(g,resmtg)
    return g


def generate_mtgs(trees = range(3), 
                  params = dict(), 
                  seeds = range(10), 
                  optionname = None):
    cwd = os.getcwd()
    os.chdir(dirname(lsysfile))
    print os.getcwd()

    outputdir = get_glm_mtg_repository(params, optionname)
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
        else:
            print join(outputdir,fname),'already exists.'

    os.chdir(cwd)


def generate_all(nb = 10):
    generate_mtgs(seeds = range(nb))

def generate_all_restriction(nb = 1000):
    generate_mtgs(seeds = range(nb))


def _generate(params):
    cmd = 'python process_glm.py '+params
    print(cmd)
    os.system(cmd)

def process_set_of_simulations(paramvalueslist):
    from multiprocessing import Pool

    paramvalueslist = [' '.join(map(str,param)) for param in paramvalueslist]
    #print paramvalueslist
    # _generate(paramvalueslist[2])
    pool = Pool(processes=1)
    pool.map(_generate,paramvalueslist)


def process_null_models(nb=1000):
    import itertools
    params = list(itertools.product(range(nb),['ESTIMATIONTYPE'],['eNullGlm'],['WITHINDELAYMETHOD'],['eMonthMultiVariateForWithin', 'eDeltaMultiVariateForWithin', 'eDeltaPoissonForWithin']))

    process_set_of_simulations(params)


def process_restricted_models(nb=1000):
    import itertools
    params = list(itertools.product(range(nb),['ESTIMATIONTYPE'],['eSelectedGlm','eInteractionGlm'],['FACTORRESTRICTION'],['eBurstDateRestriction', 'ePositionARestriction', 'ePositionAncestorARestriction', 'eNatureFRestriction', 'eAllRestriction'],['FRUIT_MODEL'],['False']))

    process_set_of_simulations(params)


if __name__ == '__main__' :
    import sys
    if len(sys.argv) > 1 :
        seed = int(sys.argv[1])
        params = dict()
        paramcmd = sys.argv[2:]
        assert len(paramcmd) % 2 == 0
        for i in xrange(len(paramcmd)/2):
            params[paramcmd[2*i]] = eval(paramcmd[2*i+1])
        print params
        generate_mtgs(seeds=[seed],params=params)
    else:
        #generate_all(100)
        process_restricted_models(100)
        import vplants.mangosim.utils.message as message
        message.send_msg('Simu','Done.')
