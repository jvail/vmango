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


def generate_all(nb = 10, fruitmodel = False):
    params = {'GLM_TYPE' : eInteractionGlm, 'FRUIT_MODEL' : fruitmodel}
    generate_mtgs(seeds = range(nb), params = params)
    params = {'GLM_TYPE' : eSelectedGlm, 'FRUIT_MODEL' : fruitmodel}
    generate_mtgs(seeds = range(nb), params = params)

def generate_all_parallel(nb = 100, fruitmodel = False):
    import itertools
    params = list(itertools.product(range(nb),['GLM_TYPE'],['eSelectedGlm','eInteractionGlm'],['GLM_RESTRICTION'],['None'],['FRUIT_MODEL'],[str(fruitmodel)]))
    process_set_of_simulations(params)

def generate_all_restriction(nb = 1000):
    generate_mtgs(seeds = range(nb))


def _generate(params):
    import os, sys
    if 'darwin' in sys.platform:
        cmd0 = 'export DYLD_FALLBACK_LIBRARY_PATH='+repr(os.environ['DYLD_FALLBACK_LIBRARY_PATH'])+' ; '
    else:
        cmd0 = ''
    cmd = 'python process_glm.py --process '+params
    print(cmd)
    os.system(cmd0+cmd)

def process_set_of_simulations(paramvalueslist, parallel = True):
    import multiprocessing as mp

    paramvalueslist = [' '.join(map(str,param)) for param in paramvalueslist]
    #print paramvalueslist
    if not parallel:
        for params in paramvalueslist:
            _generate(params)
    else:
        countcpus = mp.cpu_count()
        pool = mp.Pool(processes=countcpus-3)
        pool.map(_generate,paramvalueslist)


def process_restricted_models(nb=1000, fruitmodel = False):
    import itertools
    params = list(itertools.product(range(nb),['GLM_TYPE'],['eSelectedGlm','eInteractionGlm'],['GLM_RESTRICTION'],['eBurstDateRestriction', 'ePositionARestriction', 'ePositionAncestorARestriction', 'eNatureFRestriction', 'eAllRestriction'],['FRUIT_MODEL'],[str(fruitmodel)]))

    process_set_of_simulations(params)


if __name__ == '__main__' :
    import sys
    import vplants.mangosim.utils.message as message
    if len(sys.argv) > 1 :
        if  '--process' == sys.argv[1]:
            seed = int(sys.argv[2])
            params = dict()
            paramcmd = sys.argv[3:]
            assert len(paramcmd) % 2 == 0
            for i in xrange(len(paramcmd)/2):
                params[paramcmd[2*i]] = eval(paramcmd[2*i+1])
            generate_mtgs(seeds=[seed],params=params)
        elif '--std' == sys.argv[1]:
            maxseed = int(sys.argv[2]) if len(sys.argv) > 2 else 10
            fruitmodel = eval(sys.argv[3]) if len(sys.argv) > 3 else False
            generate_all(maxseed, fruitmodel)
            message.send_msg('Simu Std','Done.')
        elif '--restricted' == sys.argv[1]:
            maxseed = int(sys.argv[2]) if len(sys.argv) > 2 else 10
            fruitmodel = eval(sys.argv[3]) if len(sys.argv) > 3 else False
            process_restricted_models(maxseed, fruitmodel)            
            message.send_msg('Simu Restricted','Done.')
        elif '--help' == sys.argv[1] or '--h' == sys.argv[1]:
            print '--std [nb] [fruitmodel]'
            print '--restricted [nb] [fruitmodel]'
    else:
        #generate_all(100, False)
        generate_all(100, True)
        process_restricted_models(100)
        message.send_msg('Simu','Done.')
