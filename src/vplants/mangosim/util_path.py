from os.path import join, exists, abspath, dirname

import vplants.mangosim
try:
    share_dir = shared_data(vplants.mangosim, share_path = "share")
except:
    share_dir = join(dirname(__file__),'..','..','..','share')
share_dir = abspath(share_dir)

from vplants.mangosim.state import *


def get_probability_repository(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm, restriction = None):
    from os.path import join
    path = join(share_dir, 'glm_output_proba', variety)
    if restriction : path = join(path,RestrictionName[restriction])
    path = join(path,GlmTypeName[estimationtype], treename)
    return path


def get_simulated_mtg_repository(variety = 'cogshall'):
    return join(share_dir, 'glm_output_mtg', variety)


def get_option_glm_mtg_repository(variety = 'cogshall', 
                           treename = 'all_trees', 
                           estimationbase = eVarietyBased,
                           estimationtype = eSelectedGlm,
                           withindelaymethod = eMonthMultiVariateForWithin,
                           restriction = None):
    path = join(get_simulated_mtg_repository(variety), EstimationBaseName[estimationbase].lower(), 
                GlmTypeName[estimationtype])  
    if restriction : path = join(path,RestrictionName[restriction])
    path = join(path,WithinDelayMethodName[withindelaymethod].lower(),
                treename)
    return path

def get_glm_mtg_repository(trees = range(5), 
                           params = dict(), 
                           optionname = None,
                           lsysfile = 'mango_glm.lpy'):
    from openalea.lpy import Lsystem
    repparams = {}
    l = Lsystem(lsysfile)
    repparams['estimationbase'] = params.get('ESTIMATIONBASE', l.estimationbase)
    repparams['estimationtype'] = params.get('ESTIMATIONTYPE', l.estimationtype)
    repparams['withindelaymethod'] = params.get('WITHINDELAYMETHOD', l.WithinDelayMethod)
    repparams['restriction'] = params.get('FACTORRESTRICTION', l.factorrestriction)
    outputdir = get_option_glm_mtg_repository(**repparams)
    if optionname:
        outputdir = join(outputdir,optionname)
    return outputdir


mtgfname = 'simulated_mtg_{}.pkl'