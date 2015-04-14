from os.path import join
from vplants.mangosim.tools import *
from vplants.mangosim.state import *


def get_probabily_repository(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    from os.path import join
    return join(share_dir, 'glm_output_proba', variety, GlmTypeName[estimationtype], treename)


def get_simulated_mtg_repository(variety = 'cogshall'):
    return join(share_dir, 'glm_output_mtg', variety)


def get_option_glm_mtg_repository(variety = 'cogshall', 
                           treename = 'all_trees', 
                           estimationbase = eVarietyBased,
                           estimationtype = eSelectedGlm,
                           withindelaymethod = eMonthMultiVariateForWithin):
    return join(get_simulated_mtg_repository(variety), EstimationBaseName[estimationbase].lower(), 
                GlmTypeName[estimationtype],  
                WithinDelayMethodName[withindelaymethod].lower(),
                treename)

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
    outputdir = get_option_glm_mtg_repository(**repparams)
    if optionname:
        outputdir = join(outputdir,optionname)
    return outputdir


mtgfname = 'simulated_mtg_{}.pkl'