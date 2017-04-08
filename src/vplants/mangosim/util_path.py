from os.path import join, exists, abspath, dirname
import os
import vplants.mangosim
try:
    from openalea.deploy.shared_data import shared_data
    share_dir = shared_data(vplants.mangosim, share_path = "share")
except:
    share_dir = join(dirname(__file__),'..','..','..','share')
share_dir = abspath(share_dir)

def data(fname): 
    return join(share_dir, fname)

from vplants.mangosim.state import *


def get_probability_repository(variety = 'cogshall', estimationtype = eSelectedGlm, restriction = None):
    from os.path import join
    path = join(share_dir, 'glm_output_proba', variety)
    if restriction : path = join(path,RestrictionName[restriction])
    else: path = join(path,'allfactors')
    path = join(path,GlmTypeName[estimationtype])
    return path


def get_simulated_mtg_repository(variety = 'cogshall'):
    return join(share_dir, 'glm_output_mtg', variety)


def get_option_glm_mtg_repository(variety = 'cogshall', 
                                  estimationtype = eSelectedGlm,
                                  withindelaymethod = eMonthMultiVariateForWithin,
                                  restriction = None):
    path = join(get_simulated_mtg_repository(variety), GlmTypeName[estimationtype])  
    if restriction : path = join(path,RestrictionName[restriction])
    else: path = join(path,'allfactors')
    #path = join(path, WithinDelayMethodName[withindelaymethod].lower(), treename)
    return path

def get_glm_mtg_repository(trees = range(3), 
                           params = dict(), 
                           optionname = None,
                           lsysfile = 'mango_simulation.lpy'):
    repparams = {}
    # from openalea.lpy import Lsystem
    # l = Lsystem(lsysfile)
    repparams['estimationtype'] = eSelectedGlm
    #repparams['withindelaymethod'] = params.get('WITHINDELAYMETHOD', l.WithinDelayMethod)
    repparams['restriction'] = None # params.get('FACTORRESTRICTION', l.factorrestriction)
    outputdir = get_option_glm_mtg_repository(**repparams)
    if optionname:
        outputdir = join(outputdir,optionname)
    return outputdir


mtgfname = 'simulated_mtg_{}.pkl'