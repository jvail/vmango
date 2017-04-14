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


def get_probability_repository(variety = 'cogshall', 
                               estimationtype = eSelectedGlm, 
                               restriction = None):
    
    from os.path import join
    path = join(share_dir, 'glm_output_proba', variety)
    if restriction : path = join(path,RestrictionName[restriction])
    else: path = join(path,'allfactors')
    if restriction == eAllRestriction: estimationtype = eCompleteGlm
    path = join(path,GlmTypeName[estimationtype])
    return path


def get_simulated_mtg_repository(variety = 'cogshall'):
    return join(share_dir, 'glm_output_mtg', variety)


def get_option_glm_mtg_repository(variety = 'cogshall', 
                                  estimationtype = eSelectedGlm,
                                  restriction = None, 
                                  optionname = None):
    
    path = join(get_simulated_mtg_repository(variety), GlmTypeName[estimationtype])  
    if optionname: path = join(path,optionname)
    if restriction : path = join(path,RestrictionName[restriction])
    else: path = join(path,'allfactors')
    #path = join(path, WithinDelayMethodName[withindelaymethod].lower(), treename)
    return path

def get_glm_mtg_repository(params = dict(), 
                           optionname = None):
    repparams = {'optionname' : optionname}
    repparams['variety'] = params.get('VARIETY', 'cogshall') 
    repparams['estimationtype'] = params.get('GLM_TYPE', eSelectedGlm) 
    repparams['restriction'] = params.get('GLM_RESTRICTION', None)
    outputdir = get_option_glm_mtg_repository(**repparams)
    return outputdir


mtgfname = 'simulated_mtg_{}.pkl'