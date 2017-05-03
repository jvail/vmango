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
                               estimationtype = eInteractionGlm, 
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
                                  estimationtype = eInteractionGlm,
                                  restriction = None,
                                  fruitmodel = False,
                                  fruitbranchsize = None, 
                                  optionname = None):
    
    path = join(get_simulated_mtg_repository(variety), GlmTypeName[estimationtype]) 
    if optionname: path = join(path,optionname)
    if fruitmodel :
        path = join(path,'withfruitmodel')
        if not fruitbranchsize is None: path = join(path,'fruitbranchsize'+str(fruitbranchsize))
    else:
        path = join(path,'withoutfruitmodel')
    if restriction : path = join(path,RestrictionName[restriction])
    else: path = join(path,'allfactors')
    #path = join(path, WithinDelayMethodName[withindelaymethod].lower(), treename)
    return path

def get_glm_mtg_repository(params = dict(), 
                           optionname = None):
    repparams = {'optionname' : optionname}
    repparams['variety'] = params.get('VARIETY', 'cogshall') 
    repparams['estimationtype'] = params.get('GLM_TYPE', eInteractionGlm) 
    repparams['restriction'] = params.get('GLM_RESTRICTION', None)
    repparams['fruitmodel'] = params.get('FRUIT_MODEL', False) 
    repparams['fruitbranchsize'] = params.get('FRUITBRANCHSIZE')
    outputdir = get_option_glm_mtg_repository(**repparams)
    return outputdir

restricabbrev = { eBurstDateRestriction         : 'wobm', 
                    ePositionARestriction         : 'wopos', 
                    ePositionAncestorARestriction : 'woposan',                    
                    eNatureFRestriction           : 'wonat',
                    eBurstDateOnlyRestriction         : 'woybm', 
                    ePositionAOnlyRestriction         : 'woypos', 
                    ePositionAncestorAOnlyRestriction : 'woyposan',                    
                    eNatureFOnlyRestriction           : 'woynat',
                    eAllRestriction               : 'woall',
                    eNoRestriction                : 'wiall',
                    None                          : 'wiall'
                    }
glmtypeabbrev = {eCompleteGlm : 'c', eSelectedGlm : 's', eInteractionGlm : 'i'}

varietyabbrev = {'cogshall' : 'cs'}

def param_stringid(params, consideronly = None, sep = '_'):
    result = ''
    if consideronly is None or 'VARIETY' in consideronly:
        result += varietyabbrev[params.get('VARIETY', 'cogshall')]
    if consideronly is None or 'GLM_TYPE' in consideronly:
        if len(result) > 0 : result += sep
        result += glmtypeabbrev[params.get('GLM_TYPE', eInteractionGlm)]
    if consideronly is None or 'GLM_RESTRICTION' in consideronly:
        if len(result) > 0 : result += sep
        result += restricabbrev[params.get('GLM_RESTRICTION')]
    if consideronly is None or 'FRUIT_MODEL' in consideronly or 'FRUITBRANCHSIZE' in consideronly:
        if len(result) > 0 : result += sep
        if params.get('FRUIT_MODEL', False):
            result += 'wifm'
            bs = params.get('FRUITBRANCHSIZE')
            if not bs is None:
                result += str(bs)
        else:
            result+= 'nofm'
    return result

def stringid_param(string, sep = '_'):
    def inverse(mdict) : return dict([(v,k) for k,v in mdict.items()])
    abbrevs = string.split(sep)
    assert len(abbrevs) == 4
    params = {'VARIETY' : inverse(varietyabbrev)[abbrevs[0]], 'GLM_TYPE' : inverse(glmtypeabbrev)[abbrevs[1]],
              'GLM_RESTRICTION' : inverse(restricabbrev)[abbrevs[2]], 'FRUIT_MODEL' : abbrevs[3] != 'nofm' }
    if abbrevs[3].startswith('wifm') and len(abbrevs[3]) > 4:
        params['FRUITBRANCHSIZE'] = int(abbrevs[3][4:])
    return params


def common_options(paramlist):
    paramref = paramlist[0]
    commons = []
    for k, v in paramref.items():
        for param in paramlist[1:]:
            if param.get(k) != v:
                break
        else:
            commons.append(k)
    return commons


def paramset_stringid(paramlist, sharedsep = '_comp_', individualsep = '-', optionsep = '_'):
    commons = set(common_options(paramlist))
    result = param_stringid(paramlist[0], commons)
    if len(result) > 0:
        result += sharedsep
    for i,param in enumerate(paramlist):
        particularpname = set(param).difference(commons)
        if i > 0: result += individualsep
        result += param_stringid(param, particularpname, optionsep)
    return result

mtgfname = 'simulated_mtg_{}.pkl'