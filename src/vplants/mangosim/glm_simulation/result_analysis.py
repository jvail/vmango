import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
from vplants.mangosim.util_path import get_glm_mtg_repository, mtgfname
import os
from plot_distribution import *
from numpy import mean
import itertools


def retrieve_mtgs(inputdir, nb = None):
    print 'Dir:',inputdir
    import glob
    mtgfiles = glob.glob(join(inputdir,mtgfname.format('*')))
    mtgfiles.sort()
    if nb: mtgfiles = mtgfiles[:nb]
    mtgs = []
    for  mtgfile in mtgfiles:
        print 'Retreive '+repr(os.path.basename(mtgfile))
        mtgs.append(load_obj(mtgfile))
    return mtgs

WITHINDELAYMETHODVALUES = [eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin]
gu_distrib_fname = 'gu_distribution.pkl'

def burst_date_distribution(i=0, glmestimation = eNullGlm):
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[i],  'ESTIMATIONTYPE' : glmestimation}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, gu_distrib_fname)
    if not os.path.exists(distrib_file):
        print 'Retrieve MTGs'
        mtgs = retrieve_mtgs(inputdir)
        setMtgStyle(eSimulatedMtg)
        print 'Estimate burst date distribution'
        months,values,refvalues = estimate_burst_date_distribution(mtgs, reference = True)
        dump_obj((months,values,refvalues),gu_distrib_fname,inputdir)
    else:
        print 'Retreive burst date distribution from',repr(str(join(inputdir,gu_distrib_fname)))
        months,values,refvalues = load_obj(gu_distrib_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #print months
    #print [mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))]
    #print refvalues
    plot_histo(months,values,'Distribution of burst date of gu',refvalues)

def burst_date_distribution_comparison():
    allvalues = []
    for glmtype, withindelaymethodval in itertools.product([eSelectedGlm, eNullGlm],WITHINDELAYMETHODVALUES):
        params = {'WITHINDELAYMETHOD' : withindelaymethodval, 'ESTIMATIONTYPE' : glmtype}
        inputdir = get_glm_mtg_repository( params = params)
        distrib_file = join(inputdir, gu_distrib_fname)
        print 'Retreive burst date distribution from',repr(str(distrib_file))
        months,values,refvalues = load_obj(gu_distrib_fname,inputdir)
        if len(allvalues) == 0: allvalues.append(refvalues)
        allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])
    legends = ['Reference']+[b+' - '+a for a,b in itertools.product(['GLM','Null GLM'],['Month Multivariate', 'Delta Multivariate', 'Delta Poisson'])]
    print legends
    plot_histo_curve(months, allvalues, 'Distribution of burst date of gu', legends=legends)

inflo_distrib_fname = 'inflo_distribution.pkl'

def bloom_date_distribution():
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[0]}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, inflo_distrib_fname)
    if not os.path.exists(distrib_file):
        print 'Retrieve MTGs'
        mtgs = retrieve_mtgs(inputdir, 10)
        setMtgStyle(eSimulatedMtg)
        print 'Estimate bloom date distribution'
        months,values,refvalues = estimate_bloom_date_distribution(mtgs, reference = True)
        #dump_obj((months,values,refvalues),distrib_fname,inputdir)
    else:
        print 'Retreive bloom date distribution from',repr(str(distrib_file))
        months,values,refvalues = load_obj(inflo_distrib_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #print months
    #print [mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))]
    #print refvalues
    plot_histo(months,values,'Distribution of bloom date of gu',refvalues)

gu_distrib_layer1_fname = 'gu_distribution_layer1.pkl'

def burst_date_distribution_layer1(i=0, glmestimation = eSelectedGlm):
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[i],  'ESTIMATIONTYPE' : glmestimation}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, gu_distrib_layer1_fname)
    consider = lambda mtg, uc: rank_within_cycle(mtg,uc) == 0
    if not os.path.exists(distrib_file):
        print 'Retrieve MTGs'
        mtgs = retrieve_mtgs(inputdir)
        setMtgStyle(eSimulatedMtg)
        print 'Estimate burst date distribution'
        months,values,refvalues = estimate_burst_date_distribution(mtgs, reference = True, consider=consider)
        dump_obj((months,values,refvalues),gu_distrib_layer1_fname,inputdir)
    else:
        print 'Retreive burst date distribution from',repr(str(join(inputdir,gu_distrib_layer1_fname)))
        months,values,refvalues = load_obj(gu_distrib_layer1_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #plot_histo(months,values,'Distribution of burst date of gu',refvalues,consider=consider)


def _burst_date_distribution_layer1(x) : burst_date_distribution_layer1(x[0],x[1])

def process_burst_date_distribution():
    from multiprocessing import Pool, cpu_count

    #print paramvalueslist
    # _generate(paramvalueslist[2])
    params = list(itertools.product(range(3),[eSelectedGlm,eNullGlm]))
    pool = Pool(processes=min(cpu_count()-1,3))
    print params
    pool.map(_burst_date_distribution_layer1,params)





if __name__ == '__main__':
    process_burst_date_distribution()
    #burst_date_distribution_comparison()
    #burst_date_distribution_comparison()
    #bloom_date_distribution()
