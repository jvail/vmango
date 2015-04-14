import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
from vplants.mangosim.util_path import get_glm_mtg_repository, mtgfname
import os
from plot_distribution import *
from numpy import mean



def retrieve_mtgs(inputdir):
    print 'Dir:',inputdir
    import glob
    mtgfiles = glob.glob(join(inputdir,mtgfname.format('*')))
    mtgfiles.sort()
    mtgs = []
    for  mtgfile in mtgfiles:
        print 'Retreive '+repr(os.path.basename(mtgfile))
        mtgs.append(load_obj(mtgfile))
    return mtgs

WITHINDELAYMETHODVALUES = [eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin]
distrib_fname = 'gu_distribution.pkl'

def burst_date_distribution():
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[0]}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, distrib_fname)
    if not os.path.exists(distrib_file):
        print 'Retrieve MTGs'
        mtgs = retrieve_mtgs(inputdir)
        setMtgStyle(eSimulatedMtg)
        print 'Estimate burst date distribution'
        months,values,refvalues = estimate_burst_date_distribution(mtgs, reference = True)
        dump_obj((months,values,refvalues),distrib_fname,inputdir)
    else:
        print 'Retreive burst date distribution from',repr(str(join(inputdir,distrib_fname)))
        months,values,refvalues = load_obj(distrib_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #print months
    #print [mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))]
    #print refvalues
    plot_histo(months,values,'Distribution of burst date of gu',refvalues)

def burst_date_distribution_comparison():
    allvalues = []
    for withindelaymethodval in WITHINDELAYMETHODVALUES:
        params = {'WITHINDELAYMETHOD' : withindelaymethodval}
        inputdir = get_glm_mtg_repository( params = params)
        distrib_file = join(inputdir, distrib_fname)
        print 'Retreive burst date distribution from',repr(str(join(inputdir,distrib_fname)))
        months,values,refvalues = load_obj(distrib_fname,inputdir)
        if len(allvalues) == 0: allvalues.append(refvalues)
        allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])
    plot_histo_curve(months, allvalues, 'Distribution of burst date of gu', ['Reference', 'Month Multivariate', 'Delta Multivariate', 'Delta Poisson'])


if __name__ == '__main__':
    #burst_date_distribution()
    burst_date_distribution_comparison()
