from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import
from past.builtins import cmp
from builtins import zip
from builtins import str
from builtins import map
from builtins import range
from past.utils import old_div
from importlib import reload
import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
from vplants.mangosim.util_path import get_glm_mtg_repository, mtgfname
import os
from .plot_distribution import *
from numpy import mean
import itertools

def monthtranslate(txt):
        m = [('janv','jan'),('fev','feb'),('mars','march'),('avril','april'),('mai','may'),('juin','june'),('juil','july'),('aout','aug')]
        for p,nm in m:
            txt = txt.replace(p,nm)
        return txt


def __strip_histo(histo):
    # We remove the first and last part of the histo if it is equal to 0.
    keyvalues = list(histo.keys())
    for d in keyvalues:
        if histo[d] == 0:
            del histo[d]
        else : break
    if len(histo) > 0:
        for d in reversed(keyvalues):
            if histo[d] == 0:
                del histo[d]
            else : break

def burst_date_cycle_distribution(mtg, ucs, strip = True):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in list(Month.items())])
    from collections import OrderedDict
    histo_date = OrderedDict([(i,0) for i in range(3,6)])
    for uc in ucs:
        if has_burst_date(mtg,uc):
            c = get_cycle(get_burst_date(mtg,uc))
            histo_date[c] += 1
    if strip : __strip_histo(histo_date)
    return histo_date

def date_month_distribution(mtg, ucs, strip = True, date_accessor = get_burst_date):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in list(Month.items())])
    from collections import OrderedDict
    daterange = monthdate_range(cycle_end(3),cycle_begin(6))
    histo_date = OrderedDict([(d,0) for d in daterange])
    for uc in ucs:
        try:
            d = date_accessor(mtg,uc)
            m = d.month
            y = d.year
            histo_date[(m,y)] += 1
        except : pass
    if strip : __strip_histo(histo_date)
    return histo_date

def date_week_distribution(mtg, ucs, strip = True, date_accessor = get_burst_date):
    from vplants.mangosim.util_date import Month
    from collections import OrderedDict
    histo_date = dict()
    for uc in ucs:
        try:
            d = date_accessor(mtg,uc).isocalendar()
            w = d[1]
            y = d[0]
            histo_date[(w,y)] = histo_date.get((w,y),0) + 1
        except : pass
    dates = list(histo_date.keys())
    dates.sort(cmp=lambda a,b: cmp((a[1],a[0]),(b[1],b[0])))
    sorted_histo_date = OrderedDict([(d,histo_date[d]) for d in dates])
    return sorted_histo_date

#@use_global_mtg
def estimate_burst_date_distribution_from_mtgs(mtgs = None, variety = 'cogshall', reference = True, exclude = None, consider = None):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in list(Month.items())])
    histo_date = []
    if type(mtgs) == MTG: mtgs = [mtgs] 

    for mtg in mtgs:
        ucs =  get_all_gus_of_variety(mtg, None, variety)
        if exclude: ucs = [uc for uc in ucs if not exclude(mtg,uc)]
        if consider: ucs = [uc for uc in ucs if consider(mtg,uc)]
        histo_date.append(date_month_distribution(mtg, ucs, False ))
    if reference:
        currentmtgstyle = __MtgStyle
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()
        ucs =  get_all_gus_of_variety(mtg, None, variety)
        if exclude: ucs = [uc for uc in ucs if not exclude(uc)]
        if consider: ucs = [uc for uc in ucs if consider(uc)]
        ref_histo_date = date_month_distribution(mtg, ucs, False )
        setMtgStyle(currentmtgstyle)

        return [Month[m]+'-'+str(y) for m,y in list(histo_date[0].keys())],[list(h.values()) for h in histo_date], list(ref_histo_date.values())
    else:
        return [Month[m]+'-'+str(y) for m,y in list(histo_date[0].keys())],[list(h.values()) for h in histo_date]

def estimate_bloom_date_distribution(mtgs = None, variety = 'cogshall', reference = True, showallweeks = False):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in list(Month.items())])
    histo_date = []
    if variety is None:
        inflo_selector = lambda mtg : list(mtg.property(BloomPropertyName).keys())
    else:
        inflo_selector = lambda mtg : get_all_inflo_of_variety(mtg, None, variety)

    if type(mtgs) == MTG: mtgs = [mtgs] 

    for mtg in mtgs:
        histo_date.append(date_week_distribution(mtg,inflo_selector(mtg), False, get_bloom_date ))
    if reference:
        currentmtgstyle = __MtgStyle
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()
        if variety is None:
            inflo_selector = lambda mtg : list(mtg.property(BloomPropertyName).keys())
        ref_histo_date = date_week_distribution(mtg, inflo_selector(mtg), False, get_bloom_date )
        setMtgStyle(currentmtgstyle)

        return [str(w)+'/'+strdate(w,y) for w,y in list(histo_date[0].keys())],[list(h.values()) for h in histo_date], list(ref_histo_date.values())
    else:
        return [str(w)+'/'+strdate(w,y) for w,y in list(histo_date[0].keys())],[list(h.values()) for h in histo_date]

def retrieve_mtgs(inputdir, nb = None):
    print('Dir:',inputdir)
    import glob
    mtgfiles = glob.glob(join(inputdir,mtgfname.format('*')))
    mtgfiles.sort()
    if nb: mtgfiles = mtgfiles[:nb]
    mtgs = []
    for  mtgfile in mtgfiles:
        print('Retreive '+repr(os.path.basename(mtgfile)))
        mtgs.append(mtgfile)
    return mtgs

WITHINDELAYMETHODVALUES = [eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin]
gu_distrib_fname = 'gu_distribution.pkl'

def burst_date_distribution(i=0, glmestimation = eNullGlm):
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[i],  'ESTIMATIONTYPE' : glmestimation}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, gu_distrib_fname)
    if not os.path.exists(distrib_file):
        print('Retrieve MTGs')
        mtgs = retrieve_mtgs(inputdir)
        setMtgStyle(eSimulatedMtg)
        print('Estimate burst date distribution')
        months,values,refvalues = estimate_burst_date_distribution_from_mtgs(mtgs, reference = True)
        dump_obj((months,values,refvalues),gu_distrib_fname,inputdir)
    else:
        print('Retreive burst date distribution from',repr(str(join(inputdir,gu_distrib_fname))))
        months,values,refvalues = load_obj(gu_distrib_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #print months
    #print [mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))]
    #print refvalues
    plot_histo(list(map(monthtranslate,months)),values,'',refvalues)


def estimate_tree_burst_date_distribution(parameters):
    result, mtgfname, treename, exclude, consider = parameters
    if mtgfname != 'reference':
        setMtgStyle(eSimulatedMtg)
        mtg = load_obj(mtgfname)
    else:
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()

    tree = get_tree_from_name(mtg, treename)
    ucs =  get_all_gus_of_tree(mtg, tree)
    if exclude: ucs = [uc for uc in ucs if not exclude(mtg,uc)]
    if consider: ucs = [uc for uc in ucs if consider(mtg,uc)]
    result[mtgfname] = date_month_distribution(mtg, ucs, False )
    print('Done',repr(mtgfname), '...')



def tree_burst_date_distribution(treename = 'B10', i=0, glmestimation = eNullGlm):
    from multiprocessing import Pool, Manager, cpu_count
    from vplants.mangosim.util_date import MonthEn
    Month = dict( [(mi,v) for v,mi in list(MonthEn.items())] )

    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[i],  'ESTIMATIONTYPE' : glmestimation}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, treename+'_'+gu_distrib_fname)
    if not os.path.exists(distrib_file):
        print('Retrieve MTGs')
        if not os.path.exists(join(inputdir,treename+'_collect_'+gu_distrib_fname)):
            mtgs = retrieve_mtgs(inputdir)
            m = Manager()
            result = m.dict()
            params = [[result, mtg, treename, None, None] for mtg in mtgs]
            p = Pool(processes = cpu_count())
            print('Estimate burst date distribution')
            p.map(estimate_tree_burst_date_distribution, params)
            estimate_tree_burst_date_distribution([result,'reference',treename, None, None])
            # months,values,refvalues = estimate_burst_date_distribution_from_mtgs(mtgs, reference = True)
            #dump_obj((months,values,refvalues),gu_distrib_fname,inputdir)
            dump_obj(dict(result),treename+'_collect_'+gu_distrib_fname,inputdir)
        else: result = load_obj(treename+'_collect_'+gu_distrib_fname,inputdir)
        values = []
        print(len(result))
        for k,v in list(result.items()):
            if k == 'reference':
                print('found reference')
                refvalues = list(v.values())
                months = [Month[m]+'-'+str(y) for m,y in list(v.keys())]
            else:
                values.append(list(v.values()))        
        dump_obj((months,values,refvalues),treename+'_'+gu_distrib_fname,inputdir)
    else:
        print('Retreive burst date distribution from',repr(str(join(inputdir,gu_distrib_fname))))
        months,values,refvalues = load_obj(gu_distrib_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #print months
    #print [mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))]
    #print refvalues
    #plot_histo(months,values,'Distribution of burst date of gu',refvalues)

def estimate_variety_burst_date_distribution(parameters):
    result, mtgfname, exclude, consider = parameters
    if mtgfname != 'reference':
        setMtgStyle(eSimulatedMtg)
        mtg = load_obj(mtgfname)
    else:
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()
    ucs =  get_all_gus_of_variety(mtg, 'cogshall', None)
    if exclude: ucs = [uc for uc in ucs if not exclude(mtg,uc)]
    if consider: ucs = [uc for uc in ucs if consider(mtg,uc)]
    result[mtgfname] = date_month_distribution(mtg, ucs, False )
    print('Done',repr(mtgfname), '...')


def restricted_burst_date_distribution(restriction = None):
    from multiprocessing import Pool, Manager, cpu_count
    from vplants.mangosim.util_date import MonthEn
    Month = dict( [(mi,v) for v,mi in list(MonthEn.items())] )

    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[0],  'ESTIMATIONTYPE' : eSelectedGlm, 'FACTORRESTRICTION' : restriction}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, gu_distrib_fname)
    if not os.path.exists(distrib_file):
        print('Retrieve MTGs')
        if not os.path.exists(join(inputdir,'collect_'+gu_distrib_fname)):
            mtgs = retrieve_mtgs(inputdir)
            m = Manager()
            result = m.dict()
            params = [[result, mtg,  None, None] for mtg in mtgs]
            p = Pool(processes = cpu_count())
            print('Estimate burst date distribution')
            p.map(estimate_variety_burst_date_distribution, params)
            estimate_variety_burst_date_distribution([result,'reference', None, None])
            # months,values,refvalues = estimate_burst_date_distribution_from_mtgs(mtgs, reference = True)
            #dump_obj((months,values,refvalues),gu_distrib_fname,inputdir)
            dump_obj(dict(result),'collect_'+gu_distrib_fname,inputdir)
        else: result = load_obj('collect_'+gu_distrib_fname,inputdir)
        values = []
        print(len(result))
        for k,v in list(result.items()):
            if k == 'reference':
                print('found reference')
                refvalues = list(v.values())
                months = [Month[m]+'-'+str(y) for m,y in list(v.keys())]
            else:
                values.append(list(v.values()))        
        dump_obj((months,values,refvalues),gu_distrib_fname,inputdir)
    else:
        print('Retreive burst date distribution from',repr(str(join(inputdir,gu_distrib_fname))))
        months,values,refvalues = load_obj(gu_distrib_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)

def get_burst_date_distributions(treename = None, verbose = False):
    allvalues = []
    for glmtype, withindelaymethodval,restriction in itertools.product([eSelectedGlm],[eMonthMultiVariateForWithin],[None]+list(RestrictionName.keys())):
        params = {'WITHINDELAYMETHOD' : eMonthMultiVariateForWithin, 'ESTIMATIONTYPE' : eSelectedGlm, 'FACTORRESTRICTION' : restriction}
        inputdir = get_glm_mtg_repository( params = params)
        if not treename is None: lgu_distrib_fname = treename+'_'+gu_distrib_fname
        else: lgu_distrib_fname = gu_distrib_fname
        distrib_file = join(inputdir, lgu_distrib_fname)
        if verbose : print('Retreive burst date distribution from',repr(str(distrib_file)))
        months,values,refvalues = load_obj(lgu_distrib_fname,inputdir)
        if len(allvalues) == 0: allvalues.append(refvalues)
        allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])

    for glmtype, withindelaymethodval,restriction in itertools.product([eNullGlm],[eMonthMultiVariateForWithin],[None]):
        params = {'WITHINDELAYMETHOD' : withindelaymethodval, 'ESTIMATIONTYPE' : glmtype, 'FACTORRESTRICTION' : restriction}
        inputdir = get_glm_mtg_repository( params = params)
        if not treename is None: lgu_distrib_fname = treename+'_'+gu_distrib_fname
        else: lgu_distrib_fname = gu_distrib_fname
        distrib_file = join(inputdir, lgu_distrib_fname)
        if verbose : print('Retreive burst date distribution from',repr(str(distrib_file)))
        months,values,refvalues = load_obj(lgu_distrib_fname,inputdir)
        allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])
    legends = ['Reference','GLM']+[a+' '+b for a,b in itertools.product(['GLM'],[m.replace('_',' ') for m in list(RestrictionName.values())])]+['Null GLM']
    return  months, allvalues, legends


def burst_date_distribution_comparison0(treename = None):
    allvalues = []
    for glmtype, withindelaymethodval in itertools.product([eSelectedGlm,eNullGlm],[eMonthMultiVariateForWithin]):
        params = {'WITHINDELAYMETHOD' : withindelaymethodval, 'ESTIMATIONTYPE' : glmtype}
        inputdir = get_glm_mtg_repository( params = params)
        if not treename is None: lgu_distrib_fname = treename+'_'+gu_distrib_fname
        else: lgu_distrib_fname = gu_distrib_fname
        distrib_file = join(inputdir, lgu_distrib_fname)
        print('Retreive burst date distribution from',repr(str(distrib_file)))
        months,values,refvalues = load_obj(lgu_distrib_fname,inputdir)
        if len(allvalues) == 0: allvalues.append(refvalues)
        allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])
        print(allvalues[-1])

    legends = ['Reference','GLM','Null GLM']
    print(legends)

    months = list(map(monthtranslate, months))
    plot_histo_curve(months, allvalues, 'Distribution of burst dates of growth units'+ (' in '+treename if treename else ''), legends=legends,linewidth=2)

def burst_date_distribution_ks_test(treename = None):
    allvalues = []
    for glmtype, withindelaymethodval in itertools.product([eSelectedGlm],[eMonthMultiVariateForWithin]):
        params = {'WITHINDELAYMETHOD' : withindelaymethodval, 'ESTIMATIONTYPE' : glmtype}
        inputdir = get_glm_mtg_repository( params = params)
        if not treename is None: lgu_distrib_fname = treename+'_'+gu_distrib_fname
        else: lgu_distrib_fname = gu_distrib_fname
        distrib_file = join(inputdir, lgu_distrib_fname)
        print('Retreive burst date distribution from',repr(str(distrib_file)))
        months,values,refvalues = load_obj(lgu_distrib_fname,inputdir)
        if len(allvalues) == 0: allvalues.append(refvalues)
        allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])

    legends = ['Reference','GLM']
    from scipy.stats import ks_2samp
    print(sum(allvalues[0]),allvalues[0])
    print(sum(allvalues[1]),allvalues[1])
    print(ks_2samp(allvalues[0], allvalues[1]))

def burst_date_distribution_comparison(treename = None):

    months = list(map(monthtranslate, months))
    plot_histo_curve(months, allvalues, 'Distribution of burst dates of growth units'+ (' in '+treename if treename else ''), legends=legends,linewidth=2)


def burst_date_distribution_comparison1(treename = None):
    months,allvalues,legends = get_burst_date_distributions(treename)
    del allvalues[0] ; del legends[0]
    months = list(map(monthtranslate, months))
    allvalues.insert(1,allvalues[-1])
    del allvalues[-1]
    allvalues.insert(-2,allvalues[3])
    del allvalues[3]

    legends = ['GLM','Null GLM','GLM without Burst Date','GLM without Position', 'GLM without Ancestor Fate','GLM without Ancestor Position']
    plot_histo_curve(months, allvalues, 'Distribution of burst dates of growth units'+ (' in '+treename if treename else ''), legends=legends,linewidth=2)


inflo_distrib_fname = 'inflo_distribution.pkl'

def bloom_date_distribution():
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[0]}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, inflo_distrib_fname)
    if not os.path.exists(distrib_file):
        print('Retrieve MTGs')
        mtgs = retrieve_mtgs(inputdir, 10)
        setMtgStyle(eSimulatedMtg)
        print('Estimate bloom date distribution')
        months,values,refvalues = estimate_bloom_date_distribution(mtgs, reference = True)
        #dump_obj((months,values,refvalues),distrib_fname,inputdir)
    else:
        print('Retreive bloom date distribution from',repr(str(distrib_file)))
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
        print('Retrieve MTGs')
        mtgs = retrieve_mtgs(inputdir)
        setMtgStyle(eSimulatedMtg)
        print('Estimate burst date distribution')
        months,values,refvalues = estimate_burst_date_distribution(mtgs, reference = True, consider=consider)
        dump_obj((months,values,refvalues),gu_distrib_layer1_fname,inputdir)
    else:
        print('Retreive burst date distribution from',repr(str(join(inputdir,gu_distrib_layer1_fname))))
        months,values,refvalues = load_obj(gu_distrib_layer1_fname,inputdir)
    assert len(months) == len(values[0]) == len(refvalues)
    #plot_histo(months,values,'Distribution of burst date of gu',refvalues,consider=consider)


def _burst_date_distribution_layer1(x) : burst_date_distribution_layer1(x[0],x[1])

def process_burst_date_distribution():
    from multiprocessing import Pool, cpu_count

    #print paramvalueslist
    # _generate(paramvalueslist[2])
    params = list(itertools.product(list(range(3)),[eSelectedGlm,eNullGlm]))
    pool = Pool(processes=min(cpu_count()-1,3))
    print(params)
    pool.map(_burst_date_distribution_layer1,params)

def histogram_distance(hist1, hist2):
    from math import log
    sh1 = float(sum(hist1))
    sh2 = float(sum(hist2))
    print(sh1, sh2)
    h1 = [old_div(v1,sh1) for v1 in hist1]
    h2 = [old_div(v2,sh2) for v2 in hist2]
    divkl_h1_h2 = sum([v1 * log(old_div(v1,v2)) for v1,v2 in zip(h1,h2) if abs(v1) > 0 and abs(v2) > 0] )
    divkl_h2_h1 = sum([v1 * log(old_div(v1,v2)) for v1,v2 in zip(h2,h1) if abs(v1) > 0 and abs(v2) > 0] )
    return divkl_h1_h2+divkl_h2_h1


def histogram_distances(allvalues, legends):
    ref = allvalues[1] # we take GLM as reference value
    #del allvalues[0]
    #del allvalues[1]
    results = []
    for d in allvalues[2:]:
        results.append(histogram_distance(ref,d))
    res = [(old_div(r,results[-1])) for r in results]
    for l,r in zip(legends[2:], res):
        print(l,r)

def burst_date_histogram_distances():
    months, allvalues, legends = get_burst_date_distributions()
    print(legends)
    histogram_distances(allvalues, legends)


def branch_length_histogram(mtgfname = 'reference'):
    if mtgfname != 'reference':
        setMtgStyle(eSimulatedMtg)
        mtg = load_obj(mtgfname)
    else:
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()
    ucs =  get_all_gus_of_variety(mtg, 'cogshall', None)
    def axial_axe_length(uc):
        cuc = uc
        l = 1
        while mtg.parent(cuc) and mtg.edge_type(mtg.parent(cuc)) == '<':
            cuc = mtg.parent(cuc)
            l += 1
        return l
    last_apical_ucs = [uc for uc in ucs if len([is_apical(mtg,cuc) for cuc in vegetative_children(mtg,uc)]) == 0]
    axial_axe_lengths = list(map(axial_axe_length, last_apical_ucs))
    maxlength = max(axial_axe_lengths)
    histo = [0 for i in range(maxlength+1)]
    for l in axial_axe_lengths:
        histo[l] += 1    
    print('Done',repr(mtgfname), '...')
    return histo

def p_branch_length_histogram(params):
    result, mtgfname = params
    result[mtgfname] = branch_length_histogram(mtgfname)


axe_length_distrib_fname = 'axe_length_distribution.pkl'
def restricted_branch_length_histogram(glm = eSelectedGlm, restriction = None, plotting = False):
    from multiprocessing import Pool, Manager, cpu_count
    from vplants.mangosim.util_date import MonthEn
    Month = dict( [(mi,v) for v,mi in list(MonthEn.items())] )

    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[0],  'ESTIMATIONTYPE' : glm, 'FACTORRESTRICTION' : restriction}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, axe_length_distrib_fname)
    if not os.path.exists(distrib_file):
        print('Retrieve MTGs')
        if not os.path.exists(join(inputdir,'collect_'+axe_length_distrib_fname)):
            mtgs = retrieve_mtgs(inputdir)
            m = Manager()
            result = m.dict()
            p_branch_length_histogram([result,'reference'])
            params = [[result, mtg] for mtg in mtgs]
            p = Pool(processes = cpu_count())
            print('Estimate branch_length_histogram')
            p.map(p_branch_length_histogram, params)
            dump_obj(dict(result),'collect_'+axe_length_distrib_fname,inputdir)
        else: result = load_obj('collect_'+axe_length_distrib_fname,inputdir)
        values = []
        print(len(result))
        for k,v in list(result.items()):
            if k == 'reference':
                refvalues = v
            else:
                values.append(v)        
        dump_obj((values,refvalues),axe_length_distrib_fname,inputdir)
    else:
        print('Retreive branch_length_histogram from',repr(str(join(inputdir,axe_length_distrib_fname))))
        values, refvalues = load_obj(axe_length_distrib_fname,inputdir)
    if plotting :
        maxl = max(max(list(map(len,values))),len(refvalues))
        print(maxl)
        for v in values:
            del v[0]
            if len(v) < maxl: v += [0 for i in range(maxl-len(v))]
        del refvalues[0]
        if len(refvalues) < maxl:
             refvalues += [0 for i in range(maxl-len(refvalues))]
        plot_histo(list(range(1,maxl)), values, '', refvalues) #'Branch length', refvalues)


def get_branch_length_histograms(treename = None, verbose = False):
    allvalues = []

    allsetvalues = []
    grefvalues = None
    for glmtype, withindelaymethodval,restriction in list(itertools.product([eSelectedGlm],[eMonthMultiVariateForWithin],[None]+list(RestrictionName.keys())))+[(eNullGlm,eMonthMultiVariateForWithin, None )]:
    #for glmtype, withindelaymethodval,restriction in [(eSelectedGlm,eMonthMultiVariateForWithin, None ),(eNullGlm,eMonthMultiVariateForWithin, None )]:
        params = {'WITHINDELAYMETHOD' : withindelaymethodval, 'ESTIMATIONTYPE' : glmtype, 'FACTORRESTRICTION' : restriction}
        inputdir = get_glm_mtg_repository( params = params)
        if not treename is None: laxe_length_distrib_fname = treename+'_'+axe_length_distrib_fname
        else: laxe_length_distrib_fname = axe_length_distrib_fname
        distrib_file = join(inputdir, laxe_length_distrib_fname)
        if verbose : print('Retreive burst date distribution from',repr(str(distrib_file)))
        values, refvalues = load_obj(laxe_length_distrib_fname,inputdir)
        if grefvalues is None: grefvalues = refvalues
        allsetvalues.append(values)
        #allvalues.append([mean([values[j][i] for j in range(len(values))] ) for i in range(len(values[0]))])

    maxl = max(max([max(list(map(len,v))) for v in allsetvalues]),len(refvalues))
    allvalues = [grefvalues + [0 for i in range(maxl+1-len(grefvalues))]]
    allvalues += [[mean([v[i] if len(v) > i else 0 for v in _allvalues  ])  for i in range(maxl+1)] for _allvalues in allsetvalues]

    legends = ['Reference','GLM']+[a+' '+b for a,b in itertools.product(['GLM'],[m.replace('_',' ') for m in list(RestrictionName.values())])]+['Null GLM']
    return allvalues, legends



def histogram_distances(allvalues, legends):
    ref = allvalues[1] # we take GLM as reference value
    #del allvalues[0]
    #del allvalues[1]
    results = []
    for d in allvalues[2:]:
        print(ref)
        print(d)
        print(histogram_distance(ref,d))
        results.append(histogram_distance(ref,d))
    print(results)
    res = [(old_div(r,results[-1])) for r in results]
    for l,r in zip(legends[2:], res):
        print(l,r)

def branch_length_histogram_comparison(treename = None):
    allvalues,legends = get_branch_length_histograms(treename)
    legends = ['Reference','GLM', 'GLM without Ancestor Fate','GLM without Burst Date','GLM without Position','GLM without Ancestor Position','Null GLM']    
    # for i in reversed(range(2,6)):
    #     del allvalues[i]
    #     del legends[i]
    allvalues2 = []
    for values in allvalues:
        sv = float(sum(values))
        allvalues2.append([old_div(v,sv) for v in values[1:]])
    allvalues= allvalues2

    #histogram_distances(allvalues, legends)
    del allvalues[0]
    allvalues.insert(1,allvalues[-1])
    del allvalues[-1]
    allvalues.insert(-2,allvalues[3])
    del allvalues[3]

    legends = ['GLM','Null GLM','GLM without Burst Date','GLM without Position', 'GLM without Ancestor Fate','GLM without Ancestor Position']
    for v in allvalues: print(len(v))
    plot_histo_curve(list(range(1,len(allvalues[0]))), allvalues, 'Distribution of branch lengths'+ (' in '+treename if treename else ''), legends=legends,linewidth=2)



if __name__ == '__main__':
    #burst_date_distribution_ks_test()
    #process_burst_date_distribution()
    #burst_date_distribution_comparison()
    #burst_date_distribution_comparison()
    #bloom_date_distribution()
    trees = ['B10', 'B12', 'B14', 'F2', 'F6']
    #tree = 'F6'
    #tree_burst_date_distribution(treename = tree)
    #tree_burst_date_distribution(treename = tree, glmestimation=eSelectedGlm)
    #burst_date_distribution_comparison(treename = tree)
    #for restriction in RestrictionName.keys():
    #    restricted_burst_date_distribution(restriction)
    burst_date_distribution()
    #burst_date_distribution_comparison0()
    #histogram_distances()
    #histo = branch_length_histogram()
    #plot_histo(range(len(histo)), [histo], _title = 'Branch length')
    #restricted_branch_length_histogram(eNullGlm)
    #for restriction in RestrictionName.keys():
    #    restricted_branch_length_histogram(restriction=restriction)
    #branch_length_histogram_comparison()
    #burst_date_histogram_distances()
    #burst_date_distribution(0,eSelectedGlm)
    #restricted_branch_length_histogram(plotting=True)
    pass
