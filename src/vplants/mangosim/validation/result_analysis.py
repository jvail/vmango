import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
from vplants.mangosim.util_path import *
import os
from plot_distribution import *
from numpy import mean
import itertools
import matplotlib.pyplot as plt


figoutdir = 'figures'

def retrieve_mtgfiles(inputdir, nb = None):
    print 'Dir:',inputdir
    import glob
    mtgfiles = glob.glob(join(inputdir,mtgfname.format('*')))
    mtgfiles.sort()
    if nb: mtgfiles = mtgfiles[:nb]
    return mtgfiles

def retrieve_mtgs(inputdir, nb = None):
    mtgs = []
    for  mtgfile in retrieve_mtgfiles(inputdir, nb):
        print 'Retreive '+repr(os.path.basename(mtgfile))
        mtgs.append(load_obj(mtgfile))
    return mtgs

def mykeyhandler(event):
    if event.key == 'escape': plt.close()
    else: print('you pressed', event.key, event.xdata, event.ydata)

from vplants.mangosim.utils.util_parallel import *


class Invokable:
    def __init__(self, name, title):
        self.name = name 
        self.title = title

    def __call__(self, *args, **kwd):
        pass
        
class Evaluator (Invokable):
    def __init__(self, name, func, reducefuncs, title, reference = True, verbose = True, fruitmodel = False):
        Invokable.__init__(self, name, title)
        self.func = func
        self.reducefuncs = reducefuncs
        if type(reducefuncs) != list : self.reducefuncs = [reducefuncs]
        self.reference = reference
        self.verbose = verbose
        self.funcargs = []
        self.funckwds = {}
        self.reduseargs = []
        self.reducekwds = {}
        self.paramtargets = []
        self.target_tree = 'all'
        self.fruitmodel = fruitmodel

    def addtarget(self, glm = eInteractionGlm, restriction = None, **args):
        if type(glm) != list: glm = [glm]
        if type(restriction) != list: restriction = [restriction]
        baseparams = {'GLM_TYPE' : glm, 'GLM_RESTRICTION' : restriction}
        if not 'FRUIT_MODEL' in args:
            baseparams['FRUIT_MODEL'] = [self.fruitmodel]
        for name, argval in args.items():
            if type(argval) != list: baseparams[name] = [argval]
            else : baseparams[name] = argval

        paramtargets = list(itertools.product(*baseparams.values()))
        for paramvalue in paramtargets:
            params = dict(zip(baseparams.keys(),paramvalue))
            self.paramtargets.append(params)
        return self

    def targettree(self, treename):
        self.target_tree = treename

    def allrestrictions(self, glm = eInteractionGlm):
        self.addtarget(restriction=[None, eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction])
        return self

    def alluniquefactors(self, glm = eInteractionGlm):
        self.addtarget(restriction=[None, eBurstDateOnlyRestriction, ePositionAOnlyRestriction, ePositionAncestorAOnlyRestriction, eNatureFOnlyRestriction, eAllRestriction])
        return self

    def get_param_targets(self):
        if len(self.paramtargets) == 0:
            #return  [{'GLM_TYPE' : eInteractionGlm, 'GLM_RESTRICTION' : eAllRestriction, 'FRUIT_MODEL' : False}]
            return  [{'GLM_TYPE' : eInteractionGlm, 'GLM_RESTRICTION' : None, 'FRUIT_MODEL' : False}]
        else:
            return self.paramtargets

    def fruitbranchsizetest(self, sizerange = None):
        if sizerange is None:
            path = get_option_glm_mtg_repository(fruitmodel = True, estimationtype = None, fruitbranchsize = 1)
            fbs_prefix = 'fruitbranchsize'
            while not os.path.basename(path).startswith(fbs_prefix):
                path = os.path.dirname(path)
            path = os.path.dirname(path)
            import glob
            pathes = glob.glob(os.path.join(path,fbs_prefix+'*'))
            sizerange = [int(os.path.basename(lpath)[len(fbs_prefix):]) for lpath in pathes]
            print 'Fruit Branch Size Range :', sizerange

        self.addtarget(WITH_GLM = False, FRUIT_MODEL = True, FRUITBRANCHSIZE = sizerange)
        pass

    def configure(self, *args, **kwds):
        self.funcargs = args
        self.funckwds = kwds
        return self

    def reduceconfigure(self, *args, **kwds):
        self.reduseargs = args
        self.reducekwds = kwds
        return self

    def apply(self, nb = None, force = False, parallel = True, saving = False):
        valuesset, refvalues = self.compute(nb, force, parallel)
        self.reduce(valuesset, refvalues, saving)

    def compute(self, nb = None, force = False, parallel = True):
        cachebasename = self.cachebasename()
        import time
        valuesset = []
        if self.reference:
            refvalues = self._applyto(eMeasuredMtg)
        else:
            refvalues = None
        for params in self.get_param_targets():
            inputdir = get_glm_mtg_repository( params = params)
            cachefile = join(inputdir, cachebasename)
            computation = force or not exists(cachefile)
            if not computation :
                mtgfiles = retrieve_mtgfiles(inputdir)
                lastcreation = max([os.stat(fname).st_mtime for fname in mtgfiles])
                computation = lastcreation > os.stat(cachefile).st_mtime 
            if computation:
                mtgfiles = retrieve_mtgfiles(inputdir)
                values = []
                if not nb is None: mtgfiles = mtgfiles[:nb]
                t = time.time()
                if not parallel:
                    for mtgfile in mtgfiles:
                        values.append(self._applyto(eSimulatedMtg, mtgfile))
                else:
                    values += nparmap(lambda mfile : self._applyto(eSimulatedMtg, mfile), mtgfiles)
                print 'Applied in', time.time()-t,'sec.'
                dump_obj(values, cachebasename, inputdir)
            else:
                values = load_obj(cachebasename, inputdir)
            valuesset.append( (params,values) )
        return valuesset, refvalues

    def reduce(self, valuesset, refvalues, saving):
        import matplotlib.pyplot as plt
        for func, filename in zip(self.reducefuncs, self.saved_filenames()):
            func(refvalues, valuesset, *self.reduseargs, **self.reducekwds)
            if saving:
                if not os.path.exists(figoutdir):
                    os.makedirs(figoutdir)
                print 'Save',repr(filename)
                plt.savefig(filename,  bbox_inches='tight')
                plt.close()
            else:
                fig = plt.gcf()
                fig.canvas.mpl_connect('key_press_event', mykeyhandler)
                plt.show()

        return self

    def cachebasename(self):
        return 'cache_'+self.name+'_'+self.target_tree+'.pkl'

    def cache_files(self):
        cachebasename = self.cachebasename()
        return [join(get_glm_mtg_repository( params = params), cachebasename) for params in self.get_param_targets()]

    def savedbasename(self):
        fname = self.name+'_'+self.target_tree+'_'
        glmtargets = self.get_param_targets()
        if len(glmtargets) > 1:
            fname += paramset_stringid(glmtargets)
            #fname += '-comp-'
            #fname += '-'.join([options_stringid(p) for p in glmtargets])
        else:
            fname += '-'+param_stringid(glmtargets[0])

        return fname

    def saved_filenames(self):
        fname = self.savedbasename()
        fnames = [os.path.join(figoutdir,fname+'--'+str(i)+'.png') for i in xrange(len(self.reducefuncs))]
        return fnames

    def isUptodate(self):
        caches = self.cache_files()
        for c in caches: 
            if not os.path.exists(c): return False
        images = self.saved_filenames()
        if type(images) != list: images = [images]
        for i in images: 
            if not os.path.exists(i): return False

        return max([os.stat(c).st_mtime for c in caches]) < min([os.stat(i).st_mtime for i in images])

    def _applyto(self, mtgtype, mtgfile = None):
        setMtgStyle(mtgtype)
        if mtgtype == eSimulatedMtg : 
            if self.verbose : print 'Process', repr(os.path.basename(mtgfile))
            mtg = load_obj(mtgfile)
            mtg.fname = mtgfile
        else : 
            if self.verbose : print 'Process reference'
            mtg = get_mtg()
        if type(mtg) != MTG: raise ValueError(mtg)
        return self.func(mtg, mtgtype, *self.funcargs, **self.funckwds)


    def __call__(self, *args, **kwd):
        self.apply(*args, **kwd)

    def get_all_gus(self, mtg):
        if self.target_tree != 'all':
            return mm.get_all_gus_of_tree(mtg, get_tree_from_name(mtg, self.target_tree))
        else:
            return mm.get_all_gus_of_variety(mtg, eLoaded, 'cogshall')

    def get_all_inflos(self, mtg):
        if self.target_tree != 'all':
            return mm.get_all_inflo_of_tree(mtg, get_tree_from_name(mtg, self.target_tree))
        else:
            return mm.get_all_inflo_of_variety(mtg, eLoaded, 'cogshall')

    def get_terminal_gus_at_cycle(self, mtg, cycle):
        if self.target_tree != 'all':
            return mm.get_terminal_gus_of_tree_at_cycle(mtg, get_tree_from_name(mtg, self.target_tree), cycle=cycle)
        else:
            return mm.get_terminal_gus_of_variety_at_cycle(mtg, cycle=cycle, loaded= eLoaded, variety="cogshall")

    def get_all_gus_at_cycle(self, mtg, cycle):
        if self.target_tree != 'all':
            return mm.get_all_gus_of_tree_at_cycle(mtg, get_tree_from_name(mtg, self.target_tree), cycle=cycle)
        else:
            return mm.get_all_gus_of_variety_at_cycle(mtg, cycle=cycle, loaded= eLoaded, variety="cogshall")

    def get_all_inflos_at_cycle(self, mtg, cycle):
        if self.target_tree != 'all':
            return mm.get_all_inflo_of_tree_at_cycle(mtg, get_tree_from_name(mtg, self.target_tree), cycle=cycle)
        else:
            return mm.get_all_inflo_of_variety_at_cycle(mtg, cycle=cycle, loaded= eLoaded, variety="cogshall")

    def maketitle(self, text):
        if self.target_tree == 'all':
            return 'Cogshall : '+text
        else:        
            return self.target_tree+' : '+text


def monthtranslate(txt):
        m = [('janv','jan.'),('fev','feb.'),('mars','mar.'),('avril','apr.'),('mai','may'),('juin','june'),('juil','july'),('aout','aug.'),('sept','sep.'),('oct','oct.'),('nov','nov.'),('dec','dec.')]
        for p,nm in m:
            txt = txt.replace(p,nm)
        return txt


def __strip_histo(histo):
    # We remove the first and last part of the histo if it is equal to 0.
    keyvalues = histo.keys()
    for d in keyvalues:
        if histo[d] == 0:
            del histo[d]
        else : break
    if len(histo) > 0:
        for d in reversed(keyvalues):
            if histo[d] == 0:
                del histo[d]
            else : break

def ks_2samp(hist1, hist2):
    from scipy.stats import ks_2samp
    res = ks_2samp(hist1, hist2)
    return res[0],res[1]

def chisquare(hist1, hist2):
    from scipy.stats import chisquare
    #print len(hist1), len(hist2)
    h1, h2 = [],[]
    for v1,v2 in zip(hist1,hist2):
        if v1 < 5 or v2 < 5:
            if v1 == v2: pass
            elif len(h1) == 0 :
                h1.append(v1)
                h2.append(v2)
            else: 
                h1[-1] += v1
                h2[-1] += v2
        else:
            if len(h1) > 0 and (h1[-1] < 5 or h2[-1] < 5):
                h1[-1] += v1
                h2[-1] += v2
            else:
                h1.append(v1)
                h2.append(v2)
    print
    #print hist1
    #print hist2
    print h1
    print h2
    res = chisquare(h1, f_exp=h2)
    return res[0],res[1]

def normalize_histo(histo):
    sh1 = float(sum(histo))
    return [v1/sh1 for v1 in histo]

def kullback_leibler_divergence(hist1, hist2):
    return sum([v1 * log(v1/v2) for v1,v2 in zip(hist1, hist2) if abs(v1) > 0 and abs(v2) > 0] )

def sym_kullback_leibler_divergence(hist1, hist2):
    assert len(hist1) == len(hist2)
    from math import log
    h1 = normalize_histo(hist1)
    h2 = normalize_histo(hist2)
    divkl_h1_h2 = kullback_leibler_divergence(h1, h2)
    divkl_h2_h1 = kullback_leibler_divergence(h2, h1)
    return divkl_h1_h2+divkl_h2_h1

def bhattacharyya_histo_distance(hist1, hist2):
    assert len(hist1) == len(hist2)
    from math import log, sqrt
    h1 = normalize_histo(hist1)
    h2 = normalize_histo(hist2)
    return -log(sum([sqrt(p*q) for p,q in zip(h1,h2)]))

def chi_squared_histo_distance(hist1, hist2):
    h1 = normalize_histo(hist1)
    h2 = normalize_histo(hist2)
    return sum([pow(v1-v2,2)/(v1+v2) for v1,v2 in zip(h1,h2) if abs(v1) > 0 or abs(v2) > 0])/2.

def l2_normed_histo_distance(hist1, hist2):
    h1 = normalize_histo(hist1)
    h2 = normalize_histo(hist2)
    return l2_histo_distance(h1,h2) 

from math import sqrt

def l2_histo_distance(hist1, hist2):
    assert len(hist1) == len(hist2)
    return sqrt(sum([pow(v1-v2,2) for v1,v2 in zip(hist1,hist2)]))

def rmsd(hist1, hist2):
    assert len(hist1) == len(hist2)
    return sqrt(np.mean([pow(v1-v2,2) for v1,v2 in zip(hist1,hist2)]))

def normalized_rmsd(hist1, hist2):
    assert len(hist1) == len(hist2)
    hist1 = normalize_histo(hist1)
    hist2 = normalize_histo(hist2)
    return rmsd(hist1, hist2) # / (max(max(hist1),max(hist2)) - min(min(hist1),min(hist2)))

def normalized_rmsd2(hist1, hist2):
    assert len(hist1) == len(hist2)
    return rmsd(hist1, hist2)  / (max(max(hist1),max(hist2)) - min(min(hist1),min(hist2)))

def histogram_distance(hist1, hist2):
    return rmsd(hist1, hist2)
    #return normalized_rmsd(hist1, hist2)
    #return l2_normed_histo_distance(hist1, hist2)
    #return sym_kullback_leibler_divergence(hist1, hist2)
    #return bhattacharyya_histo_distance(hist1, hist2)

def histogram_distances(reference, allvalues, nullmodel = None ):
    results = []
    for k,d in allvalues:
        results.append((k,histogram_distance(reference,d)))
    if nullmodel:
        nullmodelval = results[nullmodel][1]
        results = [(k,v/nullmodelval)for k,v in results]
    return results

def meanarray(values):
    import numpy as np
    return [np.mean([v[i] for v in values]) for i in xrange(len(values[0]))]

def meanarrays(kvalues):
    if type(kvalues) == dict:
        return dict([(k,meanarray(a)) for k,a in kvalues.items()])
    else:
        return [(k,meanarray(a)) for k,a in kvalues]

def flattenarray(values):
   from itertools import chain
   return list(chain(*[vi if type(vi) in [tuple,list] else [vi] for vi in values] ))

def flattenarrays(kvalues):
    if type(kvalues) == dict:
       return dict([(k,[flattenarray(values) for values in valuesset]) for k,valuesset in kvalues.items()])
    else:
       return [(k,[flattenarray(values) for values in valuesset]) for k,valuesset in kvalues]


def selectsubarray(values, indices):
    return [values[i] for i in indices]

def selectsubarrays(kvalues, indices):
    if type(kvalues) == dict:
       return dict([(k,[selectsubarray(values, indices) for values in valuesset])  for k,valuesset in kvalues.items()])
    else:
       return [(k,[selectsubarray(values, indices) for values in valuesset])  for k,valuesset in kvalues]


def maxlength(kvalues):
    if type(kvalues) == dict:
        return max([max(map(len,ivalues)) for ivalues in kvalues.values()])
    else:    
        return max([max(map(len,ivalues)) for iparam, ivalues in kvalues])

def homogenize_histo_length(refvalues, kvalues):
    maxl = max(maxlength(kvalues), len(refvalues))
    kvalueshistos = kvalues.values() if type(kvalues) == dict else [v for k,v in kvalues]
    for ivalues in kvalueshistos:
        for v in ivalues:
            if len(v) < maxl: v += [0 for i in xrange(maxl-len(v))]
    if len(refvalues) < maxl:
         refvalues += [0 for i in xrange(maxl-len(refvalues))]
    return maxl

restrictions = list(reversed([None, eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction]))
def sortedvalues(kvalues):
    return [kvalues[(eInteractionGlm,r)] for r in restrictions if (eInteractionGlm,r) in kvalues]

def sortedkeys(kvalues):
    return [(eInteractionGlm,r) for r in restrictions if (eInteractionGlm,r) in kvalues]

def find_param(kvalues, **paramvalues):
    for i, (p,v) in enumerate(kvalues):
        for pname, pvalue in paramvalues.items():
            try:
                assert p[pname] == pvalue
            except:
                break
        else:
            return i

def histodistances(meankvalues, refsimu = {'GLM_RESTRICTION' : None}, nullsimu = {'GLM_RESTRICTION' : eAllRestriction}):
    values = list(meankvalues)
    if type(refsimu) == dict:
        refsimuid =  find_param(meankvalues, **refsimu)
        del values[refsimuid]
        reference = meankvalues[refsimuid][1]
    else:
        reference = refsimu
    if nullsimu:
        nullsimuid = find_param(meankvalues, **nullsimu)
        if not nullsimuid is None and type(refsimu) == dict: nullsimuid -= 1
    else: 
        nullsimuid = None
    histo =  histogram_distances(reference, values, nullsimuid)
    res = [v for k,v in histo]
    if type(refsimu) == dict:
        res.insert(refsimuid, 0)
    print '\t'.join(map(str,res))
    return res

def histodistance_legend(meankvalues, refsimu = {'GLM_RESTRICTION' : None}, nullsimu = {'GLM_RESTRICTION' : eAllRestriction}):
    res = histodistances(meankvalues, refsimu, nullsimu)
    res = map(str,res) #['%.3f' % v for v in res]
    return res 

def histogram(values):
    #from numpy import histogram
    #return list(histogram(values)[0])
    from collections import Counter
    c = Counter(values)
    k = c.keys()
    return [c.get(ki,0) for ki in xrange(min(k), max(k)+1)]

def histogram_comparison(refvalues, kvalues):
    meankvalues = meanarrays(kvalues)
    print 'RMSD :', rmsd(refvalues, meankvalues[0][1])
    print 'N RMSD :', normalized_rmsd(refvalues, meankvalues[0][1])
    print 'N2 RMSD :', normalized_rmsd2(refvalues, meankvalues[0][1])
    print 'Kol-Simrnov :', ks_2samp(refvalues, meankvalues[0][1])
    print 'Chi-Square :', chisquare(meankvalues[0][1], refvalues)
    print rmsd(refvalues, meankvalues[0][1]),'\t',normalized_rmsd(refvalues, meankvalues[0][1]),'\t',ks_2samp(refvalues, meankvalues[0][1])[0],'\t',ks_2samp(refvalues, meankvalues[0][1])[1]

def plot_histogram(refvalues, kvalues, xlabels, title, titlelocation = 2, xlabelrotation = 0, figsize = None):
    assert len(xlabels) == len(kvalues[0][1][0]) == len(refvalues)
    if len(kvalues) == 1:
        histogram_comparison(refvalues, kvalues)
        fig, ax = plot_histo(xlabels, allvalues=kvalues[0][1], _title=title, reference=refvalues, legendtag = kvalues[0][0], titlelocation = titlelocation, xlabelrotation = xlabelrotation, figsize = figsize)
    else:
        print 'trace'
        meankvalues = meanarrays(kvalues)
        legends = histodistance_legend(meankvalues, refvalues)
        fig, ax = plot_histos_means(xlabels, [v for k,v in meankvalues], title+' Comparison', reference=refvalues, legends = legends, legendtags = [k for k,v in meankvalues], titlelocation = titlelocation, xlabelrotation = xlabelrotation, figsize = None)
    return fig, ax


################################## Evaluator ##############################

from vplants.mangosim.util_date import Month
class burst_date_distribution(Evaluator):
    def __init__(self):
        Evaluator.__init__(self, 'burst',self.determine_distribution, self.plot, 'Burst Date Distribution')
        self.Month = dict([(i,v) for v,i in Month.items()])
        self.daterange = monthdate_range(vegetative_cycle_end(3),vegetative_cycle_begin(6))

    def determine_distribution(self, mtg, mtgype):
        from collections import OrderedDict
        histo_date = OrderedDict([(d,0) for d in self.daterange])
        ucs =  self.get_all_gus(mtg)
        for uc in ucs:
            try:
                d = get_burst_date(mtg,uc)
                m = d.month
                y = d.year
                histo_date[(y,m)] += 1
            except : pass
        return histo_date.values()

    def plot(self, refvalues,kvalues):
        strdate = lambda d : monthtranslate(self.Month[d[1]])+'-'+str(d[0])
        dates     = map(strdate, self.daterange)
        plot_histogram(refvalues, kvalues, dates, self.maketitle('Burst Dates'), titlelocation = 2, xlabelrotation = 80, figsize=(10,4))
        plt.xlabel('Month')
        plt.ylabel('Number of new growth units')

class bloom_date_distribution(Evaluator):

    def __init__(self):
        Evaluator.__init__(self,'bloom',self.date_week_distribution, self.plot, 'Bloom Date Distributon')
        self.daterange = (#[(2003,None)]+weekdate_range(flowering_cycle_begin(3),flowering_cycle_end(3))+
                 #[(2004,None)]+weekdate_range(flowering_cycle_begin(4),flowering_cycle_end(4))+
                 [(2005,None)]+weekdate_range(flowering_cycle_begin(5),flowering_cycle_end(5)))

    def date_week_distribution(self, mtg, mtgtype):
        from collections import OrderedDict
        def toweekid(inflo):
            date = get_bloom_date(mtg,inflo)
            if date is None:
                return (2000+get_unit_cycle(mtg,inflo),None)
            d = date.isocalendar()
            return (d[0],d[1]) # retrieve year and week number

        histo_date = OrderedDict([(d,0) for d in self.daterange])

        inflos = self.get_all_inflos(mtg)
        for inflo in inflos:
            try:
                d = toweekid(inflo)
                assert d in histo_date
                histo_date[d] += 1
            except : pass
        #if strip : __strip_histo(histo_date)
        return histo_date.values()

    def plot(self, refvalues, kvalues):
        strdate = lambda d : str(d[1])# +'-'+str(d[0]-2000)
        dates     = map(strdate, self.daterange)
        del refvalues[0:5]
        del dates[0:5]
        for k,valueset in kvalues:
            for values in valueset: 
                del values[0:5]
        plot_histogram(refvalues, kvalues, dates, self.maketitle('Bloom Dates'), titlelocation = 2, figsize=(5.5,4))
        plt.xlabel('Week')
        plt.ylabel('Number of Blooming Inflorescences')



class harvest_date_distribution(Evaluator):

    def __init__(self):
        Evaluator.__init__(self,'harvest',self.date_week_distribution, self.plot, 'Harvest Date Distributon')
        self.daterange = (#[(2003,None)]+weekdate_range(fruiting_cycle_begin(3),fruiting_cycle_end(3))+
                 #[(2004,None)]+weekdate_range(fruiting_cycle_begin(4),fruiting_cycle_end(4))+
                 weekdate_range(fruiting_cycle_begin(5),fruiting_cycle_end(5)))

    def date_week_distribution(self, mtg, mtgtype):
        from collections import OrderedDict
        def toweekid(inflo):
            date = get_fruits_harvest_date(mtg,inflo)
            if date is None:
                return (2000+get_unit_cycle(mtg,inflo),None)
            d = date.isocalendar()
            return (d[0],d[1]) # retrieve year and week number

        histo_date = OrderedDict([(d,0) for d in self.daterange])

        inflos = self.get_all_inflos(mtg)
        for inflo in inflos:
            if mm.get_nb_fruits(mtg,inflo) > 0:
                try:
                    d = toweekid(inflo)
                    assert d in histo_date
                    histo_date[d] += 1
                except : pass
        #if strip : __strip_histo(histo_date)
        return histo_date.values()

    def plot(self, refvalues,kvalues):
        strdate = lambda d : str(d[1])#+'-'+str(d[0]-2000)
        dates     = map(strdate, self.daterange)
        del refvalues[0:5]
        del dates[0:5]
        for k,valueset in kvalues:
            for values in valueset: 
                del values[0:5]
        fig, ax = plot_histogram(refvalues, kvalues, dates, self.maketitle('Harvest Dates'), titlelocation = 2)
        plt.xlabel('Week')
        plt.ylabel('Number of harvested fruits')



class tree_branch_length(Evaluator):
    def __init__(self):
        Evaluator.__init__(self,'branch',self.determine_histogram, self.plot, 'Axial Branches Length Distribution')

    def determine_histogram(self, mtg, mtgype):
        ucs =  self.get_all_gus(mtg)
        def axial_axe_length(uc):
            cuc = uc
            l = 1
            parent = mtg.parent(cuc)
            while parent and (mtg.edge_type(parent) == '<') and (get_unit_cycle(mtg, parent) > 3):
                cuc = parent
                parent = mtg.parent(cuc)
                l += 1
            return l
        last_apical_ucs = [uc for uc in ucs if len([cuc for cuc in vegetative_children(mtg,uc) if is_apical(mtg,cuc)]) == 0]
        axial_axe_lengths = map(axial_axe_length, last_apical_ucs)
        histo = histogram(axial_axe_lengths)
        return histo

    def plot(self, refvalues,kvalues):
        print refvalues
        maxl = homogenize_histo_length(refvalues,kvalues)
        fig, ax = plot_histogram(refvalues, kvalues, range(1,maxl+1), self.maketitle('Branch Length'), titlelocation = 1, figsize=(8,4))
        plt.xlabel('Number of Growth Units per Branch')
        plt.ylabel('Number of Branches')



class current_year_axe_length(Evaluator):
    def __init__(self):
        Evaluator.__init__(self,'axe',self.determine_histogram, self.plot, 'Current Year Axe Length Distribution')

    def determine_histogram(self, mtg, mtgype):
        ucs = [uc for uc in self.get_all_gus(mtg) if get_unit_cycle(mtg, uc) > 3]
        roots = [uc for uc in ucs if get_unit_cycle(mtg,uc) != get_unit_cycle(mtg,mtg.parent(uc))]
        groupid = dict([(uc,uc) for uc in  roots])

        for uc in ucs :       
            if not uc in groupid:
                parents = []
                while not uc in groupid:
                    parents.append(uc)
                    uc = mtg.parent(uc)
                gid = groupid[uc]
                for p in parents:
                    groupid[p] = gid
        groupidcount = {}
        for gid in groupid.values():
            groupidcount.setdefault(gid, 0)
            groupidcount[gid] += 1
        histo = histogram(groupidcount.values())
        return histo

    def plot(self, refvalues,kvalues):
        maxl = homogenize_histo_length(refvalues,kvalues)
        plot_histogram(refvalues, kvalues, range(1,maxl+1), self.maketitle('Current Year Axe Length'), titlelocation = 1)





class organ_count_distribution(Evaluator):
    def __init__(self, name, func, reducefuncs, title):
        Evaluator.__init__(self, name, func, reducefuncs, title)
        self.begcycle, self.maxcycle = 4,6

    def plot_distributioni(self, refvalues, kvalues, labels, proprange = None, legends = None, titlelocation = 2, figsize = (10,7)):
        import numpy as np
        labels = flattenarray(labels)
        kvalues = flattenarrays(kvalues)
        if refvalues:
            refvalues = flattenarray(refvalues)
        if not proprange is None:
            for pr in proprange:
                if pr > len(refvalues) : raise ValueError(pr, len(refvalues))
            refvalues = selectsubarray(refvalues, proprange)
            kvalues = selectsubarrays(kvalues, proprange)
        if len(kvalues) == 1:
            histogram_comparison(refvalues, kvalues)
            fig, ax = plot_histo(labels, kvalues[0][1], self.maketitle('Characteritics'), refvalues, legendtag = kvalues[0][0], linestyle='o', titlelocation = titlelocation, figsize = figsize)
        else:
            meankvalues = meanarrays(kvalues)
            histodistance_legend(meankvalues)
            fig, ax = plot_histos(labels, [v for k,v in kvalues], self.maketitle('Characteritics Comparison'), reference=refvalues, legendtags = [k for k,v in kvalues], legends= legends, titlelocation = titlelocation, figsize = figsize)
            
        #fig.subplots_adjust(bottom=0.30, top = 0.94)


class terminal_count_distribution(organ_count_distribution):
    def __init__(self):
        organ_count_distribution.__init__(self,'terminal',self.determine_distribution, self.plot_distribution1, 'Terminal GU Characteristics')

    def determine_distribution(self, mtg, mtgtype):
        terminals = [self.get_terminal_gus_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        nbterminals = map(lambda t : (len ([gu for gu in t if mtg.edge_type(gu) == '<']),len ([gu for gu in t if mtg.edge_type(gu) == '+'])),terminals)
        nbfloterminals = [(len([gu for gu in cterminals if len(inflorescence_children(mtg,gu)) > 0 and mtg.edge_type(gu) == '<']),
                           len([gu for gu in cterminals if len(inflorescence_children(mtg,gu)) > 0 and mtg.edge_type(gu) == '+'])) for cterminals in terminals]
        return nbterminals+nbfloterminals

    def print_distribution(self, refvalues, kvalues):
        import numpy as np
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Nb Terminals '+str(i) for i in xrange(begcycle, maxcycle)]+['Nb Flowering Terminals '+str(i) for i in xrange(begcycle, maxcycle)]+['Nb Inflos '+str(i) for i in xrange(begcycle, maxcycle)]+['Nb Fruits '+str(i) for i in xrange(begcycle, maxcycle)]
        for k,values in kvalues.items():
            glm, restriction = k
            print RestrictionName[restriction]
            for i in xrange(len(refvalues)):
                print labels[i],'\t',
                print refvalues[i],'\t',
                if type(values[0][i]) != tuple:
                    print np.mean([val[i] for val in values]),'+-',
                    print np.std([val[i] for val in values])
                else:
                    print '(',
                    for j in xrange(len(values[0][i])):
                        print np.mean([val[i][j] for val in values]),'+-',
                        print np.std([val[i][j] for val in values]),',',
                    print ')'


    def plot_distribution1(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels =  [('Nb. Ter. Api. '+str(i),'Nb. Ter. Lat. '+str(i)) for i in xrange(begcycle, maxcycle)]
        labels += [('Nb. Flo. Ter. Api. '+str(i),'Nb. Flo. Ter. Lat. '+str(i)) for i in xrange(begcycle, maxcycle)]
        self.plot_distributioni(refvalues, kvalues, labels)
    
class nbinflos(organ_count_distribution):
    def __init__(self, plotrestriction = None): 
        organ_count_distribution.__init__(self,'nbinflos',self.determine_distribution, self.plot_distribution2, 'Number of Inflorescences')
        self.plotrestriction = plotrestriction

    def determine_distribution(self, mtg, mtgtype):
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        nbinflos = [sum([mm.nb_of_inflorescences(mtg,inflo) for inflo in inflocycle]) for inflocycle in inflos]
        if mtgtype == eMeasuredMtg:
            nbinflos[0] = len(inflos[0])
        return nbinflos

    def plot_distribution2(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels =  ['Nb. Inflorescences of cycle '+str(i) for i in xrange(begcycle, maxcycle)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
        
        legends = None

        self.plot_distributioni(refvalues, kvalues, labels, legends=legends)
        plt.ylabel('Number of Inflorescences')
    


class nbfruits(organ_count_distribution):
    def __init__(self, plotrestriction = None): 
        organ_count_distribution.__init__(self,'nbfruits',self.determine_distribution, self.plot_distribution2, 'Number of Fruits')
        self.plotrestriction = plotrestriction
        self.begcycle, self.maxcycle = 3,6

    def determine_distribution(self, mtg, mtgtype):
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        nbfruits = [sum([mm.get_nb_fruits(mtg,inflo) for inflo in inflocycle]) for inflocycle in inflos]
        return nbfruits

    def plot_distribution2(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Cycle '+str(i) for i in xrange(begcycle, maxcycle)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
        
        legends = None

        self.plot_distributioni(refvalues, kvalues, labels, legends=legends, figsize=(5,4))
        plt.ylabel('Number of Fruits')
    

class production(organ_count_distribution):
    def __init__(self, plotrestriction = None): 
        organ_count_distribution.__init__(self,'production',self.determine_distribution, self.plot_distribution2, 'Production')
        self.plotrestriction = plotrestriction
        self.begcycle, self.maxcycle = 3,6

    def determine_distribution(self, mtg, mtgtype):
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        production = [sum([mm.get_fruits_weight(mtg,inflo,0) for inflo in inflocycle])/1000. for inflocycle in inflos]
        return production

    def plot_distribution2(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Cycle '+str(i) for i in xrange(3, maxcycle)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
        
        if len(kvalues) > 1:
            #histodistances(meanarrays(kvalues))
            print meanarrays(kvalues)
            legends = ['Fruiting Branch Size : '+str(i) for i in xrange(1,8)]+['Stochastic Production']
        else : 
            legends = None

        self.plot_distributioni(refvalues, kvalues, labels, legends=legends, figsize=(5,6.5))
        plt.ylabel('Production (Kg)')
    


    

class meanfruitweight(organ_count_distribution):
    def __init__(self, plotrestriction = [1,2]): 
        organ_count_distribution.__init__(self,'meanfruitweight',self.determine_distribution, self.plot_distribution, 'Mean Fruit Weight')
        self.begcycle, self.maxcycle = 3,6
        self.plotrestriction = plotrestriction
        self.fruitbranchsizetest()
        #self.addtarget(FRUIT_MODEL=False)


    def determine_distribution(self, mtg, mtgtype):
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        #if mtgtype == eMeasuredMtg:
        production = [np.mean([mm.get_fruits_weight(mtg,inflo,0)/mm.get_nb_fruits(mtg,inflo) for inflo in inflocycle for j in xrange(mm.get_nb_fruits(mtg,inflo)) if not mm.get_fruits_weight(mtg,inflo) is None]) for inflocycle in inflos]
        #else:
        #    production = [np.mean([mm.get_fruits_weight(mtg,inflo,0) for inflo in inflocycle for j in xrange(mm.get_nb_fruits(mtg,inflo)) if not mm.get_fruits_weight(mtg,inflo) is None]) for inflocycle in inflos]
        return production

    def plot_distribution(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Cycle '+str(i) for i in xrange(begcycle, maxcycle)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
        
        if len(kvalues) > 1:
            print meanarrays(kvalues)
            print refvalues
            legends = ['Size : '+str(i) for i in xrange(1,8)]+['Stochastic Production']
        else : legends = None

        self.plot_distributioni(refvalues, kvalues, labels, legends=legends, titlelocation = 0, figsize=(5,6.5))
        #plt.xlabel('Number of Growth Units per Branch')
        plt.ylabel('Mean fruit weight (g)')
    
class fruitweight(organ_count_distribution):
    def __init__(self, plotrestriction = [1,2]): 
        organ_count_distribution.__init__(self,'fruitweight',self.determine_distribution, self.plot_distribution, 'Fruit Weight')
        self.begcycle, self.maxcycle = 3,6
        self.plotrestriction = plotrestriction
        self.fruitbranchsizetest()
        #self.addtarget(FRUIT_MODEL=False)


    def determine_distribution(self, mtg, mtgtype):
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        production = [np.array([mm.get_fruits_weight(mtg,inflo,0)/mm.get_nb_fruits(mtg,inflo) for inflo in inflocycle for j in xrange(mm.get_nb_fruits(mtg,inflo)) if not mm.get_fruits_weight(mtg,inflo) is None]) for inflocycle in inflos]
        return production

    def plot_distribution(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Cycle '+str(i) for i in xrange(begcycle, maxcycle)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
        
        if len(kvalues) > 1:
            #print refvalues
            legends = ['Size : '+str(i) for i in xrange(1,8)]+['Stochastic Production']
        else : legends = None

        self.plot_distributioni(refvalues, kvalues, labels, legends=legends, titlelocation = 0, figsize=(5,6.5))
        #plt.xlabel('Number of Growth Units per Branch')
        plt.ylabel('Fruit weight (g)')
    
    def plot_distributioni(self, refvalues, kvalues, labels, proprange = None, legends = None, titlelocation = 2, figsize = (10,7)):
        import numpy as np
        labels = flattenarray(labels)
        #print kvalues
        #from IPython import embed
        #embed()
        kvalues = [(p, ( reduce(lambda a,b : np.concatenate((a,b)), [vi[0] for vi in v]), reduce(lambda a,b : np.concatenate((a,b)),[vi[1] for vi in v])  ) ) for p,v in kvalues]
        #if refvalues:
        #    refvalues = flattenarray(refvalues)
        if len(kvalues) == 1:
            histogram_comparison(refvalues, kvalues)
            fig, ax = plot_histo(labels, kvalues[0][1], self.maketitle('Characteritics'), refvalues, legendtag = kvalues[0][0], linestyle='o', titlelocation = titlelocation, figsize = figsize)
        else:
            #meankvalues = meanarrays(kvalues)
            #histodistance_legend(meankvalues)

            fig, ax = plot_histos(labels, [v for k,v in kvalues], self.maketitle('Characteritics Comparison'), reference=refvalues, legendtags = [k for k,v in kvalues], legends= legends, titlelocation = titlelocation, figsize = figsize)


class leaf_fruit_ratio(organ_count_distribution):
    def __init__(self, plotrestriction = [1,2]):
        organ_count_distribution.__init__(self,'leaf_fruit_ratio',self.determine_distribution, self.plot, 'Leaf Fruit Ratio')
        self.begcycle, self.maxcycle = 3,6        
        self.fruitbranchsizetest()
        self.plotrestriction = plotrestriction

    def determine_distribution(self, mtg, mtgtype):
        if mtgtype == eMeasuredMtg:
            mean_leaf_fruit_ratio = [0 for c in xrange(self.begcycle, self.maxcycle)]
        else:
            ratio = lambda x: x[0]/float(x[1]) if x[1] > 0 else 0
            inflos = [(c,self.get_all_inflos_at_cycle(mtg, cycle=c)) for c in xrange(self.begcycle, self.maxcycle)]
            leafruit = mtg.property('leaffruit_ratio')
            mean_leaf_fruit_ratio = []
            for c,inflocycle in inflos:
                ratios = [ratio(leafruit[inflo]) for inflo in inflocycle if inflo in leafruit]
                if len(ratios) > 0:
                    mean_leaf_fruit_ratio.append(np.mean(ratios))
                else:
                    print mtg.fname,c,ratios, len(inflocycle), len([i for i in inflocycle if mtg.property('nb_fruits')[i] > 0])
                    mean_leaf_fruit_ratio.append(None)

            #mean_leaf_fruit_ratio = [np.mean([ratio(leafruit[inflo]) for inflo in inflocycle if inflo in leafruit]) for inflocycle in inflos ]
        return mean_leaf_fruit_ratio

    def plot(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Cycle '+str(i) for i in xrange(begcycle, maxcycle)]
        legends = ['Fruiting Branch Size : '+str(i) for i in xrange(1,len(kvalues)+1)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
        self.plot_distributioni(None, kvalues, labels, legends=legends, titlelocation=0, figsize=(5,5))
        plt.ylabel('Number of Leaves per Fruit')

mycolorbar = None
def heatmap(data, monthrange, xlabelling = True, ylabelling = True, withcolorbar = True):
        from numpy import log10, arange, amax
        data = log10(data+1)
        mx = amax(data)
        data /= mx
        for i,(y,m) in enumerate(monthrange):
            ld = lastday_in_month(y,m)
            for d in xrange(1,32-ld):
                data[i,ld+d-1] = [1,1,1]

        #for i,j in itertools.product(*[range(d) for d in data.shape]):
        #    if data[i,j] == 0: data[i,j] = None

        ax = plt.gca()
        nbmonth = data.shape[0]
        #view = ax.matshow(data, cmap=plt.cm.jet,extent=[0,31,nbmonth,0]) 
        view = ax.imshow(data, extent=[0,31,nbmonth,0]) 
        #a.xaxis.set_major_formatter(ticker.NullFormatter())
        ax.set_xticks(range(31), minor = False)
        ax.set_xticklabels('')
        if xlabelling:
            ax.set_xticks(arange(0.5,31.5,0.5), minor = True)
            ax.set_xticklabels( [ str(i) if j == 0 else '' for i in range(1,32) for j in xrange(2)], minor = True )

        invmonth = dict([(i,v if len(v)<= 4 else v[:3]+'.') for v,i in MonthEn.items()])
        strdate = lambda d : invmonth[d[1]]+'-'+str(d[0])
        ax.set_yticks(range(len(monthrange)))
        ax.set_yticklabels('')
        if ylabelling:
            ax.set_yticks(arange(0.5,len(monthrange)+0.5,0.5), minor = True)
            ax.set_yticklabels( [ strdate(d) if j == 0 else '' for d in monthrange  for j in xrange(2)], minor = True )

        plt.grid(linestyle='--')
        print nbmonth // 12
        for i in xrange(0,(nbmonth // 12)+1):
            plt.plot([0,31],[12*i,12*i], color='blue', linewidth=2)


def heatmap_colormap(vmin, vmax):
    from math import ceil, floor
    from numpy import log10, array
    import numpy
    ax = plt.gca()
    ax.yaxis.tick_right()

    def color(i, intensity, maxi):
        stripl = 0.2*maxi
        if i <  stripl : return [0,intensity,0]
        elif i > 4*stripl : return [intensity,0,0]
        elif 2*stripl < i < 3*stripl : return [intensity,intensity,0]
        elif i <= 2*stripl:
            c = i/stripl - 1
            return [intensity*c,intensity,0]
        elif i >= 3*stripl:
            c = (i-3*stripl)/stripl
            return [intensity,intensity*(1-c),0]

    length = 800
    width = 100

    data = array([[color(i,(length-j)/float(length),width) for i in xrange(width)] for j in xrange(length)]).astype(float)
    view = ax.imshow(data) 

    vmaxlog = log10(vmax)
    ticklength = int(length / vmaxlog)
    ax.set_yticks([length-1-i*ticklength for i in xrange(int(floor(vmaxlog))+1)])
    ax.set_xticks([])

    ticklabels = []
    for k in xrange(int(floor(vmaxlog))+1):
        ticklabels += [str(10**k if k > 0 else 0)]
    ax.set_yticklabels(ticklabels)





class stage_evaluator(Evaluator):
    def __init__(self):
        Evaluator.__init__(self, 'stage',self.determine_distribution, self.plot, 'Sensible Phenological Stage Distribution')
        self.begindate = vegetative_cycle_begin(4)
        self.enddate   = flowering_cycle_end(5)+timedelta(days=1)
        self.monthrange = monthdate_range(self.begindate,self.enddate)
        from vplants.mangosim.temperature import init_temperatures
        init_temperatures()

    def date_to_dayid(self, date):
        if self.begindate <= date <=  self.enddate:
            dm = month_difference(date, self.begindate)
            return dm * 31 + date.day - 1
        elif self.begindate > date: return 0
        else : return (len(self.monthrange)*31) - 1

    def dayid_to_date(self, did):
        nbm = did / 31
        nbd = did % 31
        nbyear = nbm / 12
        nbm = nbm % 12
        ayear = self.begindate.year+nbyear
        amonth = self.begindate.month+nbm
        aday = self.begindate.day+nbd
        if lastday_in_month(ayear,amonth) >= aday:
            return date(ayear, amonth, aday)
        else:
            return (ayear, amonth, aday)

    def daterange_to_dayids(self, datebeg, dateend):
        fid = self.date_to_dayid(datebeg)
        lid = self.date_to_dayid(dateend)
        if datebeg.year != dateend.year or datebeg.month != dateend.month:
            result = range(fid,fid+(lastday_in_month(datebeg.year,datebeg.month)-datebeg.day)+1)
            mrange = monthdate_range(datebeg,dateend)
            mrange.pop(0)
            for m,y in mrange:
                result += range(self.date_to_dayid(date(y,m,1)),self.date_to_dayid(date(y,m,lastday_in_month(y,m))))
            result += range(self.date_to_dayid(date(dateend.year,dateend.month,1)),lid+1)
            return result
        else:
            return range(fid, lid)

    def determine_distribution(self, mtg, mtgype):
        from vplants.mangosim.thermaltime import MultiPhaseThermalTimeAccumulator
        from vplants.mangosim.simulation.organ_properties import GUManager, InfloManager
        from random import randint
        result = [0 for m in self.monthrange for i in xrange(31)]
        gumanager = GUManager()
        inflomanager = InfloManager()
        ucs =  self.get_all_gus(mtg)
        ucrdate = {}
        decal = 1000
        for uc in ucs:
            try:
                begdate = get_burst_date(mtg,uc)
            except : 
                pass
            else:
                if begdate and self.begindate <= begdate <= self.enddate:
                    if uc in ucrdate:
                        begdate = ucrdate[uc]
                    else:
                        rdate = random_date_in_month(begdate.year,begdate.month)
                        parent = mtg.parent(uc)
                        if parent:
                            for sister in vegetative_children(mtg,parent):
                                if get_burst_date(mtg,sister) == begdate:
                                    ucrdate[sister] = rdate
                        begdate = rdate 

                    gu_pheno_tts     = MultiPhaseThermalTimeAccumulator(gumanager.pheno_base_temp, gumanager.pheno_stade_temp,stagesnames=gumanager.pheno_stadename)
                    enddate = gu_pheno_tts.find_date_of_stage_end('E',begdate)
                    for did in self.daterange_to_dayids(begdate, enddate):
                        result[did] += 1
        inflos =  self.get_all_inflos(mtg)
        for inflo in inflos:
            try:
                if mtgype == eMeasuredMtg:
                    begdate = get_bloom_date(mtg,inflo)
                else:
                    begdate = get_burst_date(mtg,inflo)
            except : 
                pass
            else:
                if begdate and self.begindate <= begdate <= self.enddate:
                    if mtgype == eMeasuredMtg:
                        inflo_pheno_tts     = MultiPhaseThermalTimeAccumulator(inflomanager.pheno_base_temp, inflomanager.pheno_stade_temp, inflomanager.mean_bloom_cum_temp,stagesnames=inflomanager.pheno_stadename)
                        begdate = inflo_pheno_tts.reverse_from_finaldate(0,begdate)
                    else:
                        inflo_pheno_tts     = MultiPhaseThermalTimeAccumulator(inflomanager.pheno_base_temp, inflomanager.pheno_stade_temp,stagesnames=inflomanager.pheno_stadename)
                    enddate = inflo_pheno_tts.find_date_of_stage_end('E',begdate)
                    for did in self.daterange_to_dayids(begdate, enddate):
                        result[did] += decal
        return result

    def plot(self, refvalues,kvalues):
        from math import ceil
        from numpy import  amax
        withref = False

        fig = plt.figure(figsize=(18,8))
        
        nbmonth = int(len(refvalues)/31.)

        def toarray(v): 
            return np.reshape([[a/1000,a%1000,0] for a in v], (nbmonth,31,3))

        nbplots = len(kvalues)+withref
        nbcols = ceil(sqrt(nbplots))
        nbrows = int(ceil(nbplots/nbcols))
        nbcols = int(nbcols)

        plt.subplot(nbrows, nbcols+1,1)
        if withref:
            heatmap(toarray(refvalues).astype(float), self.monthrange)
        for i,(p, valueset) in enumerate(kvalues):
            plt.subplot(nbrows, nbcols+1,i+1+withref)
            data = toarray(valueset[0]).astype(float)
            for v in valueset[1:]:
                data += toarray(v)
            data /= len(valueset)*3
            heatmap(data, self.monthrange) #, xlabelling = i <= nbcols, ylabelling = (((i+1) % nbcols) == 0))
            mx = amax(data)

        plt.subplot(nbrows, nbcols+1,nbcols+1)
        heatmap_colormap(0, mx)
        plt.subplots_adjust(wspace=0, hspace=0)
        #plt.subplots_adjust(left=5, bottom=5, right=95, top=95, wspace=0, hspace=0)



class stage_duration(Invokable):
    def __init__(self, gu = True):
        Invokable.__init__(self,'stageduration','Development Duration')
        self.gu = gu
        from vplants.mangosim.temperature import init_temperatures
        init_temperatures()

        from vplants.mangosim.thermaltime import MultiPhaseThermalTimeAccumulator
        from vplants.mangosim.simulation.organ_properties import GUManager, InfloManager
        if self.gu:
            gumanager = GUManager()
            self.pheno = MultiPhaseThermalTimeAccumulator(gumanager.pheno_base_temp, gumanager.pheno_stade_temp,stagesnames=gumanager.pheno_stadename)
        else:
            inflomanager = InfloManager()
            self.pheno = MultiPhaseThermalTimeAccumulator(inflomanager.pheno_base_temp, inflomanager.pheno_stade_temp,stagesnames=inflomanager.pheno_stadename)

        self.monthrange = []
        for cycle in range(3,6):
            if self.gu:
                begindate = vegetative_cycle_begin(cycle)
                enddate   = vegetative_cycle_end(cycle)+timedelta(days=1)
            else:
                begindate = flowering_cycle_begin(cycle)
                enddate   = flowering_cycle_end(cycle)+timedelta(days=1)
            self.monthrange += monthdate_range(begindate,enddate)


    def compute(self):
        from random import randint
        result = [[None for i in xrange(31)] for m in self.monthrange]
        for i,(y,m) in enumerate(self.monthrange):
            for d in xrange(1,lastday_in_month(y,m)):
                    begdate = date(y,m,d)
                    enddate = self.pheno.find_date_of_stage_end('F',begdate)
                    result[i][d-1] = (enddate-begdate).days
        return result

    def plot(self, values):
        self.heatmap(values, self.monthrange)

    def heatmap(self, data, monthrange, xlabelling = True, ylabelling = True):
        import numpy as np
        data = np.array(data).astype(float)
        print np.nanmin(data), np.nanmax(data)
        #for i,j in itertools.product(*[range(d) for d in data.shape]):
        #    if data[i,j] == 0: data[i,j] = None

        ax = plt.gca()
        nbmonth = len(data)
        jet = plt.cm.jet
        rjet = jet.from_list('rjet',list(reversed(map(jet,range(jet.N)))),jet.N)
        view = ax.matshow(data, cmap=rjet, extent=[0,31,nbmonth,0]) 
        #view = ax.imshow(data, extent=[0,31,nbmonth,0]) 

        ax.set_xticks(range(31), minor = False)
        ax.set_xticklabels('')
        if xlabelling:
            ax.set_xticks(np.arange(0.5,31.5,0.5), minor = True)
            ax.set_xticklabels( [ str(i) if j == 0 else '' for i in range(1,32) for j in xrange(2)], minor = True )

        invmonth = dict([(i,v if len(v)<= 4 else v[:3]+'.') for v,i in MonthEn.items()])
        strdate = lambda d : invmonth[d[1]]+'-'+str(d[0])
        ax.set_yticks(range(len(monthrange)))
        ax.set_yticklabels('')
        if ylabelling:
            ax.set_yticks(np.arange(0.5,len(monthrange)+0.5,0.5), minor = True)
            ax.set_yticklabels( [ strdate(d) if j == 0 else '' for d in monthrange  for j in xrange(2)], minor = True )
        plt.gcf().colorbar(view)
        plt.show()

    def __call__(self):
        self.plot(self.compute())
     

def get_treenames():
    return get_treenames_of_variety(mm.get_mtg(),'cogshall',eLoaded)

from collections import OrderedDict

def class_lineno(mclass):
    return mclass.__init__.im_func.func_code.co_firstlineno

def build_cmddict():
    import inspect
    cmdflags = []
    for name, obj in globals().items():
        if inspect.isclass(obj)  and issubclass(obj, Invokable) : 
            try:
                instance = obj()
            except:
                pass
            else:
                cmdflags += [(instance.name,obj)]                
    cmdflags.sort(cmp=lambda a,b :cmp(class_lineno(a[1]),class_lineno(b[1])))
    print [class_lineno(a[1]) for a in cmdflags]
    return OrderedDict(cmdflags)

cmdflags = build_cmddict()


def saveall(fruitmodel=False, comparison = True, force=False):
    for tree in ['all']+get_treenames():
        for restriction in [None, 'all'] :
            for cmd in cmdflags.values():
                p = cmd()
                if issubclass(p, Evaluator):
                    if restriction == 'all':
                        p.allrestrictions()
                    if tree != 'all':
                        p.targettree(tree)
                    p(force=force, saving=True)
    for cmd in cmdflags.values():
        p = cmd()
        if not issubclass(p, Evaluator):
            p(force=force, saving=True)


def make_report(force = False, comparison = True, fruitmodel = False):
    from vplants.mangosim.utils.util_report import TexReportGenerator
    rep = 'texreport'
    if not os.path.exists(rep): os.makedirs(rep)
    tx = TexReportGenerator(os.path.join(rep,'validationreport.tex'),'Validation Report')

    restrictions = [None]
    if comparison: restrictions.append('all')

    for cmdname, cmd in cmdflags.items():
        p = cmd()
        tx.add_section(p.title)
        for restriction in restrictions:
            tx.add_subsection('Simulation from GLM' if restriction is None else 'Factor Sensitivity Test')
            for tree in ['all']+get_treenames():
                    tx.add_subsubsection('Tree '+tree if tree != 'all' else 'Cultivar Cogshall')
                    p = cmd()
                    p.fruitmodel = fruitmodel
                    if restriction == 'all':
                        p.allrestrictions()
                    if tree != 'all':
                        p.targettree(tree)
                    if not p.isUptodate() or force:
                        p.apply(saving=True)
                    fignames = p.saved_filenames()
                    for figname in fignames:
                        tx.add_figure(os.path.join(os.pardir,figname))
                    tx.clear_page()
    tx.close()
    tx.compile()
    tx.view()

def fruitbranchsizetest(force = False):
    p = production([1,2])
    p.fruitbranchsizetest()
    #p.addtarget(FRUIT_MODEL=False)
    p(force=force)

def fruitbranchsizelf(force = False):
    p = leaf_fruit_ratio()
    #p.addtarget(FRUIT_MODEL=False)
    p(force=force,parallel=False)

def fruitbranchsizeweight(force = False):
    p = meanfruitweight()
    #p.addtarget(FRUIT_MODEL=False)
    p(force=force,parallel=False)

def test_production():
    restriction = None #range(4)
    p = production()
    p.allrestrictions()
    v1,r1 = p.compute()

    #p.targettree('F2')

    #p.targettree('B10')
    #v2,r2 = p.compute()

    #p.targettree('B12')
    #v3,r3 = p.compute()


    #r =  r1+r2+r3
    #print r
    v = [(v1[i][0],sum([meanarrays(v)[i][1] for v in [v1,v2,v3]],[])) for i in xrange(len(v1))]
    #print v
    if restriction:
        r = selectsubarray(r, restriction)
        v = [(k,selectsubarray(vi, restriction)) for k,vi in v]

    print histodistance_legend(v)

def main():
    import sys
    def help():
        print 'Available plots :',cmdflags.keys()
        print '-c  : Comparison mode'
        print '-cu  : Comparison mode with unique factor'
        print '-fm : Use fruitmodel'
        print '-ft : Fruit model test'
        print '-flt : Fruit model leaf fruit ratio test'
        print '-f  : Force reevaluation'
        print '-s  : Save figure'
        print '-sa : Save all figures'
        print '-sa1: Save all figures without comparison section'
        print '-r  : Create a report'
        print '-r1 : Create a report without comparison section'
        print '-tTREENAME : Limit the study to one tree. Available trees are',get_treenames()
    argv = sys.argv[1:]
    switches = [a for a in argv if a.startswith('-')]
    cmds = [a for a in argv if not a.startswith('-')]
    processes = []
    if len(cmds) == 0:
        cmds = ['burst']
    for cmd in cmds:
        if cmd == 'all':
            for c in cmdflags.values():
                processes.append(c())
            break
        else:
            try:
                processes.append(cmdflags[cmd]())
            except KeyError, ke:
                print 'Invalid plot :', cmd
                help()
                return

    force = '-f' in switches
    fruitmodel = '-fm' in switches
    if '-h' in switches:
        help()
        return
    if '-sa' in switches:
        saveall(force=force, fruitmodel=fruitmodel)
    elif '-sa1' in switches:
        saveall(force=force, fruitmodel=fruitmodel, comparison=False)
    elif '-r' in switches:
        make_report(force=force, fruitmodel=fruitmodel)
    elif '-r1' in switches:
        make_report(force=force, comparison=False, fruitmodel=fruitmodel)
    elif '-ft' in switches:
        fruitbranchsizetest(force=force)
    elif '-flt' in switches:
        fruitbranchsizelf(force=force)
    elif '-sd' in switches:
        return stage_duration()()
    else:
        if fruitmodel:
            for p in processes:
                p.fruitmodel=True
                p.fruitbranchsizetest()
        if '-c' in switches:
            for p in processes:
                p.allrestrictions()
        elif '-cu' in switches:
            for p in processes:
                p.alluniquefactors()


        treename = None
        for switch in switches:
            if switch.startswith('-t'):
                treename = switch[2:]
                break

        if treename:
            for p in processes:
                p.targettree(treename)

        saving = '-s' in switches
        for i,p in enumerate(processes):
            print p.title
            p(force=force or p.isUptodate(), saving=saving)

        if treename:
            print 'Available trees are',get_treenames()



if __name__ == '__main__':
    main()
    #test_production()
    pass
