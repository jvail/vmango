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


from vplants.mangosim.utils.util_parallel import *

class Evaluator:
    def __init__(self, name, func, reducefuncs, reference = True, verbose = True, fruitmodel = False):
        self.func = func
        self.reducefuncs = reducefuncs
        if type(reducefuncs) != list : self.reducefuncs = [reducefuncs]
        self.name = name 
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
            return  [{'GLM_TYPE' : eInteractionGlm, 'GLM_RESTRICTION' : None, 'FRUIT_MODEL' : False}]
        else:
            return self.paramtargets

    def fruitbranchsizetest(self, sizerange = None):
        if sizerange is None:
            path = get_option_glm_mtg_repository(fruitmodel = True, fruitbranchsize = 1)
            fbs_prefix = 'fruitbranchsize'
            while not os.path.basename(path).startswith(fbs_prefix):
                path = os.path.dirname(path)
            path = os.path.dirname(path)
            import glob
            pathes = glob.glob(os.path.join(path,fbs_prefix+'*'))
            sizerange = [int(os.path.basename(lpath)[len(fbs_prefix):]) for lpath in pathes]
            print 'Fruit Branch Size Range :', sizerange

        self.addtarget(FRUIT_MODEL = True, FRUITBRANCHSIZE = sizerange)
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
                plt.show()

        return self

    def cachebasename(self):
        return 'cache_'+self.name+'_'+self.target_tree+'.pkl'

    def cache_files(self):
        cachebasename = self.cachebasename()
        return [join(inputdir, cachebasename) for iglm, irestriction, inputdir in self.get_param_targets()]

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
    print sum(allvalues[0]),allvalues[0]
    print sum(allvalues[1]),allvalues[1]
    print ks_2samp(allvalues[0], allvalues[1])

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
    return sqrt(sum([pow(v1-v2,2) for v1,v2 in zip(hist1,hist2)])/len(hist1))

def normalized_rmsd(hist1, hist2):
    assert len(hist1) == len(hist2)
    hist1 = normalize_histo(hist1)
    hist2 = normalize_histo(hist2)
    return rmsd(hist1, hist2) # / (max(max(hist1),max(hist2)) - min(min(hist1),min(hist2)))


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


class organ_count_distribution(Evaluator):
    def __init__(self, name, func, reducefuncs):
        Evaluator.__init__(self, name, func, reducefuncs)
        self.begcycle, self.maxcycle = 4,6

    def plot_distributioni(self, refvalues, kvalues, labels, proprange = None, legends = None):
        import numpy as np
        labels = flattenarray(labels)
        kvalues = flattenarrays(kvalues)
        refvalues = flattenarray(refvalues)
        if not proprange is None:
            for pr in proprange:
                if pr > len(refvalues) : raise ValueError(pr, len(refvalues))
            refvalues = selectsubarray(refvalues, proprange)
            kvalues = selectsubarrays(kvalues, proprange)
        if len(kvalues) == 1:
            fig, ax = plot_histo(labels, kvalues[0][1], self.maketitle('Characteritics'), refvalues, legendtag = kvalues[0][0], linestyle='o', titlelocation = 2)
        else:
            fig, ax = plot_histos(labels, [v for k,v in kvalues], self.maketitle('Characteritics Comparison'), reference=refvalues, legendtags = [k for k,v in kvalues], legends= legends, titlelocation = 2)
            
        #fig.subplots_adjust(bottom=0.30, top = 0.94)

class terminal_count_distribution(organ_count_distribution):
    def __init__(self):
        organ_count_distribution.__init__(self,'terminals',self.determine_distribution, self.plot_distribution1)

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
    

class production(organ_count_distribution):
    def __init__(self, plotrestriction = [4,5,6]): #[2,3,4,5,6]): #None):
        organ_count_distribution.__init__(self,'production',self.determine_distribution, self.plot_distribution2)
        self.plotrestriction = plotrestriction

    def determine_distribution(self, mtg, mtgtype):
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        nbinflos = [sum([mm.nb_of_inflorescences(mtg,inflo) for inflo in inflocycle]) for inflocycle in inflos]
        if mtgtype == eMeasuredMtg:
            nbinflos[0] = len(inflos[0])
        nbfruits = [sum([mm.get_nb_fruits(mtg,inflo) for inflo in inflocycle]) for inflocycle in inflos]
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=3)]+inflos
        production = [sum([mm.get_fruits_weight(mtg,inflo,0) for inflo in inflocycle if not mm.get_fruits_weight(mtg,inflo,0) is None])/100. for inflocycle in inflos]
        return nbinflos+nbfruits+production

    def plot_distribution2(self, refvalues, kvalues):
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels = ['Nb. Inflo. '+str(i) for i in xrange(begcycle, maxcycle)]
        labels += ['Nb. Fruits '+str(i) for i in xrange(begcycle, maxcycle)]
        labels += ['Production '+str(i)+' (100g)' for i in xrange(3, maxcycle)]
        if self.plotrestriction:
            refvalues = selectsubarray(refvalues, self.plotrestriction)
            kvalues = selectsubarrays(kvalues, self.plotrestriction)
            labels = selectsubarray(labels, self.plotrestriction)
            if 5 in self.plotrestriction or 6 in self.plotrestriction or 4 in self.plotrestriction:
                idx = [self.plotrestriction.index(i) for i in range(4,7) if i in self.plotrestriction]
                refvalues = [(r/10. if i in idx else r) for i,r in enumerate(refvalues) ]
                kvalues = [(p,[[(v/10. if i in idx else v) for i,v in enumerate(values)] for values in ivalues]) for p,ivalues in kvalues]
                labels = [l.replace('(100g)','(kg)') for l in labels]
        
        if len(kvalues) > 1:
            histos = histodistance_legend(meanarrays(kvalues)) #, refvalues)
            legends = histos
            #histos = [histodistance_legend(meanarrays(selectsubarrays(kvalues,(2*i,2*i+1))), selectsubarray(refvalues,(2*i,2*i+1))) for i in xrange(len(refvalues)/2)]
            #legends = [' '.join([histos[i][j] for i in xrange(len(histos))]) for j in xrange(len(histos[0]))]
        else : legends = None

        self.plot_distributioni(refvalues, kvalues, labels, legends=legends)
        #plt.xlabel('Number of Growth Units per Branch')
        plt.ylabel('Number of Fruits / Production')
    



def histogram(values):
    #from numpy import histogram
    #return list(histogram(values)[0])
    from collections import Counter
    c = Counter(values)
    k = c.keys()
    return [c.get(ki,0) for ki in xrange(min(k), max(k)+1)]

def plot_histogram(refvalues, kvalues, xlabels, title, titlelocation = 2, xlabelrotation = 0):
    assert len(xlabels) == len(kvalues[0][1][0]) == len(refvalues)
    if len(kvalues) == 1:
        fig, ax = plot_histo(xlabels, allvalues=kvalues[0][1], _title=title, reference=refvalues, legendtag = kvalues[0][0], titlelocation = titlelocation, xlabelrotation = xlabelrotation)
    else:
        meankvalues = meanarrays(kvalues)
        legends = histodistance_legend(meankvalues)
        fig, ax = plot_histos_means(xlabels, [v for k,v in meankvalues], title+' Comparison', reference=refvalues, legends = legends, legendtags = [k for k,v in meankvalues], titlelocation = titlelocation, xlabelrotation = xlabelrotation)
    return fig, ax

class tree_branch_length(Evaluator):
    def __init__(self):
        Evaluator.__init__(self,'branch_length',self.determine_histogram, self.plot)

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
        fig, ax = plot_histogram(refvalues, kvalues, range(1,maxl+1), self.maketitle('Branch Length'), titlelocation = 1)
        plt.xlabel('Number of Growth Units per Branch')
        plt.ylabel('Number of Branches')


class current_year_axe_length(Evaluator):
    def __init__(self):
        Evaluator.__init__(self,'axe_length',self.determine_histogram, self.plot)

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


class bloom_date_distribution(Evaluator):

    def __init__(self):
        Evaluator.__init__(self,'bloom_dates',self.date_week_distribution, self.plot)
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
        plot_histogram(refvalues, kvalues, dates, self.maketitle('Bloom Dates'), titlelocation = 2)
        plt.xlabel('Week')
        plt.ylabel('Number of Inflorescences')

class harvest_date_distribution(Evaluator):

    def __init__(self):
        Evaluator.__init__(self,'harvest_dates',self.date_week_distribution, self.plot)
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




from vplants.mangosim.util_date import Month
class burst_date_distribution(Evaluator):
    def __init__(self):
        Evaluator.__init__(self, 'burst_dates',self.determine_distribution, self.plot)
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
        plot_histogram(refvalues, kvalues, dates, self.maketitle('Burst Dates'), titlelocation = 2, xlabelrotation = 80)
        plt.xlabel('Month')
        plt.ylabel('Number of new growth units')

class leaf_fruit_ratio(organ_count_distribution):
    def __init__(self):
        organ_count_distribution.__init__(self,'leaf_fruit_ratio',self.determine_distribution, self.plot)

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
        labels = ['Leaf Fruit Ratio '+str(i) for i in xrange(begcycle, maxcycle)]
        self.plot_distributioni(refvalues, kvalues, labels)




def get_treenames():
    return get_treenames_of_variety(mm.get_mtg(),'cogshall',eLoaded)

from collections import OrderedDict        
cmdflags = OrderedDict([('burst' , burst_date_distribution), 
                        ('bloom' , bloom_date_distribution),
                        ('harvest' , harvest_date_distribution),                        
                        ('branch' , tree_branch_length),
                        ('axe' , current_year_axe_length),
                        ('terminal' , terminal_count_distribution),
                        ('production' , production),
                        ('lf', leaf_fruit_ratio)
                        ])
cmdlabels = {
             'burst'    : 'Burst Date Distribution', 
             'bloom'    : 'Bloom Date Distributon', 
             'harvest'  : 'Harvest Date Distributon', 
             'branch'   : 'Axial Branches Length Distribution', 
             'axe'      : 'Current Year Axe Length Distribution',
             'terminal' : 'Terminal GU Characteristics', 
             'production' : 'Production',
             'lf' : 'Leaf Fruit Ratio'
             }

def saveall(fruitmodel=False, comparison = True, force=False):
    for tree in ['all']+get_treenames():
        for restriction in [None, 'all']:
            for cmd in cmdflags.values():
                p = cmd()
                p.fruitmodel = fruitmodel
                if restriction == 'all':
                    p.allrestrictions()
                if tree != 'all':
                    p.targettree(tree)
                p(force=force, saving=True)

def make_report(force = False, comparison = True, fruitmodel = False):
    from vplants.mangosim.utils.util_report import TexReportGenerator
    rep = 'texreport'
    if not os.path.exists(rep): os.makedirs(rep)
    tx = TexReportGenerator(os.path.join(rep,'validationreport.tex'),'Validation Report')

    restrictions = [None]
    if comparison: restrictions.append('all')

    for cmdname, cmd in cmdflags.items():
        tx.add_section(cmdlabels[cmdname])
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

def fruitbranchsizetest():
    p = production([5,6])
    p.fruitbranchsizetest()
    p.addtarget(FRUIT_MODEL=False)
    p()

def fruitbranchsizelf(force = False):
    p = leaf_fruit_ratio()
    p.fruitbranchsizetest()
    #p.addtarget(FRUIT_MODEL=False)
    p(nb = 10,force=force,parallel=False)

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
        fruitbranchsizetest()
    elif '-flt' in switches:
        fruitbranchsizelf(force=force)
    else:
        if fruitmodel:
            for p in processes:
                p.fruitmodel=True
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
            print cmdlabels[cmds[i]]
            p(force=force or p.isUptodate(), saving=saving)

        if treename:
            print 'Available trees are',get_treenames()



if __name__ == '__main__':
    main()
    #test_production()
    pass
