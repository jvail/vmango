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


from multiprocessing import Process, Pipe
import itertools

def spawn(f):
    def fun(pipe,x):
        pipe.send(f(x))
        pipe.close()
    return fun

def parmap(f,X):
    pipe=[Pipe() for x in X]
    proc=[Process(target=spawn(f),args=(c,x)) for x,(p,c) in itertools.izip(X,pipe)]
    [p.start() for p in proc]
    [p.join() for p in proc]
    return [p.recv() for (p,c) in pipe]

def nparmap(f,X,n = 5):
    from math import ceil
    res = []
    nbdata = len(X)
    nbsteps =  int(ceil(float(nbdata) / n))
    print X
    for j in xrange(nbsteps):
        res += parmap(f,X[j*n:min(nbdata,(j+1)*n)])
    return res


class Evaluator:
    def __init__(self, name, func, reducefunc, reference = True, verbose = True):
        self.func = func
        self.reducefunc = reducefunc
        self.name = name 
        self.inputdirs = []
        self.reference = reference
        self.verbose = verbose
        self.funcargs = []
        self.funckwds = {}
        self.reduseargs = []
        self.reducekwds = {}
        self.glmtargets = []
        self.target_tree = 'all'

    def target(self, glm = eInteractionGlm, restriction = None):
        if type(glm) != list: glm = [glm]
        if type(restriction) != list: restriction = [restriction]
        glmtargets = list(itertools.product(glm, restriction))
        for iglm, irestriction in glmtargets:
            params = {'GLM_TYPE' : iglm, 'GLM_RESTRICTION' : irestriction}
            self.inputdirs.append((iglm, irestriction, get_glm_mtg_repository( params = params)))
        self.glmtargets += glmtargets
        return self

    def targettree(self, treename):
        self.target_tree = treename

    def allrestrictions(self, glm = eInteractionGlm):
        self.target(restriction=[None, eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction])
        return self

    def configure(self, *args, **kwds):
        self.funcargs = args
        self.funckwds = kwds
        return self

    def reduceconfigure(self, *args, **kwds):
        self.reduseargs = args
        self.reducekwds = kwds
        return self

    def apply(self, nb = None, force = False, parallel = True, saving = False):
        cachebasename = 'cache_'+self.name+'_'+self.target_tree+'.pkl'
        import time
        if len(self.inputdirs) == 0: self.target()

        valuesset = {}
        if self.reference:
            refvalues = self._applyto(eMeasuredMtg)
        else:
            refvalues = None
        for iglm, irestriction, inputdir in self.inputdirs:
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
            valuesset[(iglm, irestriction)] = values
        self.reducefunc(refvalues, valuesset, *self.reduseargs, **self.reducekwds)
        import matplotlib.pyplot as plt
        if saving:
            filename = self.saved_filename()
            if not os.path.exists(figoutdir):
                os.makedirs(figoutdir)
            print 'Save',repr(filename)
            plt.savefig(filename,  bbox_inches='tight')
            plt.close()
        else:
            plt.show()

        return self

    def saved_filename(self):
        if len(self.inputdirs) == 0: self.target()

        fname = self.name+'_'+self.target_tree
        if len(self.glmtargets) > 0:
            fname += '_comp_'
            if len(self.glmtargets) == 6:
                fname +='all'
            else:
                fname += '_'.join([RestrictionName[r] for g,r in self.glmtargets])
        else:
            fname += '_'+RestrictionName[self.glmtargets[0][1]]
        fname += '.png'
        filename = os.path.join(figoutdir,fname)
        return filename

    def _applyto(self, mtgtype, mtgfile = None):
        setMtgStyle(mtgtype)
        if mtgtype == eSimulatedMtg : 
            if self.verbose : print 'Process', repr(os.path.basename(mtgfile))
            mtg = load_obj(mtgfile)
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
        m = [('janv','jan'),('fev','feb'),('mars','march'),('avril','april'),('mai','may'),('juin','june'),('juil','july'),('aout','aug')]
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

def histogram_distance(hist1, hist2):
    from math import log
    sh1 = float(sum(hist1))
    sh2 = float(sum(hist2))
    h1 = [v1/sh1 for v1 in hist1]
    h2 = [v2/sh2 for v2 in hist2]
    divkl_h1_h2 = sum([v1 * log(v1/v2) for v1,v2 in zip(h1,h2) if abs(v1) > 0 and abs(v2) > 0] )
    divkl_h2_h1 = sum([v1 * log(v1/v2) for v1,v2 in zip(h2,h1) if abs(v1) > 0 and abs(v2) > 0] )
    return divkl_h1_h2+divkl_h2_h1


def histogram_distances(reference, allvalues, nullmodel = None ):
    results = []
    for k,d in allvalues.items():
        results.append((k,histogram_distance(reference,d)))
    results = dict(results)
    if nullmodel:
        nullmodelval = results[nullmodel]
        results = dict([(k,v/nullmodelval)for k,v in results.items()])
    return results

def meanarray(values):
    import numpy as np
    return [np.mean([v[i] for v in values]) for i in xrange(len(values[0]))]

def meanarrays(kvalues):
   return dict([(k,meanarray(a)) for k,a in kvalues.items()])

def flattenarray(values):
   from itertools import chain
   return list(chain(*[vi if type(vi) in [tuple,list] else [vi] for vi in values] ))

def flattenarrays(kvalues):
   return dict([(k,[flattenarray(values) for values in valuesset]) for k,valuesset in kvalues.items()])

def homogenize_histo_length(refvalues, kvalues):
    maxl = max(max([max(map(len,ivalues)) for ivalues in kvalues.values()]), len(refvalues))
    for ivalues in kvalues.values():
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

def histodistance_legend(meankvalues):
    d = dict(meankvalues)
    del d[(eInteractionGlm,None)]
    histo =  histogram_distances(meankvalues[(eInteractionGlm,None)], d, (eInteractionGlm,eAllRestriction))
    histo[(eInteractionGlm,None)] = 0
    skeys = sortedkeys(meankvalues)
    return [str(histo[sk]) for sk in skeys]

class terminal_count_distribution(Evaluator):
    def __init__(self):
        Evaluator.__init__(self,'terminals',self.determine_distribution, self.plot_distribution)
        self.begcycle, self.maxcycle = 4,6

    def determine_distribution(self, mtg, mtgtype):
        terminals = [self.get_terminal_gus_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        nbterminals = map(lambda t : (len ([gu for gu in t if mtg.edge_type(gu) == '<']),len ([gu for gu in t if mtg.edge_type(gu) == '+'])),terminals)
        nbfloterminals = [(len([gu for gu in cterminals if len(inflorescence_children(mtg,gu)) > 0 and mtg.edge_type(gu) == '<']),
                           len([gu for gu in cterminals if len(inflorescence_children(mtg,gu)) > 0 and mtg.edge_type(gu) == '+'])) for cterminals in terminals]
        inflos = [self.get_all_inflos_at_cycle(mtg, cycle=c) for c in xrange(self.begcycle, self.maxcycle)]
        nbinflos = [sum([mm.nb_of_inflorescences(mtg,inflo) for inflo in inflocycle]) for inflocycle in inflos]
        if mtgtype == eMeasuredMtg:
            nbinflos[0] = len(inflos[0])
        nbfruits = [sum([mm.get_nb_fruits(mtg,inflo) for inflo in inflocycle]) for inflocycle in inflos]
        return nbterminals+nbfloterminals+nbinflos+nbfruits

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

    def plot_distribution(self, refvalues, kvalues):
        import numpy as np
        begcycle, maxcycle = self.begcycle, self.maxcycle
        labels =  [('Nb. Ter. Api. '+str(i),'Nb. Ter. Lat. '+str(i)) for i in xrange(begcycle, maxcycle)]
        labels += [('Nb. Flo. Ter. Api. '+str(i),'Nb. Flo. Ter. Lat. '+str(i)) for i in xrange(begcycle, maxcycle)]
        labels += ['Nb. Inflo. '+str(i) for i in xrange(begcycle, maxcycle)]
        labels += ['Nb. Fruits '+str(i) for i in xrange(begcycle, maxcycle)]
        labels = flattenarray(labels)
        kvalues = flattenarrays(kvalues)
        if len(kvalues) == 1:
            fig, ax = plot_histo(labels, kvalues.values()[0], self.maketitle('Characteritics'), flattenarray(refvalues), legendtag = kvalues.keys()[0], linestyle='o', titlelocation = 1)
        else:
            #meankvalues = meanarrays(kvalues)
            #fig, ax = plot_histos_means(labels, sortedvalues(meankvalues), self.maketitle('Characteritics Comparison'), reference=flattenarray(refvalues), legendtags = sortedkeys(meankvalues), linestyle='o', titlelocation = 1)
            plot_histos
            fig, ax = plot_histos(labels, sortedvalues(kvalues), self.maketitle('Characteritics Comparison'), reference=flattenarray(refvalues), legendtags = sortedkeys(kvalues), titlelocation = 2)
            
        fig.subplots_adjust(bottom=0.30, top = 0.94)


def histogram(values):
    from numpy import histogram
    return list(histogram(values)[0])

def plot_histogram(refvalues, kvalues, xlabels, title, titlelocation = 2):
    assert len(xlabels) == len(kvalues.values()[0][0]) == len(refvalues)
    if len(kvalues) == 1:
        fig, ax = plot_histo(xlabels, allvalues=kvalues.values()[0], _title=title, reference=refvalues, legendtag = kvalues.keys()[0], titlelocation = titlelocation)
    else:
        meankvalues = meanarrays(kvalues)
        legends = histodistance_legend(meankvalues)
        fig, ax = plot_histos_means(xlabels, sortedvalues(meankvalues), title+' Comparison', reference=refvalues, legends = legends, legendtags = sortedkeys(meankvalues), titlelocation = titlelocation)

class tree_branch_length(Evaluator):
    def __init__(self):
        Evaluator.__init__(self,'branch_length',self.determine_histogram, self.plot)

    def determine_histogram(self, mtg, mtgype):
        ucs =  self.get_all_gus(mtg)
        def axial_axe_length(uc):
            cuc = uc
            l = 1
            while mtg.parent(cuc) and mtg.edge_type(mtg.parent(cuc)) == '<':
                cuc = mtg.parent(cuc)
                l += 1
            return l
        last_apical_ucs = [uc for uc in ucs if len([is_apical(mtg,cuc) for cuc in vegetative_children(mtg,uc)]) == 0]
        axial_axe_lengths = map(axial_axe_length, last_apical_ucs)
        histo = histogram(axial_axe_lengths)
        return histo

    def plot(self, refvalues,kvalues):
        maxl = homogenize_histo_length(refvalues,kvalues)
        plot_histogram(refvalues, kvalues, range(1,maxl+1), self.maketitle('Branch Length'), titlelocation = 1)


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

    def plot(self, refvalues,kvalues):
        strdate = lambda d : str(d[1])+'/'+str(d[0]-2000)
        dates     = map(strdate, self.daterange)
        plot_histogram(refvalues, kvalues, dates, self.maketitle('Bloom Dates'), titlelocation = 2)



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
        plot_histogram(refvalues, kvalues, dates, self.maketitle('Burst Dates'), titlelocation = 2)


def get_treenames():
    return get_treenames_of_variety(mm.get_mtg(),'cogshall',eLoaded)

from collections import OrderedDict        
cmdflags = OrderedDict([('burst' , burst_date_distribution), 
                        ('bloom' , bloom_date_distribution),
                        ('terminal' , terminal_count_distribution),
                        ('branch' , tree_branch_length),
                        ('axe' , current_year_axe_length)])
cmdlabels = { 'burst'    : 'Burst Date Distribution', 
             'bloom'    : 'Bloom Date Distributon', 
             'terminal' : 'Organ Population Characteristics', 
             'branch'   : 'Axial Branches Length Distribution', 
             'axe'      : 'Current Year Axe Length Distribution'}

def saveall():
    for tree in ['all']+get_treenames():
        for restriction in [None, 'all']:
            for cmd in cmdflags.values():
                p = cmd()
                if restriction == 'all':
                    p.allrestrictions()
                if tree != 'all':
                    p.targettree(tree)
                p(saving=True)

def make_report(force = False):
    from vplants.mangosim.utils.util_report import TexReportGenerator
    rep = 'texreport'
    if not os.path.exists(rep): os.makedirs(rep)
    tx = TexReportGenerator(os.path.join(rep,'validationreport.tex'),'Validation Report')

    for cmdname, cmd in cmdflags.items():
        tx.add_section(cmdlabels[cmdname])
        for restriction in [None, 'all']:
            tx.add_subsection('Simulation from GLM' if restriction is None else 'Factor Sensitivity Test')
            for tree in ['all']+get_treenames():
                    tx.add_subsubsection('Tree '+tree if tree != 'all' else 'Cultivar Cogshall')
                    p = cmd()
                    if restriction == 'all':
                        p.allrestrictions()
                    if tree != 'all':
                        p.targettree(tree)
                    figname = p.saved_filename()
                    if not os.path.exists(figname) or force:
                        p.apply(saving=True)
                    tx.add_figure(os.path.join(os.pardir,figname))
                    tx.clear_page()
    tx.close()
    tx.compile()
    tx.view()

def main():
    import sys
    def help():
        print 'Available plots :',cmdflags.keys()
        print '-c : Comparison mode'
        print '-f : Force reevaluation'
        print '-s : Save figure'
        print '-sa : Save all figures'
        print '-r : Create a report'
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
    if '-h' in switches:
        help()
        return
    if '-sa' in switches:
        saveall()
    elif '-r' in switches:
        make_report(force=force)
    else:
        if '-c' in switches:
            for p in processes:
                p.allrestrictions()

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
            p(force=force, saving=saving)

        if treename:
            print 'Available trees are',get_treenames()



if __name__ == '__main__':
    main()
    pass
