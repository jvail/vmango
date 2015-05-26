from openalea.mtg import *
from openalea.tree_matching.mtgmatching import *

import vplants.mangosim.doralice_mtg.mtg_manipulation as mm
reload(mm) 
from vplants.mangosim.state import *
from vplants.mangosim.tools import *
from vplants.mangosim.util_path import share_dir, get_glm_mtg_repository, mtgfname
from vplants.mangosim.util_date import month_difference, cycle_end
import os

first_date = cycle_end(3)

def get_burst_date(u, values):
    bdate = values.get(u)
    if bdate is None or bdate < first_date: return first_date
    return bdate

class MtgNodeCost(NodeCost):
    def __init__(self, refburstdate, simburstdate, refmtg, simmtg): 
        NodeCost.__init__(self)
        self.refburstdate = refburstdate
        self.simburstdate = simburstdate
        self.refmtg = refmtg
        self.simmtg = simmtg

    def getDeletionCost(self,a) : return 1
    def getInsertionCost(self,b) : return 1

    def getChangingCost(self,a,b) : 
        #if self.refmtg.edge_type(a) != self.simmtg.edge_type(b): return 0.5
        #return 0
        val = abs(month_difference(get_burst_date(a,self.refburstdate), get_burst_date(b,self.simburstdate)))/24.
        if not (0 <= val <= 1):
            raise ValueError(val, get_burst_date(a,self.refburstdate), get_burst_date(b,self.simburstdate), first_date)

        return val

def get_trees(refmtg, simmtg):
    TreeNamePropertyName = 'treename'
    treenamelist1 = dict([(v,k) for k,v in refmtg.property(TreeNamePropertyName).items() ])
    treenamelist2 = dict([(v,k) for k,v in simmtg.property(TreeNamePropertyName).items() ])
    commontrees = set(treenamelist1.keys()) & set(treenamelist2.keys())
    return dict([(tname,(treenamelist1[tname],treenamelist2[tname])) for tname in commontrees])


def compare(refmtg, simmtg):
    BurstDatePropertyName = 'burst_date'
    cost = MtgNodeCost(refmtg.property(BurstDatePropertyName),simmtg.property(BurstDatePropertyName))
    allres = []
    for tname, roots in get_trees(refmtg, simmtg).items():
        root1, root2 = roots
        m = MtgMatching(refmtg, simmtg, scale1=refmtg.max_scale(), scale2=simmtg.max_scale(), root1=root1, root2=root2, cost=cost)
        res = m.match()
        print res
        allres.append(res)
    return sum(allres)


def retrieve_mtg_filenames(inputdir, nb = None):
    print 'Dir:',inputdir
    import glob
    mtgfiles = glob.glob(join(inputdir,mtgfname.format('*')))
    mtgfiles.sort()
    if nb: mtgfiles = mtgfiles[:nb]
    mtgs = []
    for  mtgfile in mtgfiles:
        #print 'Retreive '+repr(os.path.basename(mtgfile))
        mtgs.append(mtgfile)
    return mtgs

def retrieve_mtgs(inputdir, nb = None):
    return [load_obj(fname) for fname in retrieve_mtg_filenames(inputdir, nb)]

def get_reference_mtg():
    return load_obj(join(share_dir,'replayed_mango.bmtg'))


WITHINDELAYMETHODVALUES = [eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin]
structural_comparison_cache = 'structural_comparison.pkl'

allres = None
refmtg = None

def process(mtgfname):
    global allres
    print 'compare', mtgfname
    res = compare(refmtg, load_obj(mtgfname))
    allres[mtgfname.replace(share_dir,'')] = res

def structural_comparison(wdmv = 0, glmestimation = eNullGlm, nb = None , nbproc = 1):
    global allres
    from multiprocessing import Pool, Manager, cpu_count
    if nbproc is None : nbproc = cpu_count()
    mm.setMtgStyle(eSimulatedMtg)
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[wdmv],  'ESTIMATIONTYPE' : glmestimation}
    inputdir = get_glm_mtg_repository( params = params)
    distrib_file = join(inputdir, structural_comparison_cache)
    global refmtg
    refmtg = get_reference_mtg()
    print 'Retrieve MTGs'
    mtgfnames = retrieve_mtg_filenames(inputdir,nb)

    if os.path.exists(distrib_file):
        print 'Retreive structural comparision from',repr(str(distrib_file))
        allres = load_obj(structural_comparison_cache,inputdir)
        mtgfnames = [fname for fname in mtgfnames if not fname.replace(share_dir,'') in allres]
    else : allres = {}
    if len(mtgfnames) > 0:
        manager = Manager()
        allres = manager.dict(allres)
        nbprocesses = min(nbproc, len(mtgfnames), cpu_count())
        print 'use',nbprocesses,'processes'
        p = Pool(processes = nbprocesses)
        p.map(process, mtgfnames)
        allres = dict(allres)
        dump_obj(allres,structural_comparison_cache,inputdir)
    return allres

def comparison_map(wdmv, glmestimation = eNullGlm, nb = 0 ):
    mm.setMtgStyle(eSimulatedMtg)
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[wdmv],  'ESTIMATIONTYPE' : glmestimation}
    inputdir = get_glm_mtg_repository( params = params)
    print 'Retrieve MTGs'
    global refmtg
    refmtg = get_reference_mtg()
    mtgfname = retrieve_mtg_filenames(inputdir,nb)[nb]
    simmtg = load_obj(mtgfname)
    BurstDatePropertyName = 'burst_date'
    cost = MtgNodeCost(refmtg.property(BurstDatePropertyName),simmtg.property(BurstDatePropertyName),refmtg, simmtg)
    root1, root2 = get_trees(refmtg, simmtg)['B12']
    m = MtgMatching(refmtg, simmtg, scale1=refmtg.max_scale(), scale2=simmtg.max_scale(), root1=root1, root2=root2, cost=cost)
    res = m.match()/(float(len(m.idmap1))*2.  )
    print res
    return m


def gcomparison(wdmv = 0):
    from numpy import mean, std
    mm.setMtgStyle(eSimulatedMtg)
    nbrefnode = 1659
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[wdmv],  'ESTIMATIONTYPE' : eNullGlm}
    inputdir1 = get_glm_mtg_repository( params = params)
    params = {'WITHINDELAYMETHOD' : WITHINDELAYMETHODVALUES[wdmv],  'ESTIMATIONTYPE' : eSelectedGlm}
    inputdir2 = get_glm_mtg_repository( params = params)
    allres1 = load_obj(structural_comparison_cache,inputdir1)
    allres2 = load_obj(structural_comparison_cache,inputdir2)
    print 'NullGlm:',mean(allres1.values())/(2*nbrefnode), ', SelectedGlm:',mean(allres2.values())/(2*nbrefnode)
    print 'NullGlm:',std(allres1.values())/(2*nbrefnode), ', SelectedGlm:',std(allres2.values())/(2*nbrefnode)
    return allres1, allres2

if __name__ == '__main__':
    #structural_comparison(0,eSelectedGlm,100,None)
    #structural_comparison(0,eNullGlm,100,None)
    #allres1, allres2 = gcomparison()

    pass
