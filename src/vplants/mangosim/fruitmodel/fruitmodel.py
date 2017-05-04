from os.path import join, abspath, dirname
import os
from datetime import *
import os.path

RScriptRepo = dirname(abspath(__file__))
RWorkRepo = os.path.join(RScriptRepo,'tmp')

EXTERNALPROCESS = True

def execute_r_script(idsimu, **params):
    #for var, value in params.items():
    #    launcher.write(var+" <- "+ repr(value)+'\n')
    script = '''
localdir <<- "{}"

idsimu <<- {}
set.seed(idsimu)

#out = file("{}/fruitmodel-{}.log",open="wt")
#sink(file = out, split = FALSE)

source("{}/fruit_model_main.r")
res = fruitmodel({})

write.csv(res, file=paste(localdir,"/tmp/resultats-",idsimu,".csv",sep=''))

#sink()
#close(out)
'''.format(RScriptRepo, idsimu, RWorkRepo,idsimu,RScriptRepo,' , '.join([var+" = "+ repr(value).replace("'",'"') for var, value in params.items()]))
    #print script
    if not os.path.exists(RWorkRepo):
        os.makedirs(RWorkRepo)
    if EXTERNALPROCESS:
      launch_r(idsimu, script)
    else:
      launch_rpy(idsimu, script)

def get_R_cmd():
    import sys
    if sys == 'win32':
        R_HOME = os.environ["R_USER"]
        exe = os.path.join(R_HOME,'bin','Rscript.exe')
        assert os.path.exists(exe)
    else:
        exe = 'Rscript'
    return exe

def launch_r(idsimu, script):
    cwd = os.getcwd()
    os.chdir(RWorkRepo)
    
    launchfile = 'modellauncher-'+str(idsimu)+'.r'
    launcher = file(launchfile,'w')
    launcher.write(script)
    launcher.close()
    
    exe = get_R_cmd()
    command = '"'+exe +'" '+launchfile+''
    os.system(command)
    os.remove(launchfile)
    #os.remove("fruitmodel-{}.log".format(idsimu))
    os.chdir(cwd)

def launch_rpy(idsimu, script):
  import rpy2.robjects as r
  return r.r(script)

def get_fruitmodel_function():
    def fruitmodel(idsimu, **params):
        execute_r_script(idsimu, **params)
    return fruitmodel


from vplants.mangosim.tools import *

def applymodel(mtg, cycle, fruit_distance = 4, dump = True, dumptag = None):
    from pandas import read_csv
    from random import randint
    verbose = False
    if verbose :
        print '*** Apply fruit model ***'
        print " * Load R function"
    fruitmodel = get_fruitmodel_function()

    if verbose :
        print " * Compute fruiting structures"
    import fruitingstructure as fs; reload(fs)
    fruiting_structures = fs.determine_fruiting_structure(mtg, cycle, fruit_distance = fruit_distance)


    if verbose :    
        print " * Compute property of the structures"
    params = mtg.property('p')
    somme_nb_fruits = 0
    somme_masse_fruit = 0
    somme_sucres_solubles = 0
    somme_acides_organiques = 0

    if dump:
        if dumptag :
          outdir = 'fruitmodeloutput/fruitmodel-'+dumptag+'-cycle-'+str(cycle)+'-fdist-'+str(fruit_distance)
        else:
          outdir = 'fruitmodeloutput/fruitmodel-output-cycle-'+str(cycle)+'-fdist-'+str(fruit_distance)
        if os.path.exists(outdir) : 
            import shutil
            shutil.rmtree(outdir)
        os.makedirs(outdir)
        dump_obj(mtg, 'fruitingtree.pkl', outdir) 
        dump_obj(fruiting_structures, 'fruitingbranches.pkl', outdir)

    fruit_structures = []
    for inflos, gus in fruiting_structures:
        bloom_dates = [params[inflo].fullbloom_date for inflo in inflos]
        if len(gus) == 0 and len(mtg.vertices(scale=mtg.max_scale())) == 1:
            leaf_nbs    = 100
        else:
            try:
                leaf_nbs    = sum([len(params[gu].final_length_leaves) for gu in gus if not gu is None])
            except AttributeError, ae:
                for gu in gus :
                    if not gu is None:
                        if not params[gu].hasattr('final_length_leaves'):
                            print gu, params[gu]
                            params[gu].final_length_leaves
        nb_fruits   = sum([params[inflo].nb_fruits for inflo in inflos])
        #print nb_fruits
        somme_nb_fruits += nb_fruits
        bloom_date  = bloom_dates[0] 
        bloom_date_date = bloom_date 
        cycledecal = bloom_date.year - 2002
        bloom_date  = str(bloom_date.day)+'/'+str(bloom_date.month)+'/2002'
        # call fruit model in r 
        import sys
        idsimu = randint(0,100000)
        idsimu += fruit_distance*100000
        #print 'Do simu', inflos
        tempfile = os.path.join(RWorkRepo,"resultats-"+str(idsimu)+".csv")
        if os.path.exists(tempfile): os.remove(tempfile)
        result = fruitmodel(bloom_date=bloom_date, nb_fruits=nb_fruits, leaf_nbs=leaf_nbs, idsimu=idsimu)
        
        def wait_for_file(fname, timeout = 0.1):
          import time
          t = time.time()
          while abs(t - time.time()) < timeout and not os.path.exists(fname) : pass
          return os.path.exists(fname)
         
        if not wait_for_file(tempfile):
            failedfile = os.path.join(RWorkRepo,"failed-"+str(idsimu)+".csv")
            if True : #os.path.exists(failedfile):
                if os.path.exists(failedfile): os.remove(failedfile)
                for inflos, gus in fruiting_structures:
                    for inflo in inflos:
                        params[inflo].nb_fruits = 0
                        params[inflo].fruits_weight = 0
                        params[inflo].idsimu        = idsimu
                        params[inflo].leaffruit_ratio = (leaf_nbs, nb_fruits)

                continue

        date_parser = lambda d : datetime.strptime(d, '%Y-%m-%d')
        result = read_csv(tempfile, parse_dates=['Date'], date_parser=date_parser)
        if dump:
            import shutil
            shutil.copy(tempfile,os.path.join(outdir, 'meanfruit-'+'-'.join(map(str,inflos)))+'.csv')
        os.remove(tempfile)
        
        dates = result["Date"]
        dates = map(lambda d:d.to_pydatetime(),dates)
        newyear = bloom_date_date.year
        dates = [date(d.year+cycledecal, d.month, d.day) for d in dates]
        property = zip(result["Masse_Fruit"], result["sucres_solubles"],  result["acides_organiques"])
        fruit_growth = dict(zip(dates,property))
        fruit_structures.append((len(inflos), leaf_nbs,  nb_fruits, max(result["Masse_Fruit"]), inflos, [params[inflo].nb_fruits for inflo in inflos] ))
        
        for inflo in inflos:
            params[inflo].fruits_growth_stage_date = min(dates)
            params[inflo].fruits_maturity_date     = max(dates)
            params[inflo].fruits_initial_weight    = min(result["Masse_Fruit"])
            params[inflo].fruits_weight            = max(result["Masse_Fruit"])
            params[inflo].sucres_solubles          = max(result["sucres_solubles"])
            params[inflo].acides_organiques        = max(result["acides_organiques"])
            params[inflo].fruits_growth            = fruit_growth
            params[inflo].idsimu                   = idsimu
            params[inflo].leaffruit_ratio          = (leaf_nbs, nb_fruits)
             
    if dump:
        fstream = open(os.path.join(outdir,'fruitstructure.csv'),'w')
        maxbranch = max([len(inflos) for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures])
        nbtotinflos = sum([nbinflos for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures])
        nbtotleaf = sum([nbleaf for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures])
        nbtotfruits = sum([nbfruits for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures])
        masstotfruits = sum([massfruit*nbfruits for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures])
        
        fstream.write('Filename\tNbInflos\tNbLeaf\tNbFruits\tMeanMassFruit\tTotalMassFruit')
        fstream.write(''.join(['\tIdsInflos_'+str(i) for i in xrange(maxbranch)]) )
        fstream.write(''.join(['\tNbFruitsPerInflos_'+str(i) for i in xrange(maxbranch)]))
        fstream.write('\n')
        for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures:
            fstream.write('meanfruit-'+'-'.join(map(str,inflos))+'\t'+str(nbinflos)+'\t'+str(nbleaf)+'\t'+str(nbfruits)+'\t'+str(massfruit)+'\t'+str(massfruit*nbfruits)+'\t'+'\t'.join(map(str,inflos))+'\t'*(1+maxbranch-len(inflos))+'\t'.join(map(str,nbfruitsperinflo))+'\t'*(maxbranch-len(inflos))+'\n' )
        
        fstream.write('TOTAL\t'+str(nbtotinflos)+'\t'+str(nbtotleaf)+'\t'+str(nbtotfruits)+'\t'+str(masstotfruits/nbtotfruits)+'\t'+str(masstotfruits)+'\n')
        fstream.close()
    
    if dump:
      return fruiting_structures, outdir
    else:
      return fruiting_structures

def infloset_positions(inflos, mtg, scene):
    from openalea.plantgl.all import BoundingBox, Vector3
    pos = {}
    idmap  = mtg.property('_axial_id')
    for inflo in inflos:
            obj = scene.find(idmap[inflo])
            c = BoundingBox(obj).getCenter()
            pos[inflo] = c
    return pos

def inflo_positions(fruiting_structures, mtg, scene):
    pos = {}
    for inflos, gus in fruiting_structures:
        pos.update(infloset_positions(inflos, mtg, scene))
    return pos

def central_inflo(inflos, mtg, scene):
    from openalea.plantgl.all import Point3Array
    pos = infloset_positions(inflos, mtg, scene)
    center = Point3Array(pos.values()).getCenter()
    a = pos.keys()
    a.sort(cmp = lambda a,b : cmp(pos[a]-center,pos[b]-center))
    return a[0]


def c_determine_colorindex(fruiting_structures, mtg, scene):
    from openalea.plantgl.all import norm
    import numpy
    from random import randint
    allinflos = sum([inflos for inflos, gus in fruiting_structures],[])
    inflopos = inflo_positions(fruiting_structures, mtg, scene)
    idmap  = mtg.property('_axial_id')
    pos = [inflopos[i] for i in inflopos]
    distances = numpy.array([[norm(p1-p2) for p1 in pos] for p2 in pos])
    #print distances
    maxdist = numpy.amax(distances)
    mindist = numpy.amin(distances)
    distances -= mindist
    distances /= (maxdist- mindist)
    nbcolors = len(pos)
    index = range(nbcolors)
    print distances
    def cost(index, distances):
        return sum([sum([distances[i,j]*abs(vi-vj) for j,vj in enumerate(index)]) for i,vi in enumerate(index)])

    def swap(index, i = None, j = None):
        assert i != j or i == None
        nindex = list(index)
        i1, i2 = randint(0,nbcolors-1) if i is None else i,randint(0,nbcolors-1) if j is None else j
        if i1 == i2: return swap(index,i,j)
        nindex[i1], nindex[i2] = index[i2], index[i1]
        return nindex

    def bestswap(index, distances):
        bcost = cost(index, distances)
        bindex = index
        for i in xrange(len(index)-2):
            for j in xrange(i+1,len(index)-1):
                nindex = swap(index,i,j)
                ccost = cost(nindex, distances)
                if ccost < bcost:
                    bcost = ccost
                    bindex = nindex
        return bindex

    def bestswapi(index, distances, i):
        bcost = cost(index, distances)
        bindex = index
        for j in xrange(0,len(index)-1):
            if j != i:
                nindex = swap(index,i,j)
                ccost = cost(nindex, distances)
                if ccost < bcost:
                    bcost = ccost
                    bindex = nindex
        return bindex

    def shuffle(index):
        from numpy.random import shuffle as nshuffle
        nindex = list(index)
        nshuffle(nindex)
        return nindex


    bcost = cost(index, distances)
    print bcost
    bindex = index
    bestiter = None
    for i in xrange(10):
        success = True
        index = shuffle(index)
        icost = cost(index, distances)
        while success:
            nindex = bestswapi(index, distances, randint(0,nbcolors-1))
            cicost = cost(nindex, distances)
            if cicost < icost:
                index = nindex
                icost = cicost
                print icost
            else:
                success = False
        if icost < bcost:
            bcost = icost
            bindex = index
            print 'bestiter',i
            bestiter = i

    print bcost
    result = {}
    i = 0
    for inflos, gus in fruiting_structures:
        for inflo in inflos:
            result[inflo] = index[i]
            i+=1
        nindex.append(result[inflos[0]])
    print bestiter
    #print result
    return result


def determine_colorindex(fruiting_structures, mtg, scene):
    from vplants.mangosim.tools import load_obj, dump_obj
    cache = 'cache_colorinflo.pkl'
    result = None
    allinflos1 = set([vid for vid,lid in mtg.property('_axial_id').items() if mtg.label(vid) == 'Inflorescence' and mtg.property('nb_fruits')[vid] > 0])
    allinflos = set(sum([inflos for inflos, gus in fruiting_structures],[]))
    if os.path.exists(cache):
        result = load_obj(cache)
        if len(allinflos.symmetric_difference(result)) > 0:
            #print list(sorted(allinflos))
            #print list(sorted(result.keys()))
            #print allinflos.symmetric_difference(result)
            result = None
    if result is None:
        result = c_determine_colorindex(fruiting_structures, mtg, scene)
        assert len(allinflos.symmetric_difference(result)) == 0
        dump_obj(result, cache)

    i = 0
    nindex = []
    for inflos, gus in fruiting_structures:
        nindex.append(result[central_inflo(inflos,mtg, scene)])
    return nindex

def color_structures(fruiting_structures, mtg, scene):
    import matplotlib.pyplot as plt
    from openalea.plantgl.all import Material, Shape
    from random import randint, seed
    from numpy.random import seed as nseed
    seed(0) ; nseed(0)
    nbcolors = len(sum([inflos for inflos, gus in fruiting_structures],[])) #len(fruiting_structures)
    _colors = plt.get_cmap('jet',nbcolors)
    colors = lambda x: _colors( x )

    structures = dict()
    idmap  = mtg.property('_axial_id')

    print 'determine colors'
    colindex = determine_colorindex(fruiting_structures, mtg, scene)
    print colindex
    allinflos = [lid for vid,lid in idmap.items() if mtg.label(vid) == 'Inflorescence']
    for inflos, gus in fruiting_structures:
        i = colindex.pop(0)
        col = colors(i)
        mat = Material([int(c*100) for c in col[:3]],2)
        for j in inflos:
            structures[idmap[j]] = mat
        for j in gus:
            structures[idmap[j]] = mat
        print col, inflos

    definfmat = Material((50,50,50))
    for inf in allinflos:
        if not inf in structures:
            structures[inf] = definfmat

    defmat = Material((0,0,0))
    print 'compute colored scene'
    nscene = Scene([Shape(sh.geometry,  structures.get(sh.id,defmat), sh.id, sh.parentId) for sh in scene ])
    return nscene
    Viewer.display(nsc)

def leaf_fruit_ratio(mtg, fruiting_structures):
    res = []
    params = mtg.property('p')
    for inflos, gus in fruiting_structures:
        nb_leaves    = sum([len(params[gu].final_length_leaves) for gu in gus if not gu is None])
        nb_fruits   = sum([params[inflo].nb_fruits for inflo in inflos])
        res.append((nb_leaves, nb_fruits))
    return res

if __name__ == '__main__':
    import sys
    if '--R' in sys.argv:
        launch_r()
    else:
        from vplants.mangosim.tools import load_obj
        import numpy as np

        from openalea.plantgl.all import Scene, Viewer
        cycle=4
        lowres = True
        mtg = load_obj('structure-cycle'+str(cycle)+('b' if lowres else '')+'.pkl')
        sc = Scene('structure-cycle'+str(cycle)+('b' if lowres else '')+'.bgeom')
        #Viewer.display(sc)

        import fruitingstructure as fsm
        distance = int(sys.argv[1]) if len(sys.argv) > 1 else 4
        fs = fsm.determine_fruiting_structure(mtg, cycle=cycle, fruit_distance = distance)
        print 'Nb of groups',len(fs)
        lfr = leaf_fruit_ratio(mtg, fs)
        print [l/float(f) for l,f in lfr]
        print np.mean([l/float(f) for l,f in lfr])

        #fs = applymodel(mtg, 3, int(sys.argv[1]) if len(sys.argv) > 1 else 3) 
        colorrepresentation = False
        if colorrepresentation:
            print 'Start coloring'
            nsc = color_structures(fs, mtg, sc)
            print 'End coloring'
            nsc.save('structure-cycle'+str(cycle)+'-dist'+str(distance)+'.bgeom')
            #Viewer.display(nsc)
            nsc.save('structure-cycle'+str(cycle)+'-dist'+str(distance)+'-core.pov')
