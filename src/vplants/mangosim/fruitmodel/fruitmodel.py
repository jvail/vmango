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
out = file("{}/fruitmodel-{}.log",open="wt")
sink(file = out, split = FALSE)

source("{}/fruit_model_main.r",keep.source=TRUE)
res = fruitmodel({})

sink()
close(out)
'''.format(RWorkRepo,idsimu,RScriptRepo,' , '.join([var+" = "+ repr(value).replace("'",'"') for var, value in params.items()])+' , idsimu='+str(idsimu))
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
    os.remove("fruitmodel-{}.log".format(idsimu))
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
                        params[inflo].fruits_weight
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

def color_structures(fruiting_structures, mtg, scene):
    import matplotlib.pyplot as plt
    from openalea.plantgl.all import Material, Shape
    nbcolors = len(fruiting_structures)
    _colors = plt.get_cmap('jet',nbcolors)
    colors = lambda x: _colors( x )

    structures = dict()
    idmap  = mtg.property('_axial_id')

    i = 0
    print 'determine colors'
    for inflos, gus in fruiting_structures:
        col = colors(i)
        mat = Material([int(c*200) for c in col[:3]])
        for j in inflos:
            structures[idmap[j]] = mat
        for j in gus:
            structures[idmap[j]] = mat
        i += 1

    defmat = Material((0,0,0))
    print 'compute colored scene'
    nscene = Scene([Shape(sh.geometry,  structures.get(sh.id,defmat), sh.id, sh.parentId) for sh in scene ])
    return nscene



if __name__ == '__main__':
    import sys
    if '--R' in sys.argv:
        launch_r()
    else:
        from vplants.mangosim.tools import load_obj

        from openalea.plantgl.all import Scene, Viewer
        cycle=4
        mtg = load_obj('structure-cycle'+str(cycle)+'.pkl')
        sc = Scene('structure-cycle'+str(cycle)+'.bgeom')

        import fruitingstructure as fsm
        fs = fsm.determine_fruiting_structure(mtg, cycle=cycle, fruit_distance = int(sys.argv[1]) if len(sys.argv) > 1 else 4)

        #fs = applymodel(mtg, 3, int(sys.argv[1]) if len(sys.argv) > 1 else 3) 
        print len(fs)
        for inflos, gus in fs:
            print inflos, list(gus)    
        Viewer.display(color_structures(fs, mtg, sc))