from os.path import join, abspath, dirname
import os
from datetime import *
import os.path

RScriptRepo = dirname(abspath(__file__))

EXTERNALPROCESS = True

def execute_r_script(**params):
    #for var, value in params.items():
    #    launcher.write(var+" <- "+ repr(value)+'\n')
    script = '''
out = file("fruitmodel.log",open="wt")
sink(file = out, split = FALSE)

source("model_fruit_final.r")
fruitmodel({})

sink()
close(out)
'''.format(','.join([var+" = "+ repr(value).replace("'",'"') for var, value in params.items()]))
    #print script
    if EXTERNALPROCESS:
      launch_r(script)
    else:
      launch_rpy(script)

def launch_r(script):
    cwd = os.getcwd()
    os.chdir(RScriptRepo)
    
    launchfile = 'myscript2.R'
    launcher = file(launchfile,'w')
    launcher.write(script)
    launcher.close()
    
    R_HOME = os.environ["R_USER"]
    exe = os.path.join(R_HOME,'bin','Rscript.exe')
    assert os.path.exists(exe)
    #print os.getcwd()
    command = '"'+exe +'" '+launchfile+''
    #print command
    os.system(command)
    os.chdir(cwd)

def launch_rpy(script):
  import rpy2.robjects as r
  return r.r(script)

def get_fruitmodel_function():
    def fruitmodel(**params):
        execute_r_script(**params)
    return fruitmodel


from vplants.mangosim.tools import *

def applymodel(mtg, cycle, fruit_distance = 4, dump = True):
    from pandas import read_csv
    print 'apply fruit model'
    print " * Load R function"
    fruitmodel = get_fruitmodel_function()

    print " * Compute fruiting structures"
    import fruitingstructure as fs; reload(fs)
    fruiting_structures = fs.determine_fruiting_structure(mtg, cycle, fruit_distance = fruit_distance)

    print " * Compute property of the structures"
    params = mtg.property('p')
    somme_nb_fruits = 0
    somme_masse_fruit = 0
    somme_sucres_solubles = 0
    somme_acides_organiques = 0

    if dump:
        outdir = 'fruitmodel-output-cycle-'+str(cycle)+'-fdist-'+str(fruit_distance)
        if not os.path.exists(outdir) : os.makedirs(outdir)
        dump_obj(mtg, 'fruitingtree.pkl', outdir) 
        dump_obj(fruiting_structures, 'fruitingbranches.pkl', outdir)

    fruit_structures = []
    for inflos, gus in fruiting_structures:
        bloom_dates = [params[inflo].bloom_date for inflo in inflos]
        leaf_nbs    = sum([len(params[gu].final_length_leaves) for gu in gus])
        nb_fruits   = sum([params[inflo].nb_fruits for inflo in inflos])
        print nb_fruits
        fruit_structures.append((len(inflos),nb_fruits, inflos, [params[inflo].nb_fruits for inflo in inflos] ))
        somme_nb_fruits += nb_fruits
        bloom_date  = bloom_dates[0] 
        bloom_date_date = bloom_date 
        bloom_date  = str(bloom_date.day)+'/'+str(bloom_date.month)+'/'+str(bloom_date.year)
        # call fruit model in r 
        tempfile = os.path.join(RScriptRepo,"resultats.csv")
        if os.path.exists(tempfile): os.remove(tempfile)
        result = fruitmodel(bloom_date=bloom_date, nb_fruits=nb_fruits, leaf_nbs=leaf_nbs)
        
        def wait_for_file(fname, timeout = 5):
          import time
          t = time.time()
          while abs(t - time.time()) < timeout and not os.path.exists(fname) : pass
          return os.path.exists(fname)
         
        assert wait_for_file(tempfile)
        date_parser = lambda d : datetime.strptime(d, '%Y-%m-%d')
        result = read_csv(os.path.join(RScriptRepo,"resultats.csv"), parse_dates=['Date'], date_parser=date_parser)
        if dump:
          import shutil
          shutil.copy(tempfile,os.path.join(outdir, 'meanfruit-'+'-'.join(map(str,inflos)))+'.csv')
        
        dates = result["Date"]
        dates = map(lambda d:d.to_datetime(),dates)
        newyear = bloom_date_date.year
        #dates = [datetime(newyear, d.month, d.day) for d in dates]
        dates = [date(d.year+1, d.month, d.day) for d in dates]
        property = zip(result["Masse_Fruit"], result["sucres_solubles"],  result["acides_organiques"])
        fruit_growth = dict(zip(dates,property))
        #print type(fruit_growth.keys()[0])
        #assert type(fruit_growth.keys()[0])==datetime
        
        for inflo in inflos:
            params[inflo].fruit_appearance_date = min(dates)
            params[inflo].fruit_maturity_date   = max(dates)
            params[inflo].fruit_weight_min      = min(result["Masse_Fruit"])
            params[inflo].sucres_solubles      = max(result["sucres_solubles"])
            params[inflo].acides_organiques    = max(result["acides_organiques"])
            params[inflo].fruit_growth          = fruit_growth
             
    if dump:
        fstream = open(os.path.join(outdir,'fruitstructure.csv'),'w')
        fstream.write('NbInflos\tNbFruits\tFilename\tIdsInflos_NbFruitsPerInflos\n')
        for nbinflos, nbfruits, inflos, nbfruitsperinflo in fruit_structures:
            fstream.write(str(nbinflos)+'\t'+str(nbfruits)+'\t'+'meanfruit-'+'-'.join(map(str,inflos))+'\t'+'\t'.join(map(str,inflos))+'\t'+'\t'.join(map(str,nbfruitsperinflo))+'\n' )
        fstream.close()

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
        mtg = load_obj('fruitstructure.pkl','../shoot_growth')
        sc = Scene('../shoot_growth/fruitstructure.bgeom')
        fs = applymodel(mtg, 3, int(sys.argv[1]) if len(sys.argv) > 1 else 3) 
        #print len(fs)
        #for inflos, gus in fs:
        #    print inflos, list(gus)    
        #Viewer.display(color_structures(fs, mtg, sc))