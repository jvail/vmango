from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import
from importlib import reload
from builtins import str
from builtins import zip
from builtins import map
from builtins import range
from past.utils import old_div
from os.path import join, abspath, dirname
import os
from datetime import *
import os.path

RScriptRepo = dirname(abspath(__file__)).replace(os.sep, '/')
RWorkRepo = os.path.join(RScriptRepo,'tmp').replace(os.sep, '/')

EXTERNALPROCESS = True

def execute_r_script(idsimu, bloom_date, nb_fruits, nb_leaves):

    params = { 'bloom_date' : bloom_date, 'nb_fruits' : nb_fruits, 'nb_leaves' : nb_leaves }

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
'''.format(RScriptRepo, idsimu, RWorkRepo,idsimu,RScriptRepo,' , '.join([var+" = "+ repr(value).replace("'",'"') for var, value in list(params.items())]))
    #print script
    if not os.path.exists(RWorkRepo):
        os.makedirs(RWorkRepo)
    if EXTERNALPROCESS:
      launch_r(idsimu, script)
    else:
      launch_rpy(idsimu, script)

def get_R_cmd():
    import sys
    if sys.platform == 'win32':
        R_HOME = os.environ["R_HOME"]
        exe = os.path.join(R_HOME,'bin','Rscript.exe')
        assert os.path.exists(exe)
    else:
        exe = 'Rscript'
    return exe

def launch_r(idsimu, script):
    cwd = os.getcwd()
    os.chdir(RWorkRepo)
    
    launchfile = 'modellauncher-'+str(idsimu)+'.r'
    
    launcher = open(launchfile, 'w')
    launcher.write(script)
    launcher.close()
    
    exe = get_R_cmd()
    command = '"'+exe +'" '+launchfile+''
    os.system(command)
    
    os.remove(launchfile)

    logfile = "fruitmodel-{}.log".format(idsimu)
    if os.path.exists(logfile): os.remove(logfile)
    os.chdir(cwd)

def launch_rpy(idsimu, script):
  import rpy2.robjects as r
  return r.r(script)

#def get_fruitmodel_function():
#    def fruitmodel(idsimu, **params):
#        execute_r_script(idsimu, **params)
#    return fruitmodel


def wait_for_file(fname, timeout = 0.1):
  import time
  t = time.time()
  while abs(t - time.time()) < timeout and not os.path.exists(fname) : pass
  return os.path.exists(fname)

def fruitmodel(idsimu, bloom_date, nb_fruits, nb_leaves, dumpdir = None):
    #print 'Do simu', inflos
    tempfile = os.path.join(RWorkRepo,"resultats-"+str(idsimu)+".csv")
    if os.path.exists(tempfile): os.remove(tempfile)    

    execute_r_script(idsimu, bloom_date, nb_fruits, nb_leaves)

    if not wait_for_file(tempfile):
        failedfile = os.path.join(RWorkRepo,"failed-"+str(idsimu)+".csv")
        if os.path.exists(failedfile): os.remove(failedfile)
        return 
    else:
        from pandas import read_csv
        date_parser = lambda d : datetime.strptime(d, '%Y-%m-%d')
        result = read_csv(tempfile, parse_dates=['Date'], date_parser=date_parser)
        if dumpdir:
            import shutil
            shutil.copy(tempfile,os.path.join(dumpdir, 'meanfruit-'+'-'.join(map(str,inflos)))+'.csv')        
        os.remove(tempfile)
        return result

def pfruitmodel(params): 
    return fruitmodel(*params)

from vplants.mangosim.tools import *
from vplants.mangosim.utils.util_parallel import *

def applymodel(mtg, cycle, fruit_distance = 4, dump = True, dumptag = None, parallel = True):

    from random import randint
    verbose = False

    if verbose : print(" * Compute fruiting structures")
    from . import fruitingstructure as fs; reload(fs)

    fruiting_structures = fs.determine_fruiting_structure(mtg, cycle, fruit_distance = fruit_distance)

    #inflomtg = set([inflo for inflo,l in mtg.property('label').items() if l == 'Inflorescence' and mtg.property('nb_fruits')[inflo] > 0 and mtg.property('cycle')[inflo] == cycle] )
    #inflostruct = set([inflo for inflos, gus in fruiting_structures for inflo in inflos])
    #assert len(inflomtg.symmetric_difference(inflostruct)) == 0
    #from collections import Counter
    #c = Counter([inflo for inflos, gus in fruiting_structures for inflo in inflos])

    if verbose : print(" * Compute property of the structures")
    
    params = mtg.property('p')

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


    parameters = []
    for inflos, gus in fruiting_structures:
        bloom_dates = [params[inflo].fullbloom_date for inflo in inflos]
        if len(gus) == 0 and len(mtg.vertices(scale=mtg.max_scale())) == 1:
            nb_leaves    = 100
        else:
            nb_leaves    = sum([len(params[gu].final_length_leaves) for gu in gus if not gu is None])

        nb_fruits   = sum([params[inflo].nb_fruits for inflo in inflos])

        bloom_date  = bloom_dates[0] 
        bloom_date_date = bloom_date 
        cycledecal = bloom_date.year - 2002
        bloom_date  = str(bloom_date.day)+'/'+str(bloom_date.month)+'/2002'

        idsimu = randint(0,100000)
        idsimu += fruit_distance*100000

        parameters.append((idsimu, bloom_date, nb_fruits, nb_leaves, outdir if dump else None))

    if parallel:
        results = parmap(pfruitmodel, parameters)
    else:
        results = []
        for idsimu, bloom_date, nb_fruits, nb_leaves, outdir in parameters:
            results.append(fruitmodel(idsimu=idsimu, bloom_date=bloom_date, nb_fruits=nb_fruits, nb_leaves=nb_leaves, dumpdir=outdir))


    fruit_results = []
    for result, (inflos, gus) in zip(results, fruiting_structures):
        if result is None:

            # print 'Simu', idsimu, 'failed', inflos, nb_fruits
            for inflo in inflos:
                p = params[inflo]
                p.nb_fruits = 0
                p.fruits_weight = 0
                p.idsimu        = idsimu
                p.leaffruit_ratio = (nb_leaves, nb_fruits)
            continue
        else:

            # print 'Simu', idsimu, 'succeed', inflos, nb_fruits 
            dates = result["Date"]
            dates = [d.to_pydatetime() for d in dates]
            newyear = bloom_date_date.year
            dates = [date(d.year+cycledecal, d.month, d.day) for d in dates]
            fruitproperties = list(zip(result["Masse_Fruit"], result["sucres_solubles"],  result["acides_organiques"]))

            fruit_growth = dict(list(zip(dates,fruitproperties)))
            fruits_growth_stage_date, fruits_maturity_date = min(dates), max(dates)
            fruits_initial_weight, fruits_weight = min(result["Masse_Fruit"]), max(result["Masse_Fruit"])
            # print fruits_initial_weight, fruits_weight, fruits_growth_stage_date, fruits_maturity_date

            fruit_results.append((len(inflos), nb_leaves,  nb_fruits, fruits_weight, inflos, [params[inflo].nb_fruits for inflo in inflos] ))

            
            for inflo in inflos:
                p = params[inflo]
                p.fruits_growth_stage_date = fruits_growth_stage_date
                p.fruits_maturity_date     = fruits_maturity_date
                p.fruits_initial_weight    = fruits_initial_weight
                p.fruits_weight            = fruits_weight*p.nb_fruits
                p.fruits_growth            = fruit_growth
                p.idsimu                   = idsimu
                p.leaffruit_ratio          = (nb_leaves, nb_fruits)

             
    if dump:
        fstream = open(os.path.join(outdir,'fruitstructure.csv'),'w')
        maxbranch     = max([len(inflos)        for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_results])
        nbtotinflos   = sum([nbinflos           for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_results])
        nbtotleaf     = sum([nbleaf             for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_results])
        nbtotfruits   = sum([nbfruits           for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_results])
        masstotfruits = sum([massfruit*nbfruits for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_results])
        
        fstream.write('Filename\tNbInflos\tNbLeaf\tNbFruits\tMeanMassFruit\tTotalMassFruit')
        fstream.write(''.join(['\tIdsInflos_'+str(i) for i in range(maxbranch)]) )
        fstream.write(''.join(['\tNbFruitsPerInflos_'+str(i) for i in range(maxbranch)]))
        fstream.write('\n')
        for nbinflos, nbleaf, nbfruits, massfruit, inflos, nbfruitsperinflo in fruit_structures:
            fstream.write('meanfruit-'+'-'.join(map(str,inflos))+'\t'+str(nbinflos)+'\t'+str(nbleaf)+'\t'+str(nbfruits)+'\t'+str(massfruit)+'\t'+str(massfruit*nbfruits)+'\t'+'\t'.join(map(str,inflos))+'\t'*(1+maxbranch-len(inflos))+'\t'.join(map(str,nbfruitsperinflo))+'\t'*(maxbranch-len(inflos))+'\n' )
        
        fstream.write('TOTAL\t'+str(nbtotinflos)+'\t'+str(nbtotleaf)+'\t'+str(nbtotfruits)+'\t'+str(old_div(masstotfruits,nbtotfruits))+'\t'+str(masstotfruits)+'\n')
        fstream.close()
    
    if dump:
        return fruiting_structures, outdir
    else:
        return fruiting_structures
