import rpy2.robjects as r
from os.path import join, abspath, dirname
import os

def execute_r_script(script, **params):
    R_HOME = os.environ["R_HOME"]
    exe = os.path.join(R_HOME,'bin','R.exe')
    assert os.path.exists(exe)
    launchfile = 'myscript.R'
    launcher = file(launchfile,'w')
    for var, value in params.items():
        launcher.write(var+" <- "+ str(value)+'\n')
    launcher.write('source "'+launchfile+'"\n')
    #exe = exe.replace(' ','\\ ')
    command = '"'+exe +'" "'+launchfile+'"'
    print command
    os.system(command)

RScriptRepo = dirname(abspath(__file__))

def runmodel():
    script = file(U"sim.r",'r').read()
    r.r(script)

def get_fruitmodel_functions():
    script = file(join(RScriptRepo,"fruitmodel.r"),'r').read()
    r.r(script)
    return r.r('fruitmodel'), r.r('fruitgrowth')

def test():
    fruitmodel, fruitgrowth = get_fruitmodel_functions()
    fruitdata = fruitmodel(1, '10/8/03', 0.2, 0.5)
    for t in xrange(100):
        fruitdata = fruitgrowth(fruitdata)
    print fruitdata

def get_fruitmodel_function_test():
    script = file(join(RScriptRepo,"model_fruit_final.r"),'r').read()
    def fruitmodel(**params):
        execute_r_script(script, **params)
    return fruitmodel
    #r.r(script)
    #return r.r('fruitmodel')


def applymodel(mtg, cycle, fruit_distance = 3):
    from pandas import read_csv
    print 'apply fruit model'
    print " * Load R function"
    fruitmodel = get_fruitmodel_function_test()

    print " * Compute fruiting structures"
    import fruitingstructure as fs; reload(fs)
    fruiting_structures = fs.determine_fruiting_structure(mtg, cycle, fruit_distance = fruit_distance)

    print " * Compute property of the structures"
    params = mtg.property('p')
    for inflos, gus in fruiting_structures:
        bloom_dates = [params[inflo].bloom_date for inflo in inflos]
        leaf_nbs    = sum([len(params[gu].final_length_leaves) for gu in gus])
        nb_fruits   = sum([params[inflo].nb_fruits for inflo in inflos])
        print nb_fruits

        bloom_date  = bloom_dates[0]
        bloom_date  = str(bloom_date.day)+'/'+str(bloom_date.month)+'/'+str(bloom_date.year)
        # call fruit model in r 
        result = fruitmodel(bloom_date=bloom_date, nb_fruits=nb_fruits, leaf_nbs=leaf_nbs)
        result = read_csv("resultats.csv")
        #print result["Masse_Fruit"]
        #print type(result)
        #print dir(result)
        #print result["Date"]
        #print result["Masse_Fruit"]
        fruit_growth = dict(zip(result["Date"],result["Masse_Fruit"]))
        
        for inflo in inflos:
            params[inflo].fruit_appearance_date = min(result["Date"])
            params[inflo].fruit_maturity_date   = max(result["Date"])
            params[inflo].fruit_masses          = fruit_growth
        
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
    from vplants.mangosim.tools import load_obj
    from openalea.plantgl.all import Scene, Viewer
    mtg = load_obj('fruitstructure.pkl','../shoot_growth')
    sc = Scene('../shoot_growth/fruitstructure.bgeom')
    fs = applymodel(mtg, 3, int(sys.argv[1]) if len(sys.argv) > 1 else 3) 
    #print len(fs)
    #for inflos, gus in fs:
    #    print inflos, list(gus)    
    #Viewer.display(color_structures(fs, mtg, sc))