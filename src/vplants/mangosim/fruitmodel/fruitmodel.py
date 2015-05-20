import rpy2.robjects as r
from os.path import join, abspath, dirname

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
    script = file(join(RScriptRepo,"fruitmodel.r"),'r').read()
    r.r(script)
    return r.r('empty_fruit_model')


def applymodel(mtg, cycle, fruit_distance = 3):
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
        leaf_nbs    = sum([len(params[gu].final_size_Leaves) for gu in gus])
        nb_fruits   = [params[inflo].nb_fruits for inflo in inflos]
        print nb_fruits

        bloom_date  = bloom_dates[0]
        bloom_date  = str(bloom_date.day)+'/'+str(bloom_date.month)+'/'+str(bloom_date.year)
        # call fruit model in r 
        fruitmodel(bloom_date, nb_fruits, leaf_nbs)

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
    print len(fs)
    for inflos, gus in fs:
        print inflos, list(gus)    
    Viewer.display(color_structures(fs, mtg, sc))