import rpy2.robjects as r

def runmodel():
    script = file(U"sim.r",'r').read()
    r.r(script)

def get_fruitmodel_functions():
    script = file(U"fruitmodel.r",'r').read()
    r.r(script)
    return r.r('fruitmodel')

def test():
    fruitmodel, fruitgrowth = get_fruitmodel_functions()
    fruitdata = fruitmodel(1, '10/8/03', 0.2, 0.5)
    for t in xrange(100):
        fruitdata = fruitgrowth(fruitdata)
    print fruitdata


def applymodel(mtg, cycle):
    print 'apply fruit model'
    import fruitingstructure as fs; reload(fs)
    fruiting_structures = fs.determine_fruiting_structure(mtg,cycle, 3)
    print fruiting_structures



if __name__ == '__main__':
    from vplants.mangosim.tools import load_obj
    mtg = load_obj('fruitstructure.pkl','../shoot_growth')
    applymodel(mtg, 3)