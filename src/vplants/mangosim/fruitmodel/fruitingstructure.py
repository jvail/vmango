from __future__ import print_function
from __future__ import unicode_literals
from __future__ import absolute_import
from past.builtins import cmp
from builtins import str
from builtins import range
from vplants.mangosim.doralice_mtg.mtg_manipulation import *
from functools import reduce

def neighbours(mtg, node):
    """ Gives the neighbours of a node in the mtg. In a tree graph, it is the parent and the children """
    parent = mtg.parent(node)
    if parent: return [parent]+vegetative_children(mtg,node)
    else: return vegetative_children(mtg,node)

def nodes_at_distance(mtg, node, distance):
    """ Return all nodes at a distance of node in the mtg. A set is returned """
    orderi = neighbours(mtg, node)
    res = set(orderi)
    for j in range(1,distance):
        orderj = []
        for ni in orderi:
            if j < distance:
                candidates = neighbours(mtg, ni)
                orderj += [nj for nj in candidates if not nj in res]
        res |= set(orderj)
        orderi = orderj
    return res


def determine_fruiting_structure(mtg, cycle, fruit_distance):
    """ Determine the different fruiting branches.
        For this the different inflorescences that will gives fruits are first located. 
        All growth units at a given distance are associated with the inflo, forming a fruiting branches.
        If two set of growth units have common elements, the fruiting branches are merged.
        Return a list of tuple. Each tuple contains first a list of id of inflorescences and second a set of id of growth units.
    """
    def merge(supporti, supportj):
        return supporti[0]+supportj[0], supporti[1] | supportj[1]
    
    if len(mtg.vertices(scale=mtg.max_scale())) == 1:
      return [(mtg.vertices(scale=mtg.max_scale()),[])]
    mtgstyle = getMtgStyle()
    setMtgStyle(eSimulatedMtg)
    tree = get_all_trees(mtg)[0]

    inflo = [inflo for inflo in get_all_inflo_of_tree_at_cycle(mtg, tree, cycle) if get_nb_fruits(mtg,inflo) > 0]
    inflo_support = [([i],nodes_at_distance(mtg,i,fruit_distance)) for i in inflo]


    i = 0
    to_merge_at_last = []
    while i < len(inflo_support):
        # We first test if inflo_support[i] has common elements with others.
        tomerge = []
        for j in range(i+1,len(inflo_support)):
            if len(inflo_support[i][1] & inflo_support[j][1]) > 0 : 
                # The 2 sets have elements in common
                tomerge.append(j)

        # We merge the elements
        if len(tomerge) > 0:
            to_merge_at_last.append((i,inflo_support[i]))
            tomerge.sort()
            inflo_support[i] = reduce(merge, [inflo_support[m] for m in tomerge])
            for m in reversed(tomerge): del inflo_support[m]
        else:
            i += 1

    for i, m in to_merge_at_last:
        inflo_support[i] = merge(inflo_support[i], m)

    setMtgStyle(mtgstyle)
    return inflo_support


def lowest_common_ancestor(mtg, nodei, nodej):
    """ Return the lowest common ancestor lca of nodei and nodej in the mtg and the distance of nodei and nodej to lca"""
    ancestorsi = dict([(v,i) for i,v in enumerate(mtg.Ancestors(nodei)) ])
    ancestorsj = dict([(v,i) for i,v in enumerate(mtg.Ancestors(nodej)) ])
    print(ancestorsi)
    print(ancestorsj)
    
    commonancestors = list(set(ancestorsi.keys()) & set(ancestorsj.keys()))
    commonancestors.sort(key = lambda x: ancestorsi[x])
    try:
        return commonancestors[0], ancestorsi[commonancestors[0]], ancestorsj[commonancestors[0]]
    except:
        return None




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
    center = Point3Array(list(pos.values())).getCenter()
    a = list(pos.keys())
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
    index = list(range(nbcolors))
    print(distances)
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
        for i in range(len(index)-2):
            for j in range(i+1,len(index)-1):
                nindex = swap(index,i,j)
                ccost = cost(nindex, distances)
                if ccost < bcost:
                    bcost = ccost
                    bindex = nindex
        return bindex

    def bestswapi(index, distances, i):
        bcost = cost(index, distances)
        bindex = index
        for j in range(0,len(index)-1):
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
    print(bcost)
    bindex = index
    bestiter = None
    for i in range(10):
        success = True
        index = shuffle(index)
        icost = cost(index, distances)
        while success:
            nindex = bestswapi(index, distances, randint(0,nbcolors-1))
            cicost = cost(nindex, distances)
            if cicost < icost:
                index = nindex
                icost = cicost
                print(icost)
            else:
                success = False
        if icost < bcost:
            bcost = icost
            bindex = index
            print('bestiter',i)
            bestiter = i

    print(bcost)
    result = {}
    i = 0
    for inflos, gus in fruiting_structures:
        for inflo in inflos:
            result[inflo] = index[i]
            i+=1
        nindex.append(result[inflos[0]])
    print(bestiter)
    #print result
    return result


def determine_colorindex(fruiting_structures, mtg, scene):
    from vplants.mangosim.tools import load_obj, dump_obj
    cache = 'cache_colorinflo.pkl'
    result = None
    allinflos1 = set([vid for vid,lid in list(mtg.property('_axial_id').items()) if mtg.label(vid) == 'Inflorescence' and mtg.property('nb_fruits')[vid] > 0])
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

    print('determine colors')
    colindex = determine_colorindex(fruiting_structures, mtg, scene)
    print(colindex)
    allinflos = [lid for vid,lid in list(idmap.items()) if mtg.label(vid) == 'Inflorescence']
    for inflos, gus in fruiting_structures:
        i = colindex.pop(0)
        col = colors(i)
        mat = Material([int(c*100) for c in col[:3]],2)
        for j in inflos:
            structures[idmap[j]] = mat
        for j in gus:
            structures[idmap[j]] = mat
        print(col, inflos)

    definfmat = Material((50,50,50))
    for inf in allinflos:
        if not inf in structures:
            structures[inf] = definfmat

    defmat = Material((0,0,0))
    print('compute colored scene')
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
    from vplants.mangosim.tools import load_obj
    import numpy as np

    from openalea.plantgl.all import Scene, Viewer
    cycle=4
    lowres = False
    mtg = load_obj('structure-cycle'+str(cycle)+('b' if lowres else '')+'.pkl')
    #Viewer.display(sc)

    from . import fruitingstructure as fsm
    distances = [int(sys.argv[1])] if len(sys.argv) > 1 else list(range(1,10))
    if len(sys.argv) > 2:
        distances = list(range(distances[0], int(sys.argv[2])))

    params = mtg.property('p')
    for distance in distances:
        fs = fsm.determine_fruiting_structure(mtg, cycle=cycle, fruit_distance = distance)
        print()
        print('Nb of groups',len(fs))
        #lfr = leaf_fruit_ratio(mtg, fs)
        #print [l/float(f) for l,f in lfr]
        #print np.mean([l/float(f) for l,f in lfr])
        nbgus = [len(gus) for inflos,gus in fs]
        #print nbgus
        print(np.mean(nbgus), np.std(nbgus))

        radii = [max([params[gu].radius for gu in gus]) for inflos,gus in fs]
        #print radii
        print(np.mean(radii), np.std(radii))


    #fs = applymodel(mtg, 3, int(sys.argv[1]) if len(sys.argv) > 1 else 3) 
    colorrepresentation = False
    if colorrepresentation:
        sc = Scene('structure-cycle'+str(cycle)+('b' if lowres else '')+'.bgeom')
        print('Start coloring')
        nsc = color_structures(fs, mtg, sc)
        print('End coloring')
        nsc.save('structure-cycle'+str(cycle)+'-dist'+str(distance)+'.bgeom')
        #Viewer.display(nsc)
        nsc.save('structure-cycle'+str(cycle)+'-dist'+str(distance)+'-core.pov')
