from vplants.mangosim.doralice_mtg.mtg_manipulation import *

def neighbours(mtg, node):
    """ Gives the neighbours of a node in the mtg. In a tree graph, it is the parent and the children """
    return [mtg.parent(node)]+vegetative_children(mtg,node)

def nodes_at_distance(mtg, node, distance):
    """ Return all nodes at a distance of node in the mtg. A set is returned """
    orderi = neighbours(mtg, node)
    res = set(orderi)
    for j in xrange(1,distance):
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
    
    mtgstyle = getMtgStyle()
    setMtgStyle(eSimulatedMtg)
    tree = get_all_trees(mtg)[0]

    inflo = get_all_inflo_of_tree_at_cycle(mtg, tree, cycle)
    inflo_support = [([i],nodes_at_distance(mtg,i,fruit_distance)) for i in inflo if get_nb_fruits(mtg,i) > 0]


    i = 0
    to_merge_at_last = []
    while i < len(inflo_support):
        # We first test if inflo_support[i] has common elements with others.
        tomerge = []
        for j in xrange(i+1,len(inflo_support)):
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
    print ancestorsi
    print ancestorsj
    
    commonancestors = list(set(ancestorsi.keys()) & set(ancestorsj.keys()))
    commonancestors.sort(cmp = lambda x,y : cmp(ancestorsi[x],ancestorsi[y]))
    try:
        return commonancestors[0], ancestorsi[commonancestors[0]], ancestorsj[commonancestors[0]]
    except:
        return None
