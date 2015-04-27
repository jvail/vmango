from vplants.mangosim.tools import *
from vplants.mangosim.state import *
from vplants.mangosim.util_date import *
from mtg_import import get_mtg
from openalea.mtg import MTG

# Fred Note:
# Date format for Burst_Date_Children
# the hundred unit represent delta of cycle
# the two other number represent the month

# Pb: most of the flowering date are NA in 2004


__MtgStyle = eMeasuredMtg

BurstDatePropertyName = 'date_burst'
BloomPropertyName = 'flowering'
NbFruitPropertyName = 'nb_fr'
CyclePropertyName = 'year'
VarietyPropertyName = 'var'
TreeNamePropertyName = 'code'
MixedInfloPropertyName = 'mixed_inflo' 
LoadingPropertyName = 'fr_load'

LoadedValue = 'C'
NotLoadedValue = 'NC'
InfloLabel = 'F'
FruitLabel = ''

TreeScale = 1
GUScale = 4

def setMtgStyle(style):
    global __MtgStyle, BurstDatePropertyName, BloomPropertyName, CyclePropertyName, VarietyPropertyName, TreeNamePropertyName, MixedInfloPropertyName, LoadingPropertyName
    global LoadedValue, NotLoadedValue, InfloLabel, FruitLabel
    global TreeScale, GUScale
    if __MtgStyle != style:
        __MtgStyle = style
    if __MtgStyle == eMeasuredMtg:
        BurstDatePropertyName = 'date_burst'
        BloomPropertyName = 'flowering'
        CyclePropertyName = 'year'
        VarietyPropertyName = 'var'
        TreeNamePropertyName = 'code'
        MixedInfloPropertyName = 'mixed_inflo' 
        LoadingPropertyName = 'fr_load'

        LoadedValue = 'C'
        NotLoadedValue = 'NC'
        InfloLabel = 'F'
        FruitLabel = ''

        TreeScale = 1
        GUScale = 4
    elif __MtgStyle == eSimulatedMtg:
        BurstDatePropertyName = 'burst_date'
        BloomPropertyName = 'bloom_date'
        CyclePropertyName = 'cycle'
        VarietyPropertyName = 'variety'
        TreeNamePropertyName = 'treename'
        MixedInfloPropertyName = 'mixed_inflo' 
        LoadingPropertyName = 'loading'

        LoadedValue = 'C'
        NotLoadedValue = 'NC'
        InfloLabel = 'Inflorescence'
        FruitLabel = 'Fruit'

        TreeScale = 1
        GUScale = 2
    else:
        assert __MtgStyle in [eMeasuredMtg,eSimulatedMtg]

def use_global_mtg(f):
    def simplified_func(*args, **kwds):
        if not 'mtg' in kwds and len(args) == 0 or type(args[0]) != MTG:
            return f(get_mtg(),*args,**kwds)
        else:
            return f(*args,**kwds)
    return simplified_func

# Characterizing UCs
@use_global_mtg
def get_unit_cycle(mtg, unit):
  """Return the cycle of the unit.  """
  if mtg.label(unit) in 'MP' : return 3
  else: return mtg.property(CyclePropertyName)[unit]

@use_global_mtg
def get_burst_date(mtg, unit, default=None):
  """Return the date of burst of the unit.  """
  return mtg.property(BurstDatePropertyName).get(unit, default)

@use_global_mtg
def has_burst_date(mtg, unit):
  """Return the date of burst of the unit.  """
  return unit in mtg.property(BurstDatePropertyName) and (not mtg.property(BurstDatePropertyName)[unit] is None)


@use_global_mtg
def get_bloom_dates(mtg, inflo, default=None):
    if __MtgStyle == eMeasuredMtg:
        return mtg.property(BloomPropertyName).get(inflo,default)
    else:
        return [mtg.property(BloomPropertyName).get(inflo,default)]


@use_global_mtg
def get_bloom_date(mtg, inflo, default=None):
    if __MtgStyle == eMeasuredMtg:
        res = mtg.property(BloomPropertyName).get(inflo,default)
        if res: return res[0]
    else:
        return mtg.property(BloomPropertyName).get(inflo,default)


@use_global_mtg
def get_nb_fruits(mtg, inflo, default=None):
    nb_fruit = mtg.property(NbFruitPropertyName).get(inflo,'')
    if len(nb_fruit) == 0:
        return 0
    else:
        return int(nb_fruit)


@use_global_mtg
def is_gu_dead_before_cycle_end(mtg, gu, cycle):
  """Return a boolean that indicates if a unit growth is still dead at the end of a cycle. """
  if gu not in mtg.property('dead') : # not dead
    return False
  else : 
    # We check if the death is in the cycle
    date_dead = date_from_string(mtg.property('dead')[gu])
    date_end_cycle = date(2000+cycle, 7, 15)
    return date_dead < date_end_cycle

@use_global_mtg
def is_inflorescence(mtg, unit):
    return mtg.label(unit) == InfloLabel

@use_global_mtg
def is_fruit(mtg, unit):
    return mtg.label(unit) == FruitLabel

@use_global_mtg
def is_gu(mtg, unit):
    return not mtg.label(unit) in [InfloLabel,FruitLabel]

@use_global_mtg
def is_gu_flowering(mtg, gu, inflocycle = None):
    for ch in mtg.children(gu):
        if is_inflorescence(mtg, ch):
            if inflocycle is None or get_unit_cycle(mtg,ch) == inflocycle:
                return True
            else : return False

@use_global_mtg
def is_gu_fruiting(mtg, gu, inflocycle = None):
    for ch in mtg.children(gu):
        if is_fruit(mtg, ch) or (is_inflorescence(mtg, ch) and get_nb_fruits(mtg,ch) > 0):
            if inflocycle is None or get_unit_cycle(mtg,ch) == inflocycle:
                return True
            else : return False

@use_global_mtg
def is_gu_mixed_inflorescence(mtg, gu):
    return gu in mtg.property(MixedInfloPropertyName)

@use_global_mtg
def is_terminal_at_cycle(mtg, gu, cycle):
    # we consider gu borned in cycle or before and not dead before end of cycle.
    if get_unit_cycle(mtg,gu) <= cycle and not is_gu_dead_before_cycle_end(mtg,gu,cycle):
        for child in mtg.children(gu):
            if is_gu(mtg,child) and get_unit_cycle(mtg,child) <= cycle:
                return False
        # No child of gu was borned before end of cycle
        return True
    return False


@use_global_mtg
def get_nature_gu(mtg, gu):
  if is_gu_fruiting(mtg,gu) : return eFruiting
  return eFlowering if is_gu_flowering(mtg, gu) else eVegetative

@use_global_mtg
def is_trunk(mtg, gu):
  return mtg.parent(gu)is None

@use_global_mtg
def is_apical(mtg, gu):
    if is_trunk(mtg,gu) : return True
    return mtg.property('edge_type')[gu] == '<'

@use_global_mtg
def is_lateral(mtg, gu):
    if is_trunk(mtg,gu) : return False
    return mtg.property('edge_type')[gu] == '+'

@use_global_mtg
def get_position_gu(mtg, gu):
  """ """
  return eApical if is_apical(mtg, gu) else eLateral

@use_global_mtg
def get_parent(mtg, gu):
    return mtg.parent(gu)

@use_global_mtg
def get_ancestor_of_previous_cycle(mtg, gu):
    gu_cyle = get_unit_cycle(mtg, gu)
    target_cycle = gu_cyle - 1
    if gu_cyle != 3: # No ancestor for unit of cycle 3
        parent_gu = mtg.parent(gu)
        while get_unit_cycle(mtg,parent_gu) ==  gu_cyle:
            parent_gu = mtg.parent(parent_gu)
        return parent_gu

@use_global_mtg
def get_ancestors_within_cycle(mtg, gu):
    cycle = get_unit_cycle(mtg,gu)
    ancestors = []
    parent = get_parent(mtg,gu)
    while  (not (parent is None)) and get_unit_cycle(mtg,parent) == cycle:
        ancestors.append(parent)
        parent = get_parent(mtg,parent)
    return ancestors

@use_global_mtg
def get_all_ancestors(mtg, gu):
    ancestors = []
    parent = get_parent(mtg,gu)
    while  (not (parent is None)):
        ancestors.append(parent)
        parent = get_parent(mtg,parent)
    return ancestors

@use_global_mtg
def nb_ancestors_within_cycle(mtg, gu):
    return len(get_ancestors_within_cycle(mtg, gu))

rank_within_cycle = nb_ancestors_within_cycle

@use_global_mtg
def get_nature_position_ancestor(mtg, gu):
  """ """
  ancestor = get_ancestor(mtg, gu)
  if ancestor:
    # Fred Note: Position value has been changed. 1 for position means Apical
    return get_nature_gu(mtg,ancestor), get_position_gu(mtg,ancestor)

@use_global_mtg
def get_tree_of_gu(mtg, gu):
    return mtg.complex_at_scale(gu,scale = TreeScale)


@use_global_mtg
def vegetative_children(mtg,gu):
    return [child for child in mtg.children(gu) if is_gu(mtg,child) ]

@use_global_mtg
def vegetative_children_at_cycle(mtg,gu,cycle):
    return [child for child in mtg.children(gu) if is_gu(mtg,child) and get_unit_cycle(mtg,child) == cycle]


@use_global_mtg
def inflorescence_children(mtg,gu):
    return [child for child in mtg.children(gu) if is_inflorescence(mtg, child)]

@use_global_mtg
def inflorescence_children_at_cycle(mtg,gu,cycle):
    return [child for child in mtg.children(gu) if is_inflorescence(mtg, child) and get_unit_cycle(mtg,child) == cycle]



# Characterizing trees
@use_global_mtg
def is_loaded(mtg,tree):
    return mtg.property(LoadingPropertyName)[tree] == LoadedValue

@use_global_mtg
def load_state(mtg,tree):
    return eLoaded if is_loaded(mtg,tree) else eNotLoaded

@use_global_mtg
def get_tree_name(mtg,tree):
    return mtg.property(TreeNamePropertyName)[tree]

@use_global_mtg
def get_tree_from_name(mtg,treename):
    for tree in get_all_trees(mtg):
        if get_tree_name(mtg,tree) == treename:
            return tree

@use_global_mtg
def get_all_trees(mtg):
    return list(mtg.property(VarietyPropertyName).keys())

@use_global_mtg
def get_all_treenames(mtg):
    return map(lambda tid: get_tree_name(mtg,tid), get_all_trees(mtg))

@use_global_mtg
def get_treenames_of_variety(mtg, variety = 'cogshall'):
    return map(lambda tid: get_tree_name(mtg,tid), get_all_trees_of_variety(mtg, variety))

@use_global_mtg
def get_all_trees_of_variety(mtg, variety = 'cogshall'):
    return [k for k,v in mtg.property(VarietyPropertyName).items() if v == variety]

@use_global_mtg
def get_variety(mtg, tree):
    return mtg.property(VarietyPropertyName)[tree]

@use_global_mtg
def get_all_varieties(mtg):
    return list(set(mtg.property(VarietyPropertyName).values()))

# tree selection
@use_global_mtg
def select_trees(mtg, loaded= None, variety = "cogshall"):
  """ Return a list of trees from a given variety and which are loaded or not.
  Parameters : 
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  if loaded is None:
      if variety == "all": return get_all_trees(mtg)
      else: return [i for i,v in mtg.property(VarietyPropertyName).items() if v == variety]
  else : 
      load = mtg.property(LoadingPropertyName)
      loaded_propvalue = LoadedValue if loaded == eLoaded else NotLoadedValue
      if variety == "all": 
            return [i for i in get_all_trees(mtg) if load[i] == loaded_propvalue]
      else: 
            return [i for i,v in mtg.property(VarietyPropertyName).items() if v == variety and load[i] == loaded_propvalue]

@use_global_mtg
def get_first_gu(mtg, tree):
    return mtg.component_roots_at_scale(tree,scale = GUScale)[0]

# Selection of UCS

@use_global_mtg
def get_all_gus_of_tree(mtg, tree):
  """Return all the unit growth of a tree and for a cycle 
  Parameters : 
    tree: integer in g.property('var')
    cycle: integer 3, 4 or 5"""
  return [i for i in mtg.components_at_scale(tree,scale=GUScale) if is_gu(mtg,i)]
 

@use_global_mtg
def get_all_gus_of_tree_at_cycle(mtg, tree, cycle):
  """Return all the unit growth of a tree and for a cycle 
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5"""
  gus_tree = get_all_gus_of_tree(mtg, tree)
 
  # keep unit growth (remove inflorescence)
  gus_tree_cycle = [gu for gu in gus_tree if get_unit_cycle(mtg, gu) == cycle]

  # Fred Question: Pourquoi on retrie sur les date de burst?
  if cycle != 3:
    date_burst = mtg.property(BurstDatePropertyName)
    gus_tree_cycle = [gu for gu in gus_tree_cycle if in_cycle(date_burst[gu],cycle)]
  return gus_tree_cycle

@use_global_mtg
def get_all_gus_of_cycle(mtg, cycle = 4, loaded = None, variety = "cogshall"):
  """ 
  Parameters : 
    cycle : an integer, cycle 4 or 5
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  gus_of_cycle = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
    gus_of_cycle += get_all_gus_of_tree_at_cycle(mtg, tree, cycle)
  return gus_of_cycle

@use_global_mtg
def get_all_gus_of_variety(mtg, loaded = None, variety = "cogshall"):
  """ 
  Parameters : 
    cycle : an integer, cycle 4 or 5
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  gus_of_cycle = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
    for cycle in [3,4,5]:
        gus_of_cycle += get_all_gus_of_tree_at_cycle(mtg, tree, cycle)
  return gus_of_cycle

@use_global_mtg
def get_sorted_gus(mtg, cycles = [4,5], loaded = [eLoaded,eNotLoaded], varieties = ['cogshall']):
    """
    Return all gus sorted according to cycles, loading and tree varieties.
    Parameters :
        cycles : a list of integer. All cycles to consider
        loaded : a list of load state. All load state to consider
        varieties : a list of varieties name. All varieties to consider.
    """
    gus = {}
    for var in varieties:
        for load in loaded:
            for cycle in cycles:
                gus[(cycle, load, var)] = get_all_gus_of_cycle(mtg, cycle, load, var)
    return gus

@use_global_mtg
def get_all_inflo_of_variety(mtg, loaded = None, variety = "cogshall"):
  """ 
  Parameters : 
    cycle : an integer, cycle 4 or 5
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  inflos = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
        inflos += [i for i in mtg.components_at_scale(tree,scale=GUScale) if is_inflorescence(mtg,i)]
  return inflos



# def get_gus_tree_cycle_in_extremity(tree, cycle):
#   """Return a list of unit growth which are in the extremity of the canopy (for a tree and for a cycle).
#   If a unit growth dead in the cycle, it is remove from the list of terminal unit growth.
#   Parameters : 
#     tree: integer giving id of one tree. To select in g.property('var').keys() for instance.
# 	cycle: integer 3, 4 or 5"""

#   extremity = []
  
#   if cycle == 4 or cycle == 5:
#     # Fred: Je ne comprends pas ce qui est fait la.
#     gus_extremity_cycleMinus1 = get_terminal_gus(mtg, tree,cycle-1)
#     for i1 in gus_extremity_cycleMinus1:
#       children_extremity = [i2 for i2 in g.children(i1) if g.label(i2)!='F' and get_cycle_gu(i2)==cycle]
#       if children_extremity == [] and not is_dead_in_cycle(i1,cycle): extremity.append(i1)

#   gus_veget_tree_cycle = get_all_gus_of_tree_at_cycle(mtg, tree, cycle)
#   for i in gus_veget_tree_cycle :
#     childrens_in_cycle = [child for child in g.children(i) if not g.label(child)!='F' and get_cycle_gu(child)==cycle ]
#     if childrens_in_cycle == [] : extremity.append(i)
#   return extremity



@use_global_mtg
def get_terminal_gus_of_tree_at_cycle(mtg, tree, cycle):
  """
    Return a list of unit growth which are in the extremity of the canopy (for a tree and at end of a given cycle).
    If a unit growth dead in the cycle, it is remove from the list of terminal unit growth.
  Parameters : 
    tree: integer giving id of one tree. To select in g.property('var').keys() for instance.
    cycle: integer 3, 4 or 5"""  
  return [ gu for gu in get_all_gus_of_tree_at_cycle(mtg, tree, cycle) if is_terminal_at_cycle(mtg, gu, cycle)]



@use_global_mtg
def get_terminal_gus_of_variety_at_cycle(mtg, cycle=4, loaded= None, variety="cogshall"):
  """Return the list of terminal growth unit in the cycle and for the given variety.
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True gus are from loaded trees, else they are from not loaded trees
    variety : a string, the choice or variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  terminal_gus = []
  for tree in select_trees(mtg, loaded, variety) : 
    terminal_gus += get_terminal_gus_of_tree_at_cycle(mtg, tree, cycle)
  return terminal_gus

@use_global_mtg
def get_non_terminal_gus_of_tree_at_cycle(mtg, tree, cycle):
  """
    Return a list of unit growth which are in the extremity of the canopy (for a tree and at end of a given cycle).
    If a unit growth dead in the cycle, it is remove from the list of terminal unit growth.
  Parameters : 
    tree: integer giving id of one tree. To select in g.property('var').keys() for instance.
    cycle: integer 3, 4 or 5"""  
  return [ gu for gu in get_all_gus_of_tree_at_cycle(mtg, tree, cycle) if not is_terminal_at_cycle(mtg, gu, cycle)]


@use_global_mtg
def get_non_terminal_gus_of_variety_at_cycle(mtg, cycle=4, loaded= None, variety="cogshall"):
  """Return the list of terminal growth unit in the cycle and for the given variety.
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True gus are from loaded trees, else they are from not loaded trees
    variety : a string, the choice or variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  terminal_gus = []
  for tree in select_trees(mtg, loaded, variety) : 
    terminal_gus += get_non_terminal_gus_of_tree_at_cycle(mtg, tree, cycle)
  return terminal_gus

@use_global_mtg
def get_sorted_terminal_gu(mtg, cycles = [3,4,5], loaded = [eLoaded,eNotLoaded], varieties = ['cogshall']):
    """
    Return all teminal gus sorted according to cycles, loading and tree varieties.
    Parameters :
        cycles : a list of integer. All cycles to consider
        loaded : a list of load state. All load state to consider
        varieties : a list of varieties name. All varieties to consider.
    """
    gus = {}
    for var in varieties:
        for load in loaded:
            for cycle in cycles:
                gus[(cycle, load, var)] = get_terminal_gus_of_variety_at_cycle(mtg, cycle, load, var)
    return gus    


@use_global_mtg
def get_first_layer_gus(mtg, tree, cycle):
  assert 3 <= cycle <= 5
  return [ gu for gu in get_all_gus_of_tree_at_cycle(mtg, tree, cycle) if get_unit_cycle(mtg,get_parent(mtg, gu)) == cycle - 1]

@use_global_mtg
def get_sorted_terminal_gu_per_nb_ancestor_within_cycle(mtg, cycles = [4,5], variety = 'cogshall'):
    ancestor_rank = {}
    for cycle in cycles:
        terminal_gus = get_terminal_gus_of_variety_at_cycle(mtg, cycle, None, variety)
        nb_ancestors = map(lambda x: nb_ancestors_within_cycle(mtg,x),terminal_gus)
        for gu,nba in zip(terminal_gus, nb_ancestors):
            ancestor_rank.setdefault(nba,[]) 
            ancestor_rank[nba].append(gu)
    return ancestor_rank

#### test #####

@use_global_mtg
def check_cycle_and_burst_date_coherence(mtg, varieties = ['cogshall']):
    date_burst = mtg.property(BurstDatePropertyName)
    for var in varieties:
        gus_var = []
        for tree in select_trees(mtg,variety=var):
            gus_var += get_all_gus_of_tree(mtg, tree)
        invalid_gus = []
        for gu in gus_var:
            if (gu in date_burst):
               if not in_cycle(date_burst[gu],get_unit_cycle(mtg,gu)):
                    invalid_gus.append(gu)

        if len(invalid_gus) > 0:
            import warnings
            warnings.warn(('Incoherent cycle and burst date for %i gus of %s :' % (len(invalid_gus), var)) + str(invalid_gus))

@use_global_mtg
def check_terminal3_producing_at_cycle5(mtg, variety = 'cogshall'):
    terminal_gus = get_terminal_gus_of_variety_at_cycle(mtg, 3, None, variety)
    terminal_gus3 = [gu for gu in terminal_gus if get_unit_cycle(mtg,gu) == 3]
    terminal_gu3_giving_gu5 = [gu for gu in terminal_gus3 if len(vegetative_children_at_cycle(mtg,gu,5)) > 0]
    if len(terminal_gu3_giving_gu5) > 0:
        print('''There is %i terminal gus of cycle 3 that produces gus at cycle 5. The date format do not make it possible to take this into account.''' % len(terminal_gu3_giving_gu5))
    terminal_gu3_giving_inflo5 = [gu for gu in terminal_gus3 if len(inflorescence_children_at_cycle(mtg,gu,5)) > 0]
    if len(terminal_gu3_giving_inflo5) > 0:
        print('''There is %i terminal gus of cycle 3 that produces inflorescences at cycle 5. The date format do not make it possible to take this into account.''' % len(terminal_gu3_giving_inflo5))


@use_global_mtg
def check_apical_ratio_in_first_layer(mtg, variety = 'cogshall'):
    trees = get_all_trees_of_variety(mtg, variety)
    trees.sort(lambda x,y: cmp(is_loaded(mtg,x),is_loaded(mtg,y)))
    treenames = [get_tree_name(mtg,i) for i in trees]
    loaded = [is_loaded(mtg,i) for i in trees]
    def apical_ratio(mtg, trees, cycle):
        if type(trees) == int: trees = [trees]
        units = []
        for tree in trees:
            units += get_first_layer_gus(mtg, tree, cycle)
        apicals = [i for i in units if is_apical(mtg,i)]
        return len(units), len(apicals), len(apicals)/float(len(units))
    apical_ratios = [apical_ratio(mtg,tree,4) for tree in trees]
    loaded_trees = [tree for tree in trees if is_loaded(mtg,tree)]
    notloaded_trees = [tree for tree in trees if not is_loaded(mtg,tree)]
    apical_ratios += [apical_ratio(mtg,_trees,4) for _trees in [loaded_trees, notloaded_trees,trees]]
    trees += [None,None,'All']
    treenames += ['Loaded','NoLoad','All']
    loaded += [True, False, sum(loaded)/float(len(loaded))]


    print 'Tree\tName\tLoaded\tNbUnits\tNbApical\tRatio'
    for t,n,l,r in zip(trees,treenames,loaded,apical_ratios):
        print t,'\t',n,'\t',l,'\t',r[0],'\t',r[1],'\t',r[2]

@use_global_mtg
def check_terminal_flowering_has_no_apical(mtg, variety = 'cogshall'):
    flowering_with_apical = []
    for cycle in range(3,6):
        for gu in get_terminal_gus_of_variety_at_cycle(mtg, cycle, None, variety):
            has_inflo, has_apical = False, False
            for ch in mtg.children(gu):
                if is_inflorescence(mtg, ch):
                    has_inflo = True
                elif is_apical(mtg, ch):
                    has_apical = True
            if has_inflo and has_apical:
                flowering_with_apical.append(gu)

    print "It exists",len(flowering_with_apical),"terminal gu with both inflorescence and apical vegetative children :",flowering_with_apical
    nbterminalgus = sum([len(get_terminal_gus_of_variety_at_cycle(mtg, cycle, None, variety)) for cycle in xrange(3,6)])
    print "It represents",100.*len(flowering_with_apical)/nbterminalgus,"% of the terminal gus."

@use_global_mtg
def check_if_has_lateral_has_apical(mtg, variety = 'cogshall'):
    children_with_no_apical = []
    for gu in get_all_gus_of_variety(mtg, None, variety):
        if len(inflorescence_children(mtg,gu)) == 0:
            children = vegetative_children(mtg, gu)
            has_apical, nb_children = False, len(children)
            for ch in children:
                if is_apical(mtg, ch):
                        has_apical = True
            if nb_children > 0 and not has_apical:
                    children_with_no_apical.append(gu)

    print "It exists",len(children_with_no_apical)," gu with only vegetative lateral children and no apical and no inflorescences:",children_with_no_apical
    print "It represents",100.*len(children_with_no_apical)/len(get_all_gus_of_variety(mtg, None, variety)),"% of the gus."

@use_global_mtg
def check_apical_strength(mtg, cycles = [4,5], variety = 'cogshall'):
    sorted_gus = get_sorted_terminal_gu_per_nb_ancestor_within_cycle(mtg, cycles=cycles, variety = variety)
    maxorder = max(sorted_gus.keys())
    for order, ucs in sorted_gus.items():
        histo = dict()
        histol = dict([(True,0),(False,0)])
        histonl = dict([(True,0),(False,0)])
        for uc in ucs:
            ancestors = get_ancestors_within_cycle(mtg,uc)
            apstrength = tuple(reversed([is_apical(uc)]+[is_apical(auc) for auc in ancestors]))
            histo[apstrength] = histo.get(apstrength,0) + 1
            histol[is_apical(uc)] += 1
        for o in range(order+1,maxorder+1):
            for auc in sorted_gus[o]:
                ancestors = get_ancestors_within_cycle(mtg,auc)
                histonl[is_apical(ancestors[-order])] += 1

        print 'For nb ancestor = ',order,', we have',len(ucs),'gu with the following histogram of apical strength:'
        ordered_config = list(histo.keys())
        ordered_config.sort(lambda x,y:cmp(len(x),len(y)))
        for k in ordered_config:
            print k, histo[k]
        print "only considering apical prop of order",order,"for terminal :",histol
        print "only considering apical prop of order",order,"for non terminal :",histonl




@use_global_mtg
def check_burst_dates(mtg, cycles = [4,5], variety = 'cogshall'):
    from vplants.mangosim.util_date import Month
    sorted_gus = get_sorted_terminal_gu_per_nb_ancestor_within_cycle(mtg, cycles=cycles, variety = variety)
    maxorder = max(sorted_gus.keys())
    Month = dict([(i,v) for v,i in Month.items()])
    for order, ucs in sorted_gus.items():
        from collections import OrderedDict
        histo_date = [OrderedDict([(i,0) for i in xrange(6,13)]+[(i,0) for i in xrange(1,6)]) for i in xrange(order+1)]
        for uc in ucs:
            m = get_burst_date(mtg,uc).month
            histo_date[order][m] += 1
            for i, auc in enumerate(get_ancestors_within_cycle(mtg,uc)):
                m = get_burst_date(mtg,auc).month
                histo_date[order-i-1][m] += 1

        #plot_histo([Month[m] for m in histo_date[0].keys()],list(reversed([hd.values() for hd in histo_date])),'Burst date of gu of order '+str(order))
        plot_histo([Month[m] for m in histo_date[0].keys()],list(reversed([hd.values() for hd in histo_date])),'Burst date of gu of order '+str(order))


def __strip_histo(histo):
    # We remove the first and last part of the histo if it is equal to 0.
    keyvalues = histo.keys()
    for d in keyvalues:
        if histo[d] == 0:
            del histo[d]
        else : break
    if len(histo) > 0:
        for d in reversed(keyvalues):
            if histo[d] == 0:
                del histo[d]
            else : break

def burst_date_cycle_distribution(mtg, ucs, strip = True):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in Month.items()])
    from collections import OrderedDict
    histo_date = OrderedDict([(i,0) for i in xrange(3,6)])
    for uc in ucs:
        if has_burst_date(mtg,uc):
            c = get_cycle(get_burst_date(mtg,uc))
            histo_date[c] += 1
    if strip : __strip_histo(histo_date)
    return histo_date

def date_month_distribution(mtg, ucs, strip = True, date_accessor = get_burst_date):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in Month.items()])
    from collections import OrderedDict
    daterange = monthdate_range(cycle_end(3),cycle_begin(6))
    histo_date = OrderedDict([(d,0) for d in daterange])
    for uc in ucs:
        try:
            d = date_accessor(mtg,uc)
            m = d.month
            y = d.year
            histo_date[(m,y)] += 1
        except : pass
    if strip : __strip_histo(histo_date)
    return histo_date

#@use_global_mtg
def estimate_burst_date_distribution(mtgs = None, variety = 'cogshall', reference = True, exclude = None, consider = None):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in Month.items()])
    histo_date = []
    if type(mtgs) == MTG: mtgs = [mtgs] 
    for mtg in mtgs:
        ucs =  get_all_gus_of_variety(mtg, None, variety)
        if exclude: ucs = [uc for uc in ucs if not exclude(mtg,uc)]
        if consider: ucs = [uc for uc in ucs if consider(mtg,uc)]
        histo_date.append(date_month_distribution(mtg, ucs, False ))
    if reference:
        currentmtgstyle = __MtgStyle
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()
        ucs =  get_all_gus_of_variety(mtg, None, variety)
        if exclude: ucs = [uc for uc in ucs if not exclude(uc)]
        if consider: ucs = [uc for uc in ucs if consider(uc)]
        ref_histo_date = date_month_distribution(mtg, ucs, False )
        setMtgStyle(currentmtgstyle)

        return [Month[m]+'-'+str(y) for m,y in histo_date[0].keys()],[h.values() for h in histo_date], ref_histo_date.values()
    else:
        return [Month[m]+'-'+str(y) for m,y in histo_date[0].keys()],[h.values() for h in histo_date]


def estimate_bloom_date_distribution(mtgs = None, variety = 'cogshall', reference = True):
    from vplants.mangosim.util_date import Month
    Month = dict([(i,v) for v,i in Month.items()])
    histo_date = []
    if type(mtgs) == MTG: mtgs = [mtgs] 
    for mtg in mtgs:
        histo_date.append(date_month_distribution(mtg, get_all_inflo_of_variety(mtg, None, variety), False, get_bloom_date ))
    if reference:
        currentmtgstyle = __MtgStyle
        setMtgStyle(eMeasuredMtg)
        mtg = get_mtg()
        ref_histo_date = date_month_distribution(mtg, get_all_inflo_of_variety(mtg, None, variety), False, get_bloom_date )
        setMtgStyle(currentmtgstyle)

        return [Month[m]+'-'+str(y) for m,y in histo_date[0].keys()],[h.values() for h in histo_date], ref_histo_date.values()
    else:
        return [Month[m]+'-'+str(y) for m,y in histo_date[0].keys()],[h.values() for h in histo_date]

if __name__ == '__main__' :
    months,values = estimate_bloom_date_distribution([get_mtg()], reference = False)
    from vplants.mangosim.glm_simulation.plot_distribution import plot_histo
    plot_histo(months,values,'Distribution of bloom date of inflorescences')
    # check_cycle_and_burst_date_coherence()
    # check_terminal3_producing_at_cycle5()
    # check_terminal_flowering_has_no_apical()
    # check_if_has_lateral_has_apical()
    # check_apical_ratio_in_first_layer()
    pass



