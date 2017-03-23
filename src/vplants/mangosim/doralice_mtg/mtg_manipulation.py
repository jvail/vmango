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
FruitWeightPropertyName = 'wgt_fr'
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
    global __MtgStyle, BurstDatePropertyName, BloomPropertyName, NbFruitPropertyName, CyclePropertyName, VarietyPropertyName, TreeNamePropertyName, MixedInfloPropertyName, LoadingPropertyName
    global LoadedValue, NotLoadedValue, InfloLabel, FruitLabel
    global TreeScale, GUScale
    if __MtgStyle != style:
        __MtgStyle = style
    if __MtgStyle == eMeasuredMtg:
        BurstDatePropertyName = 'date_burst'
        BloomPropertyName = 'flowering'
        NbFruitPropertyName = 'nb_fr'
        FruitWeightPropertyName = 'wgt_fr'
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
        NbFruitPropertyName = 'nb_fruits'
        FruitWeightPropertyName = 'fruit_weight'
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

def getMtgStyle(): return __MtgStyle

def use_global_mtg(f):
    def simplified_func(*args, **kwds):
        if not 'mtg' in kwds and len(args) == 0 or type(args[0]) != MTG:
            return f(get_mtg(),*args,**kwds)
        else:
            return f(*args,**kwds)
    import inspect
    args, varargs, keywords = inspect.getargs(f.func_code)
    if varargs: args.append('*'+varargs)
    if keywords: args.append('**'+keywords)
    doc = f.func_name+'('+','.join(args)+')'
    if f.__doc__:
        doc += '\n'+f.__doc__
    simplified_func.__doc__ = doc
    simplified_func.original = f
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
def get_nb_fruits(mtg, inflo, default=0):
    return mtg.property(NbFruitPropertyName).get(inflo,default)

def get_fruits_weight(mtg, inflo, default = None):
    return mtg.property(FruitWeightPropertyName).get(inflo,default)

@use_global_mtg
def is_gu_dead_before_cycle_end(mtg, gu, cycle):
  """Return a boolean that indicates if a unit growth is still dead at the end of a cycle. """
  if gu not in mtg.property('dead') : # not dead
    return False
  else : 
    # We check if the death is in the cycle
    date_dead = mtg.property('dead')[gu]
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
    return not (mtg.label(unit) in [InfloLabel,FruitLabel])

@use_global_mtg
def is_gu_flowering(mtg, gu, inflocycle = None):
    for ch in mtg.children(gu):
        if is_inflorescence(mtg, ch) or is_gu_mixed_inflorescence(mtg,ch):
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
def nb_of_inflorescences(mtg, gu):
    nbinflo = 0
    if is_gu(mtg, gu):
        for ch in mtg.children(gu):
            if is_inflorescence(mtg, ch):
                nbinflo += nb_of_inflorescences(mtg, ch)
    elif is_inflorescence(mtg, gu):
        nbinflo = mtg.property('nb_inflo_t').get(gu,0) + mtg.property('nb_inflo_l').get(gu,0)
    return nbinflo

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
def is_terminal(mtg, gu):
    return is_terminal_at_cycle(mtg, gu, get_unit_cycle(mtg,gu))
 
@use_global_mtg
def is_reiteration(mtg, gu):
    if not is_gu(mtg,gu): return False
    cycle = get_unit_cycle(mtg,gu)
    parent = mtg.parent(gu)
    if get_unit_cycle(parent) == cycle: return False
    for child in  mtg.children(parent):
        if is_gu(mtg,child) and get_unit_cycle(mtg,child) < cycle:
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
def vegetative_children(mtg, gu, purevegetative = False):
    return [child for child in mtg.children(gu) if is_gu(mtg,child) and (not purevegetative or not is_gu_mixed_inflorescence(mtg, child))]

@use_global_mtg
def vegetative_children_at_cycle(mtg, gu, cycle, purevegetative = False):
    return [child for child in mtg.children(gu) if is_gu(mtg,child) and get_unit_cycle(mtg,child) == cycle and (not purevegetative or not is_gu_mixed_inflorescence(mtg, child))]

@use_global_mtg
def inflorescence_children(mtg, gu):
    return [child for child in mtg.children(gu) if is_inflorescence(mtg, child)]

@use_global_mtg
def inflorescence_children_at_cycle(mtg, gu, cycle):
    return [child for child in mtg.children(gu) if is_inflorescence(mtg, child) and get_unit_cycle(mtg,child) == cycle]

@use_global_mtg
def mixed_inflorescence_children(mtg, gu):
    return [child for child in mtg.children(gu) if is_gu_mixed_inflorescence(mtg, child)]

@use_global_mtg
def mixed_inflorescence_children_at_cycle(mtg, gu, cycle):
    return [child for child in mtg.children(gu) if is_gu_mixed_inflorescence(mtg, child) and get_unit_cycle(mtg,child) == cycle]



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
      if variety == "all" or variety is None: return get_all_trees(mtg)
      else: return [i for i,v in mtg.property(VarietyPropertyName).items() if v == variety]
  else : 
      load = mtg.property(LoadingPropertyName)
      loaded_propvalue = LoadedValue if loaded == eLoaded else NotLoadedValue
      if variety == "all" or variety is None: 
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
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  assert loaded in [None, eLoaded, eNotLoaded]
  gus = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
        gus += get_all_gus_of_tree(mtg, tree)
  return gus

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
def get_all_inflo_of_tree(mtg, tree):
  """ 
  Parameters : 
    tree: the tree id.
  """
  return [i for i in mtg.components_at_scale(tree,scale=GUScale) if is_inflorescence(mtg,i)]

@use_global_mtg
def get_all_inflo_of_tree_at_cycle(mtg, tree, cycle):
  """ 
  Parameters : 
    tree: the tree id.
  """
  return [i for i in mtg.components_at_scale(tree,scale=GUScale) if is_inflorescence(mtg,i) and in_flowering_cycle(get_bloom_date(mtg,i),cycle) ]


@use_global_mtg
def get_all_inflo_of_variety(mtg, loaded = None, variety = "cogshall"):
  """ 
  Parameters : 
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  inflos = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
        inflos += get_all_inflo_of_tree(mtg, tree)
  return inflos



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


@use_global_mtg
def filter_reiterations_and_descendants(mtg, units):
    reiterations = {}
    sunit = set(units)
    for gu in units:
        if gu in reiterations: pass
        elif is_reiteration(mtg, gu):
            reiterations[gu] = True
        else:
            ancestors = [gu]
            reit = False
            while True:
                parent = mtg.parent(ancestors[-1])
                if not parent in units: break
                elif parent in reiterations:
                        reit = reiterations[parent]
                        break
                else:
                    ancestors.append(parent)
                    if is_reiteration(mtg, parent):
                        reit = True
                        break
            for a in ancestors : reiterations[a] = reit
    return [gu for gu,reit in reiterations.items() if reit is False]



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
    apical_children = []
    loaded = True
    for cycle in range(3,6):
        lflowering_with_apical = []
        lapical_children = []
        for gu in get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety):
            has_inflo, has_apical = False, False
            capical = []
            for ch in mtg.children(gu):
                if is_inflorescence(mtg, ch):
                    has_inflo = True
                elif is_apical(mtg, ch) and is_gu(mtg,ch):
                    has_apical = True
                    capical.append(ch)
            if has_inflo and has_apical:
                lflowering_with_apical.append(gu)
                if len(capical) == 1:  lapical_children += capical
                else: lapical_children.append(capical)
        flowering_with_apical += lflowering_with_apical
        apical_children += lapical_children
        print ("In cycle "+str(cycle)+', there is '+str(len(get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety)))+' terminal units.')
        print "It exists",len(lflowering_with_apical),"terminal gu with both inflorescence and apical vegetative children :",lflowering_with_apical
        mixed_inflo = filter(is_gu_mixed_inflorescence, lapical_children)
        print "The number of gu marked as mixed inflo from this set is",len(mixed_inflo),':',mixed_inflo
        for gu in lapical_children:
            if not is_gu_mixed_inflorescence(mtg,gu):
                par = mtg.parent(gu)
                print(par,mtg.property('code')[par], [mtg.property('code')[c] for c in mtg.children(par) if not is_inflorescence(mtg,c)], [mtg.property('code')[c] for c in mtg.children(par) if is_inflorescence(mtg,c)])

    nbterminalgus = sum([len(get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety)) for cycle in xrange(3,6)])
    print "In total, It exists",len(flowering_with_apical),"terminal gu with both inflorescence and apical vegetative children :",flowering_with_apical
    print "It represents",100.*len(flowering_with_apical)/nbterminalgus,"% of the terminal gus."
    print "The apical children are :",apical_children
    mixed_inflo = filter(is_gu_mixed_inflorescence, apical_children)
    print "The number of gu marked as mixed inflo from this set is",len(mixed_inflo),':',mixed_inflo
    print "The others are",list(set(apical_children) - set(mixed_inflo))

@use_global_mtg
def check_terminal_non_flowering_has_no_apical(mtg, variety = 'cogshall'):
    nflowering_without_apical = []
    loaded = True
    for cycle in range(3,5):
        lnflowering_without_apical = []
        lapical_children = []
        for gu in get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety):
            has_inflo, has_apical = False, False
            for ch in mtg.children(gu):
                if is_inflorescence(mtg, ch):
                    has_inflo = True
                elif is_apical(mtg, ch) :
                    has_apical = True
            if len(mtg.children(gu)) > 0 and not has_inflo and not has_apical:
                lnflowering_without_apical.append(gu)
        nflowering_without_apical += lnflowering_without_apical
        print ("In cycle "+str(cycle)+', there is '+str(len(get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety)))+' terminal units.')
        print "It exists",len(lnflowering_without_apical),"terminal gu without both inflorescence and apical vegetative children :"
        for gu in lnflowering_without_apical:
            print(gu,is_gu_mixed_inflorescence(gu),mtg.property('code')[gu], [get_unit_cycle(mtg,c) for c in mtg.children(gu) if not is_gu_mixed_inflorescence(mtg,c)], [get_unit_cycle(mtg,c) for c in mtg.children(gu) if is_gu_mixed_inflorescence(mtg,c)])

    nbterminalgus = sum([len(get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety)) for cycle in xrange(3,6)])
    print "In total, It exists",len(nflowering_without_apical),"terminal gu without both inflorescence and apical vegetative children :",nflowering_without_apical
    print "It represents",100.*len(nflowering_without_apical)/nbterminalgus,"% of the terminal gus."

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
def check_if_within_has_lateral_has_apical(mtg, variety = 'cogshall'):
    import numpy as np
    children_with_no_apical = []
    for gu in get_all_gus_of_variety(mtg, None, variety):
        if not is_terminal(mtg, gu):
            children = vegetative_children(mtg, gu)
            has_apical, nb_children = False, len(children)
            for ch in children:
                if is_apical(mtg, ch):
                        has_apical = True
            if nb_children > 0 and not has_apical:
                    children_with_no_apical.append(gu)

    print "It exists",len(children_with_no_apical)," gu with only vegetative lateral children and no apical and no inflorescences:",children_with_no_apical
    print "It represents",100.*len(children_with_no_apical)/len(get_all_gus_of_variety(mtg, None, variety)),"% of the gus."
    cycles = map(get_unit_cycle, children_with_no_apical)
    print "Cycle:", cycles
    print 'Hist cycle:', np.histogram(cycles, range(3,7))
    ranks = map(rank_within_cycle, children_with_no_apical)
    print "Rank:", ranks
    print 'Hist rank:', np.histogram(ranks, range(5))

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
            apstrength = tuple(reversed([is_apical(mtg, uc)]+[is_apical(auc) for auc in ancestors]))
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

@use_global_mtg
def check_produce_within_and_next_cycle(mtg, variety = 'cogshall'):
    import numpy as np
    within_and_next = {}
    for gu in get_all_gus_of_variety(mtg, None, variety):
        gucycle = get_unit_cycle(mtg,gu)
        whithinc, nextc = False, False
        reit = []
        within = []
        for child in mtg.children(gu):
            if is_gu(mtg,child) :
                chcycle = get_unit_cycle(mtg,child)
                assert chcycle >= gucycle 
                if chcycle == gucycle: 
                    whithinc = True
                    within.append(child)
                else:
                    nextc = True
                    reit.append(child)
        if whithinc and nextc : 
            within_and_next[gu] = within, reit
    print "It exists", sum([len(reit) for w, reit in within_and_next.values()]), " gu that produce both within and in the next cycles."
    if len(within_and_next) > 0:
        print 'The reiteration parent units are', within_and_next.keys()
        code = mtg.property('code')
        pgu = lambda gu : str(get_burst_date(mtg,gu))+'('+str(get_unit_cycle(mtg,gu))+'):'+code[gu]
        for gu, children  in within_and_next.items():
            within, reit = children
            print  pgu(gu), '-->', [pgu(g) for g in reit],[pgu(g) for g in within]
        nbdescendants = { 3 : [], 4 : [], 5 : []}
        for gu, children  in within_and_next.items():
            within, reit = children
            for gur in reit:
                nbdescendants[get_unit_cycle(mtg,gur)] += [len(mtg.Descendants(gur))]
        for i in xrange(3,6):
            print np.histogram(nbdescendants[i], range(0,10))

@use_global_mtg
def check_mixed_inflo(mtg, variety = 'cogshall'):
    from vplants.mangosim.util_date import MonthName
    import numpy as np
    nbchild_mi = {}
    for gu in get_all_gus_of_variety(mtg, True, variety):
        if is_gu_mixed_inflorescence(mtg, gu):
            nbchild_mi[gu] = mtg.nb_children(gu)
    print 'Nb of mixed inflorescences :',len(nbchild_mi)

    inflos = get_all_inflo_of_variety(mtg, True, variety)
    potentialmixedinflo = [inflo for inflo in inflos if mtg.property('type_inflo').get(inflo) in ['I/M', 'IM', 'M', 'M/I']]
    print 'Nb of other potential inflos (I/M, IM, M, M/I):', len(potentialmixedinflo)
    print 'Ids : ',nbchild_mi.keys()
    print 'Histogram of mixed inflorescences children :', np.histogram(nbchild_mi.values(), range(0,10))
    forbetween = []
    for gu in nbchild_mi.keys():
        gcycle = get_unit_cycle(mtg, gu)
        for ch in mtg.children(gu):
            if gcycle < get_unit_cycle(mtg, gu):
                forbetween.append(gu)
                break

    print 'Nb of MI that has children in next cycles :', len(forbetween)
    hasflowering = []
    for gu in nbchild_mi.keys():
        for ch in mtg.children(gu):
            if is_inflorescence(mtg, gu):
                hasflowering.append(gu)
                break
    print 'Nb of MI that has inflorescence children :', len(hasflowering)

    isapical = [is_apical(mtg, gu) for gu in nbchild_mi.keys()]
    print 'Nb of apical MI  :', sum(isapical), ' from ',len(isapical)

    parents = set([])
    nbmixedinflo = {}
    for gu in nbchild_mi.keys(): parents.add(mtg.parent(gu))
    for par in parents:
        nbmixedinflo[par] = len([ch for ch in mtg.children(par) if is_gu_mixed_inflorescence(mtg, ch)])

    print 'Nb of MI in siblings  :', np.histogram(nbmixedinflo.values(), range(0,10))

    isterminal = [is_terminal(mtg, gu) for gu in parents]
    print 'Terminal parents  :', sum(isterminal), ' from ',len(parents)

    isapicalparent = [is_apical(mtg, gu) for gu in parents]
    print 'Apical parents  :', sum(isapicalparent), ' from ',len(parents)

    isloadedtree = [gu for gu in nbchild_mi.keys() if is_loaded(mtg,get_tree_of_gu(mtg, gu))]
    print 'From loaded trees  :', len(isloadedtree), ' from ',len(nbchild_mi)


    hasflowering = {}
    for par in parents:
        nbinflo = 0
        for ch in mtg.children(par):
            if is_inflorescence(mtg, ch):
                nbinflo += 1
        hasflowering[par] = nbinflo
    print 'Flowering parents  :', len([gu for gu, nbinflo in hasflowering.items() if nbinflo > 0]), ' from ',len(parents)

    print "Considering property 'nb_inflo_t' and 'nb_inflo_l'"
    print "Note that some inflorescences has no inflo (?) :", len([inflo for inflo in get_all_inflo_of_variety(mtg,True) if nb_of_inflorescences(mtg,inflo) == 0])
    hasflowering = {}
    for par in parents:
        nbinflo = 0
        for ch in mtg.children(par):
            if is_inflorescence(mtg, ch):
                nbinflo += nb_of_inflorescences(mtg, ch)
        hasflowering[par] = nbinflo
    print 'Flowering parents  :', len([gu for gu, nbinflo in hasflowering.items() if nbinflo > 0]), ' from ',len(parents)
    print 'Histogram of nb of inflo of parents :', np.histogram(hasflowering.values(), range(0,10))

    isfruiting =  [par  for par in parents if is_gu_fruiting(mtg,par)]
    print 'Fruiting parents  :', len(isfruiting), ' from ',len(parents)

    hasvegdev = {}
    for par in parents:
        nbguchild = 0
        for ch in mtg.children(par):
            if is_gu(mtg, ch) and not is_gu_mixed_inflorescence(mtg,ch):
                nbguchild +=1
        hasvegdev[par] = nbguchild
    print 'Vegetative parents  :', len([gu for gu, nbguchild in hasvegdev.items() if nbguchild > 0]), ' from ',len(parents)
    print 'Histogram of nb of gu of parents :', np.histogram(hasvegdev.values(), range(0,12))

    production = {}
    for par in parents: 
        production[par] = mtg.nb_children(par)
    print 'Producing parents  :', len([gu for gu, nbchild in production.items() if nbchild > 0]), ' from ',len(parents)
    print 'Histogram of nb of child of parents :', np.histogram(production.values(), range(0,14))

    potentialparents = [gu for gu in get_all_gus_of_variety(mtg, None, variety) if is_terminal(mtg, gu) and get_unit_cycle(mtg,gu) < 5]
    potentialapifloparents = [gu for gu in potentialparents if is_apical(mtg,gu) and is_gu_flowering(mtg, gu)]
    nbparentmiinflo = len(parents)
    print 'Parent burst rate :', nbparentmiinflo/float(len(potentialparents)),'(terminal)', nbparentmiinflo/float(len(potentialapifloparents)),'(flowering apical)'


    def monthhisto(val):
        values, ranges = np.histogram(val, range(1,14))
        ranges = list(ranges)
        return ' , '.join([str(values[ranges.index(r)]) for r in range(6,13)+range(1,6)]) + " -> "+ ' , '.join([MonthEnName[r] for r in range(6,13)+range(1,6)])


    for cycle in [4,5]:
        print "* Cycle",cycle,':'
        print 'Number of mixed inflos :', len([v for k,v in nbchild_mi.items() if get_unit_cycle(mtg,k) == cycle])
        potentialparents = [gu for gu in get_all_gus_of_variety(mtg, True, variety) if is_terminal(mtg, gu) and get_unit_cycle(mtg,gu) == cycle-1]
        potentialapifloparents = [gu for gu in potentialparents if is_apical(mtg,gu) and is_gu_flowering(mtg, gu)]
        nbparentmiinflo = len([p for p in parents if get_unit_cycle(mtg,p) == cycle-1])
        print 'Parent burst rate :', nbparentmiinflo/float(len(potentialparents)),'(terminal)', nbparentmiinflo/float(len(potentialapifloparents)),'(flowering apical)'

        print 'Histogram of children :', np.histogram([v for k,v in nbchild_mi.items() if get_unit_cycle(mtg,k) == cycle], range(0,10))
        print "Burst rate :", len([v for k,v in nbchild_mi.items() if get_unit_cycle(mtg,k) == cycle and v > 0])/float(len([v for k,v in nbchild_mi.items() if get_unit_cycle(mtg,k) == cycle]))
        print "Mean nb child :", np.mean([v for k,v in nbchild_mi.items() if get_unit_cycle(mtg,k) == cycle and v > 0])
        mi_month = []
        for gu in nbchild_mi.keys():
          if get_unit_cycle(mtg,gu) == cycle:
            if get_burst_date(mtg,gu):
                mi_month.append(get_burst_date(mtg,gu).month)
        print 'Month of MI       :', monthhisto(mi_month)
        children_month = []
        for gu in nbchild_mi.keys():
          if get_unit_cycle(mtg,gu) == cycle:
            for ch in mtg.children(gu):
                if get_burst_date(mtg,ch):
                    children_month.append(get_burst_date(mtg,ch).month)
        print 'Month of children :', monthhisto(children_month)
        children_deltamonth = []
        for gu in nbchild_mi.keys():
          if get_unit_cycle(mtg,gu) == cycle:
            gdate =  get_burst_date(mtg,gu)
            for ch in mtg.children(gu):
                if get_burst_date(mtg,ch):
                    children_deltamonth.append(month_difference(get_burst_date(mtg,ch), gdate))
        print 'Delta Month of children :', np.histogram(children_deltamonth, range(1,8))


@use_global_mtg
def check_inflo(mtg, variety = 'cogshall'):
    import numpy as np
    inflo = []
    code = mtg.property('code')
    for gu in get_all_inflo_of_variety(mtg, None, variety):
        if is_inflorescence(mtg, gu):
            print gu, code[gu], code[mtg.parent(gu)]


@use_global_mtg
def check_fruit_production(mtg, variety = 'cogshall'):
    trees = [t for t in get_all_trees_of_variety(mtg,variety) if is_loaded(mtg,t)]
    wgt_fr = mtg.property('wgt_fr')
    fruits = [(vid,nb,wgt_fr[vid]) for vid, nb in mtg.property('nb_fr').items() if get_tree_of_gu(mtg,vid)in trees and nb > 0]
    for tree in trees:
        for cycle in xrange(3,5):
            subfruitset = [(vid,nb,wgt) for vid,nb,wgt in fruits if get_tree_of_gu(mtg,vid) == tree and get_unit_cycle(mtg,vid) == cycle]
            snb = sum([nb for vid,nb,wgt in subfruitset])
            swgt = sum([wgt for vid,nb,wgt in subfruitset])

            print get_tree_name(mtg,tree),'\t:',cycle,'\t:',snb, '\t', swgt, '\t', swgt/snb if snb > 0 else 0
        print

@use_global_mtg
def check_pure_vegetative(mtg, variety = 'cogshall'):
    loaded = True
    for cycle in range(3,6):
        for gu in get_all_gus_of_cycle(mtg, cycle, loaded, variety):
            for vid in vegetative_children(mtg, gu, purevegetative=True):
                assert not is_gu_mixed_inflorescence(mtg,vid)
 
if __name__ == '__main__' :
    # check_cycle_and_burst_date_coherence()
    #check_produce_within_and_next_cycle()
    # check_terminal3_producing_at_cycle5()
    check_terminal_flowering_has_no_apical()
    #check_terminal_non_flowering_has_no_apical()
    #check_if_has_lateral_has_apical()
    #check_if_within_has_lateral_has_apical()
    #check_apical_ratio_in_first_layer()
    #check_mixed_inflo()
    #check_inflo()
    #check_fruit_production()
    check_pure_vegetative()
    pass

# check reiteration


