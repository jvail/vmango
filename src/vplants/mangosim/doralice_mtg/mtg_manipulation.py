from vplants.mangosim.tools import *
from vplants.mangosim.state import *
from vplants.mangosim.util_date import *
from openalea.mtg import MTG

# Fred Note:
# Date format for Burst_Date_Children
# the hundred unit represent delta of cycle
# the two other number represent the month

# Pb: most of the flowering date are NA in 2004


def use_global_mtg(f):
    from mtg_import import get_mtg
    def simplified_func(*args, **kwds):
        if not 'mtg' in kwds and len(args) == 0 or type(args[0]) != MTG:
            return f(get_mtg(),*args,**kwds)
        else:
            return f(*args,**kwds)
    return simplified_func

# Characterizing UCs
@use_global_mtg
def get_unit_cycle(mtg, unit):
  """Return the cycle of the uc.  """
  if mtg.label(unit) in 'MP' : return 3
  else: return mtg.property('year')[unit]

@use_global_mtg
def is_uc_dead_before_cycle_end(mtg, uc, cycle):
  """Return a boolean that indicates if a unit growth is still dead at the end of a cycle. """
  if uc not in mtg.property('dead') : # not dead
    return False
  else : 
    # We check if the death is in the cycle
    date_dead = date_from_string(mtg.property('dead')[uc])
    date_end_cycle = date(2000+cycle, 7, 15)
    return date_dead < date_end_cycle

@use_global_mtg
def is_inflorescence(mtg, unit):
    return mtg.label(unit) == 'F'

@use_global_mtg
def is_uc_inflorescent(mtg, uc, inflocycle = None):
    for ch in mtg.children(uc):
        if is_inflorescence(mtg, ch):
            if inflocycle is None or get_unit_cycle(mtg,ch) == inflocycle:
                return True
            else : return False

@use_global_mtg
def is_terminal_at_cycle(mtg, uc, cycle):
    # we consider uc borned in cycle or before and not dead before end of cycle.
    if get_unit_cycle(mtg,uc) <= cycle and not is_uc_dead_before_cycle_end(mtg,uc,cycle):
        for child in mtg.children(uc):
            if not is_inflorescence(mtg,child) and get_unit_cycle(mtg,child) <= cycle:
                return False
        # No child of uc was borned before end of cycle
        return True
    return False

@use_global_mtg
def get_nature_uc(mtg, uc):
  """ """
  return eInflorescence if is_uc_inflorescent(mtg, uc) else eVegetative

@use_global_mtg
def is_apical(mtg, uc):
    return mtg.property('edge_type')[uc] == '<'

@use_global_mtg
def is_lateral(mtg, uc):
    return mtg.property('edge_type')[uc] == '+'

@use_global_mtg
def get_position_uc(mtg, uc):
  """ """
  return eApical if is_apical(mtg, uc) else eLateral

@use_global_mtg
def get_parent(mtg, uc):
    return mtg.parent(uc)

@use_global_mtg
def get_ancestor(mtg, uc):
    uc_cyle = get_unit_cycle(mtg, uc)
    target_cycle = uc_cyle - 1
    if uc_cyle != 3: # No ancestor for unit of cycle 3
        parent_uc = mtg.parent(uc)
        while get_unit_cycle(mtg,parent_uc) ==  uc_cyle:
            parent_uc = mtg.parent(parent_uc)
        return parent_uc

@use_global_mtg
def get_nature_position_ancestor(mtg, uc):
  """ """
  ancestor = get_ancestor(mtg, uc)
  if ancestor:
    # Fred Note: Position value has been changed. 1 for position means Apical
    return get_nature_uc(mtg,ancestor), get_position_uc(mtg,ancestor)

@use_global_mtg
def get_tree_of_uc(mtg, uc):
    return mtg.complex_at_scale(uc,scale = 1)


@use_global_mtg
def vegetative_children(mtg,uc):
    return [child for child in mtg.children(uc) if not is_inflorescence(mtg,child) ]

@use_global_mtg
def vegetative_children_at_cycle(mtg,uc,cycle):
    return [child for child in mtg.children(uc) if not is_inflorescence(mtg,child) and get_unit_cycle(mtg,child) == cycle]


@use_global_mtg
def inflorescence_children(mtg,uc):
    return [child for child in mtg.children(uc) if is_inflorescence(mtg, child)]

@use_global_mtg
def inflorescence_children_at_cycle(mtg,uc,cycle):
    return [child for child in mtg.children(uc) if is_inflorescence(mtg, child) and get_unit_cycle(mtg,child) == cycle]


# Characterizing trees
@use_global_mtg
def is_loaded(mtg,tree):
    return mtg.property('fr_load')[tree] == 'C'

@use_global_mtg
def load_state(mtg,tree):
    return eLoaded if is_loaded(mtg,tree) else eNotLoaded

@use_global_mtg
def get_tree_name(mtg,tree):
    return mtg.property('code')[tree]

@use_global_mtg
def get_tree_from_name(mtg,treename):
    for tree in get_all_trees(mtg):
        if get_tree_name(mtg,tree) == treename:
            return tree

@use_global_mtg
def get_all_trees(mtg):
    return list(mtg.property('var').keys())

@use_global_mtg
def get_all_treenames(mtg):
    return map(lambda tid: get_tree_name(mtg,tid), get_all_trees(mtg))

@use_global_mtg
def get_all_trees_of_variety(mtg, variety = 'cogshall'):
    return [k for k,v in mtg.property('var').items() if v == variety]

@use_global_mtg
def get_variety(mtg, tree):
    return mtg.property('var')[tree]

@use_global_mtg
def get_all_varieties(mtg):
    return list(set(mtg.property('var').values()))

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
      else: return [i for i,v in mtg.property('var').items() if v == variety]
  else : 
      load = mtg.property('fr_load')
      loaded_propvalue = 'C' if loaded == eLoaded else 'NC'
      if variety == "all": 
            return [i for i in get_all_trees(mtg) if load[i] == loaded_propvalue]
      else: 
            return [i for i,v in mtg.property('var').items() if v == variety and load[i] == loaded_propvalue]

@use_global_mtg
def get_first_uc(mtg, tree):
    return mtg.component_roots_at_scale(tree,scale = 4)[0]

# Selection of UCS

@use_global_mtg
def get_all_ucs_of_tree(mtg, tree):
  """Return all the unit growth of a tree and for a cycle 
  Parameters : 
    tree: integer in g.property('var')
    cycle: integer 3, 4 or 5"""
  return [i for i in mtg.components_at_scale(tree,scale=4) if not is_inflorescence(mtg,i)]
 

@use_global_mtg
def get_all_ucs_of_tree_at_cycle(mtg, tree, cycle):
  """Return all the unit growth of a tree and for a cycle 
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5"""
  ucs_tree = get_all_ucs_of_tree(mtg, tree)
 
  # keep unit growth (remove inflorescence)
  ucs_tree_cycle = [uc for uc in ucs_tree if get_unit_cycle(mtg, uc) == cycle]

  # Fred Question: Pourquoi on retrie sur les date de burst?
  if cycle != 3:
    date_burst = mtg.property('date_burst')
    ucs_tree_cycle = [uc for uc in ucs_tree_cycle if in_cycle(date_burst[uc],cycle)]
  return ucs_tree_cycle

@use_global_mtg
def get_all_ucs_of_cycle(mtg, cycle = 4, loaded = None, variety = "cogshall"):
  """ 
  Parameters : 
    cycle : an integer, cycle 4 or 5
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  ucs_of_cycle = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
    ucs_of_cycle += get_all_ucs_of_tree_at_cycle(mtg, tree, cycle)
  return ucs_of_cycle

@use_global_mtg
def get_all_ucs_of_variety(mtg, loaded = None, variety = "cogshall"):
  """ 
  Parameters : 
    cycle : an integer, cycle 4 or 5
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  ucs_of_cycle = []
  trees = select_trees(mtg, loaded, variety)
  for tree in trees :
    for cycle in [3,4,5]:
        ucs_of_cycle += get_all_ucs_of_tree_at_cycle(mtg, tree, cycle)
  return ucs_of_cycle

@use_global_mtg
def get_sorted_ucs(mtg, cycles = [4,5], loaded = [eLoaded,eNotLoaded], varieties = ['cogshall']):
    """
    Return all ucs sorted according to cycles, loading and tree varieties.
    Parameters :
        cycles : a list of integer. All cycles to consider
        loaded : a list of load state. All load state to consider
        varieties : a list of varieties name. All varieties to consider.
    """
    ucs = {}
    for var in varieties:
        for load in loaded:
            for cycle in cycles:
                ucs[(cycle, load, var)] = get_all_ucs_of_cycle(mtg, cycle, load, var)
    return ucs

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
        inflos += [i for i in mtg.components_at_scale(tree,scale=4) if is_inflorescence(mtg,i)]
  return inflos



# def get_ucs_tree_cycle_in_extremity(tree, cycle):
#   """Return a list of unit growth which are in the extremity of the canopy (for a tree and for a cycle).
#   If a unit growth dead in the cycle, it is remove from the list of terminal unit growth.
#   Parameters : 
#     tree: integer giving id of one tree. To select in g.property('var').keys() for instance.
# 	cycle: integer 3, 4 or 5"""

#   extremity = []
  
#   if cycle == 4 or cycle == 5:
#     # Fred: Je ne comprends pas ce qui est fait la.
#     ucs_extremity_cycleMinus1 = get_terminal_ucs(mtg, tree,cycle-1)
#     for i1 in ucs_extremity_cycleMinus1:
#       children_extremity = [i2 for i2 in g.children(i1) if g.label(i2)!='F' and get_cycle_uc(i2)==cycle]
#       if children_extremity == [] and not is_dead_in_cycle(i1,cycle): extremity.append(i1)

#   ucs_veget_tree_cycle = get_all_ucs_of_tree_at_cycle(mtg, tree, cycle)
#   for i in ucs_veget_tree_cycle :
#     childrens_in_cycle = [child for child in g.children(i) if not g.label(child)!='F' and get_cycle_uc(child)==cycle ]
#     if childrens_in_cycle == [] : extremity.append(i)
#   return extremity



@use_global_mtg
def get_terminal_ucs_of_tree_at_cycle(mtg, tree, cycle):
  """
    Return a list of unit growth which are in the extremity of the canopy (for a tree and at end of a given cycle).
    If a unit growth dead in the cycle, it is remove from the list of terminal unit growth.
  Parameters : 
    tree: integer giving id of one tree. To select in g.property('var').keys() for instance.
    cycle: integer 3, 4 or 5"""  
  return [ uc for uc in get_all_ucs_of_tree_at_cycle(mtg, tree, cycle) if is_terminal_at_cycle(mtg, uc, cycle)]



@use_global_mtg
def get_terminal_ucs_of_variety_at_cycle(mtg, cycle=4, loaded= None, variety="cogshall"):
  """Return the list of terminal growth unit in the cycle and for the given variety.
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice or variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  terminal_ucs = []
  for tree in select_trees(mtg, loaded, variety) : 
    terminal_ucs += get_terminal_ucs_of_tree_at_cycle(mtg, tree, cycle)
  return terminal_ucs

@use_global_mtg
def get_sorted_terminal_uc(mtg, cycles = [3,4,5], loaded = [eLoaded,eNotLoaded], varieties = ['cogshall']):
    """
    Return all teminal ucs sorted according to cycles, loading and tree varieties.
    Parameters :
        cycles : a list of integer. All cycles to consider
        loaded : a list of load state. All load state to consider
        varieties : a list of varieties name. All varieties to consider.
    """
    ucs = {}
    for var in varieties:
        for load in loaded:
            for cycle in cycles:
                ucs[(cycle, load, var)] = get_terminal_ucs_of_variety_at_cycle(mtg, cycle, load, var)
    return ucs    


@use_global_mtg
def get_first_layer_ucs(mtg, tree, cycle):
  assert 3 <= cycle <= 5
  return [ uc for uc in get_all_ucs_of_tree_at_cycle(mtg, tree, cycle) if get_unit_cycle(mtg,get_parent(mtg, uc)) == cycle - 1]


#### test #####

@use_global_mtg
def check_cycle_and_burst_date_coherence(mtg, varieties = ['cogshall']):
    date_burst = mtg.property('date_burst')
    for var in varieties:
        ucs_var = []
        for tree in select_trees(mtg,variety=var):
            ucs_var += get_all_ucs_of_tree(mtg, tree)
        invalid_ucs = []
        for uc in ucs_var:
            if (uc in date_burst):
               if not in_cycle(date_burst[uc],get_unit_cycle(mtg,uc)):
                    invalid_ucs.append(uc)

        if len(invalid_ucs) > 0:
            import warnings
            warnings.warn(('Incoherent cycle and burst date for %i ucs of %s :' % (len(invalid_ucs), var)) + str(invalid_ucs))

@use_global_mtg
def check_terminal3_producing_at_cycle5(mtg, variety = 'cogshall'):
    import warnings
    terminal_ucs = get_terminal_ucs_of_variety_at_cycle(mtg, 3, None, variety)
    terminal_ucs3 = [uc for uc in terminal_ucs if get_unit_cycle(mtg,uc) == 3]
    terminal_uc3_giving_gu5 = [uc for uc in terminal_ucs3 if len(vegetative_children_at_cycle(mtg,uc,5)) > 0]
    if len(terminal_uc3_giving_gu5) > 0:
        warnings.warn('''There is %i terminal ucs of cycle 3 that produces ucs at cycle 5. The date format do not make it possible to take this into account.''' % len(terminal_uc3_giving_gu5))
    terminal_uc3_giving_inflo5 = [uc for uc in terminal_ucs3 if len(inflorescence_children_at_cycle(mtg,uc,5)) > 0]
    if len(terminal_uc3_giving_inflo5) > 0:
        warnings.warn('''There is %i terminal ucs of cycle 3 that produces inflorescences at cycle 5. The date format do not make it possible to take this into account.''' % len(terminal_uc3_giving_inflo5))


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
            units += get_first_layer_ucs(mtg, tree, cycle)
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
