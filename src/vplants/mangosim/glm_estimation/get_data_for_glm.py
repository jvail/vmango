from vplants.mangosim.util_path import *
from vplants.mangosim.state import *
from vplants.mangosim.util_date import *
from vplants.mangosim.doralice_mtg.mtg_import import get_mtg
from vplants.mangosim.doralice_mtg.mtg_manipulation import *


# Fred Note:
# Date format for Burst_Date_Children
# the hundred unit represent delta of cycle
# the two other number represent the month

# Pb: most of the flowering date are NA in 2004

# Data table creation

NotRelevant = None
NotAvailable = 'NA'


def add_id_variables(dict_gu_prop, mtg, gu):
    dict_gu_prop["gu"] = gu
    # tree name
    dict_gu_prop["tree"] = get_tree_name(mtg,get_tree_of_gu(mtg,gu))
    dict_gu_prop["code"] = mtg.property('code')[gu]
    dict_gu_prop["cycle"] = get_unit_cycle(mtg,gu)


def vegetative_dev_variables(mtg, gu, cycle = None):
    import collections
    if cycle is None:
        veg_children = vegetative_children(mtg,gu)
        veg_children = [child for child in veg_children if get_unit_cycle(mtg,child) > 3]
    else:
        veg_children = vegetative_children_at_cycle(mtg,gu,cycle)
    Vegetative_Burst = int(len(veg_children) > 0)
    if not Vegetative_Burst:
        Apical_GU_Child  = NotRelevant
        Lateral_GU_Children = NotRelevant
        Nb_Lateral_GU_Children = NotRelevant
        Burst_Date_Children = NotRelevant
        Burst_Delta_Date_Children = NotRelevant
    else:
        nb_apical_veg_children = len([c for c in veg_children if is_apical(mtg,c)])
        assert nb_apical_veg_children in [0,1]
        lateral_veg_children = [c for c in veg_children if is_lateral(mtg,c)]

        Apical_GU_Child = int(nb_apical_veg_children > 0)
        Lateral_GU_Children = int(len(lateral_veg_children) > 0)
        Nb_Lateral_GU_Children = len(lateral_veg_children)

        dates_daugther = dict(collections.Counter([get_burst_date(mtg,child) for child in veg_children if has_burst_date(mtg,child)]))
        mostFrequentDate = dates_daugther.items()[0][0]
        #Burst_Date_Children = cycle*100+dateD.month
        cycle_diff = get_cycle(mostFrequentDate)-get_unit_cycle(mtg,gu)
        if not  cycle_diff in [0,1,2]:
            raise ValueError("Cycle difference between a mother and its children is not in [0,1,2]", cycle_diff)
        Burst_Date_Children = mostFrequentDate.month + 100*cycle_diff
        if get_unit_cycle(mtg,gu) == 3:
            Burst_Delta_Date_Children = month_difference(mostFrequentDate,cycle_end(3))
        else:
            Burst_Delta_Date_Children = month_difference(mostFrequentDate, get_burst_date(mtg,gu))

    return Vegetative_Burst, Apical_GU_Child, Lateral_GU_Children, Nb_Lateral_GU_Children, Burst_Date_Children, Burst_Delta_Date_Children

def add_vegetative_dev_variables(dict_gu_prop, mtg, gu, cycle = None):
    Vegetative_Burst, Apical_GU_Child, Lateral_GU_Children, Nb_Lateral_GU_Children, Burst_Date_Children, Burst_Delta_Date_Children = vegetative_dev_variables(mtg, gu, cycle)
    dict_gu_prop["Vegetative_Burst"] = Vegetative_Burst
    dict_gu_prop["Has_Apical_GU_Child"] = Apical_GU_Child
    dict_gu_prop["Has_Lateral_GU_Children"] = Lateral_GU_Children
    dict_gu_prop["Nb_Lateral_GU_Children"] = Nb_Lateral_GU_Children 
    dict_gu_prop["Burst_Date_Children"] = Burst_Date_Children
    dict_gu_prop["Burst_Delta_Date_Children"] = Burst_Delta_Date_Children


def flowering_dev_variables(mtg, gu, cycle = None):
    if cycle is None:
        inflorescence_child = inflorescence_children(mtg,gu)
        inflorescence_child = [inflo for inflo in inflorescence_child if get_unit_cycle(mtg,inflo) > 3]
        is_terminal = len(inflorescence_child) > 0
    else:
        is_terminal = int(is_terminal_at_cycle(mtg, gu, cycle)  )      
    if not is_terminal : # We consider only terminal gu can bloom
      Flowering = NotRelevant
      Nb_Inflorescence = NotRelevant
      Flowering_Week = NotRelevant
      Fruiting, Nb_Fruits = NotRelevant, NotRelevant
    else : 
      # The inflorescence children of the gu
      if not cycle is None:
          inflorescence_child = inflorescence_children_at_cycle(mtg,gu,cycle)

      Flowering = int(len(inflorescence_child) > 0)

      if not Flowering : 
        Nb_Inflorescence = NotRelevant
        Flowering_Week = NotRelevant
        Fruiting, Nb_Fruits = NotRelevant, NotRelevant
      else : 
        # A unique entity in the MTG represent all the inflorescence children of a given gu
        inflo = inflorescence_child[0]
        icycle = cycle if not cycle is None else get_unit_cycle(mtg,inflo)
        if mtg.property('nb_inflo_l').get(inflo,'') != '':
            # The number of inflorescence apical and lateral are stored into the two properties
            Nb_Inflorescence = int( mtg.property('nb_inflo_t')[inflo] ) + int( mtg.property('nb_inflo_l')[inflo] )
        else : 
            Nb_Inflorescence = NotAvailable

        # date of full bloom is given by property 'flowering'
        date_flo = get_bloom_dates(mtg,inflo)
        if date_flo is None : 
            Flowering_Week = NotAvailable
        else : 
            # We find the week of bloom using the predefined calendar
            Flowering_Week = get_bloom_week(date_flo[0], icycle)
        
        nbfruits = get_nb_fruits(mtg, inflo)
        Fruiting, Nb_Fruits = int(nbfruits > 0), nbfruits 
        
    return is_terminal, Flowering, Nb_Inflorescence, Flowering_Week, Fruiting, Nb_Fruits

def add_flowering_dev_variables(dict_gu_prop, mtg, gu, cycle):
    is_terminal, Flowering, Nb_Inflorescence, Flowering_Week, Fruiting, Nb_Fruits = flowering_dev_variables(mtg, gu, cycle)
    dict_gu_prop["is_terminal"] = is_terminal
    dict_gu_prop["Flowering"] = Flowering
    dict_gu_prop["Nb_Inflorescence"] = Nb_Inflorescence 
    dict_gu_prop["Flowering_Week"] = Flowering_Week
    dict_gu_prop["Fruiting"] = Fruiting
    dict_gu_prop["Nb_Fruits"] = Nb_Fruits

def explicative_variables_inside(mtg, gu):
    # get position feature (as mother)
    Position_A   = get_position_gu(mtg,gu)
    # get burst date feature (as mother)
    Burst_Date = get_burst_date(mtg,gu).month

    # get nature and position of ancestor
    ancestor = get_ancestor_of_previous_cycle(mtg, gu)
    if ancestor:
        # Fred Note: Position value has been changed. 1 for position means Apical
        Nature_Ancestor_F   = 0 if get_nature_gu(mtg,ancestor) == 0 else 1
        Position_Ancestor_A = get_position_gu(mtg,ancestor)
    else:
        Nature_Ancestor_F   = NotAvailable
        Position_Ancestor_A = NotAvailable

    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_gu(mtg,gu))
    return Position_A, Burst_Date, Nature_Ancestor_F, Position_Ancestor_A, Tree_Fruit_Load


def add_explicative_variables_inside(dict_gu_prop, mtg, gu):
    Position_A, Burst_Date, Nature_Ancestor_F, Position_Ancestor_A, Tree_Fruit_Load = explicative_variables_inside(mtg, gu)
    dict_gu_prop["Position_A"] = Position_A
    dict_gu_prop["Burst_Date"] = Burst_Date
    dict_gu_prop["Nature_Ancestor_F"] = Nature_Ancestor_F
    dict_gu_prop["Position_Ancestor_A"] = Position_Ancestor_A
    dict_gu_prop["Tree_Fruit_Load"] = Tree_Fruit_Load

def explicative_variables_transition(mtg, gu):
    # get position feature (as mother)
    Position_A   = get_position_gu(mtg,gu)
    Nature_F     = 0 if get_nature_gu(mtg,gu) == 0 else 1

    # get burst date feature (as mother)
    if get_unit_cycle(mtg,gu) == 3:
        Burst_Date = NotAvailable
    else:
        Burst_Date =  get_burst_date(mtg,gu).month

    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_gu(mtg,gu))
    return Position_A, Nature_F, Burst_Date, Tree_Fruit_Load


def add_explicative_variables_transition(dict_gu_prop, mtg, gu):
    Position_A, Nature_F, Burst_Date, Tree_Fruit_Load = explicative_variables_transition(mtg, gu)
    dict_gu_prop["Position_A"] = Position_A
    dict_gu_prop["Nature_F"] = Nature_F 
    dict_gu_prop["Burst_Date"] = Burst_Date
    dict_gu_prop["Tree_Fruit_Load"] = Tree_Fruit_Load

def explicative_variables_null_model(mtg, gu):
    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_gu(mtg,gu))
    return Tree_Fruit_Load


def add_explicative_variables_null_model(dict_gu_prop, mtg, gu):
    Tree_Fruit_Load = explicative_variables_null_model(mtg, gu)
    dict_gu_prop["Tree_Fruit_Load"] = Tree_Fruit_Load

def get_table_within_cycle_for_glm(mtg, cycle=4, loaded = None, variety="cogshall"):
  """ 
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True gus are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  table_within_cycle_for_glm = []

  gus_cycle_variety = get_all_gus_of_cycle(mtg, cycle, loaded, variety)

  for gu in gus_cycle_variety:
    dict_gu = {}

    add_id_variables(dict_gu, mtg, gu)
    add_vegetative_dev_variables(dict_gu, mtg, gu, cycle)
    add_flowering_dev_variables(dict_gu, mtg, gu, cycle)
    add_explicative_variables_inside(dict_gu, mtg, gu)

    # put the dictionnary on the list
    table_within_cycle_for_glm.append(dict_gu)

  return table_within_cycle_for_glm


def get_table_between_cycle_for_glm(mtg, cycle=3, loaded=None, variety="cogshall"):
  """
  Parameters :
    cycle : an integer 3 or 4
    loaded : a booleen, if True gus are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety. """
  table_between_cycle_for_glm = []
  terminal_gus = get_terminal_gus_of_variety_at_cycle(mtg, cycle, loaded, variety)
  for gu in terminal_gus:

    dict_gu = {}
    add_id_variables(dict_gu, mtg, gu)
    add_vegetative_dev_variables(dict_gu, mtg, gu, None)
    add_explicative_variables_transition(dict_gu, mtg, gu)
    # put the dictionnary on the list
    table_between_cycle_for_glm.append(dict_gu)

  return table_between_cycle_for_glm



def get_table_for_null_model(mtg, loaded=None, variety="cogshall"):
  """
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True gus are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety. """
  table_for_null_model = []
  gus_cycle_variety = get_all_gus_of_variety(mtg, loaded, variety)

  for gu in gus_cycle_variety:
    dict_gu = {}

    add_id_variables(dict_gu, mtg, gu)
    add_vegetative_dev_variables(dict_gu, mtg, gu, None)
    add_flowering_dev_variables(dict_gu, mtg, gu, None)
    add_explicative_variables_null_model(dict_gu, mtg, gu)

    # put the dictionnary on the list
    table_for_null_model.append(dict_gu)

  return table_for_null_model


def export_tables(mtg, variety, outputpath):
    from os.path import join, exists 
    if not exists(outputpath): 
        from os import makedirs
        makedirs(outputpath)

    table_within_cycle_for_glm_04 = DataFrame( get_table_within_cycle_for_glm(mtg, cycle=4, loaded=None, variety=variety) )
    table_within_cycle_for_glm_05 = DataFrame( get_table_within_cycle_for_glm(mtg, cycle=5, loaded=None, variety=variety) )
    column_names = list( table_within_cycle_for_glm_04.columns )
    #
    table_within_cycle_for_glm_04.to_csv(join(outputpath,"table_within_cycle_04.csv"),header=column_names, index=False)
    table_within_cycle_for_glm_05.to_csv(join(outputpath,"table_within_cycle_05.csv"),header=column_names, index=False)

    table_between_cycle_for_glm_03to04 = DataFrame( get_table_between_cycle_for_glm(mtg, cycle=3, loaded=None, variety=variety) )
    table_between_cycle_for_glm_04to05 = DataFrame( get_table_between_cycle_for_glm(mtg, cycle=4, loaded=None, variety=variety) )
    # convert data frame in csv table
    column_names = list( table_between_cycle_for_glm_03to04.columns )
    table_between_cycle_for_glm_03to04.to_csv(join( outputpath, "table_between_cycle_03to0405.csv"),header=column_names, index=False)
    table_between_cycle_for_glm_04to05.to_csv(join( outputpath, "table_between_cycle_04to05.csv"),header=column_names, index=False)


    table_for_null_model = DataFrame( get_table_for_null_model(mtg, loaded=None, variety=variety) )
    column_names = list( table_for_null_model.columns )
    table_for_null_model.to_csv(join( outputpath, "table_for_null_model.csv"),header=column_names, index=False)


from pandas import DataFrame, concat
if __name__ == '__main__' :
    mtg = get_mtg()   
    export_tables(mtg, 'cogshall', join(share_dir, 'glm_estimate_input','cogshall'))


