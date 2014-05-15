from vplants.mangosim.tools import *
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


def add_id_variables(dict_uc_prop, mtg, uc):
    dict_uc_prop["uc"] = uc
    # tree name
    dict_uc_prop["tree"] = get_tree_name(mtg,get_tree_of_uc(mtg,uc))
    dict_uc_prop["code"] = mtg.property('code')[uc]
    dict_uc_prop["cycle"] = get_unit_cycle(mtg,uc)


def vegetative_dev_variables(mtg, uc, cycle = None):
    import collections
    if cycle is None:
        veg_children = vegetative_children(mtg,uc)
        veg_children = [child for child in veg_children if get_unit_cycle(mtg,child) > 3]
    else:
        veg_children = vegetative_children_at_cycle(mtg,uc,cycle)
    Vegetative_Burst = int(len(veg_children) > 0)
    if not Vegetative_Burst:
        Lateral_GU_Children = NotRelevant
        Nb_Lateral_GU_Children = NotRelevant
        Burst_Date_Children = NotRelevant
        Burst_Delta_Date_Children = NotRelevant
    else:
        lateral_veg_children = [c for c in veg_children if mtg.property('edge_type')[c]=='+']
        Lateral_GU_Children = int(len(lateral_veg_children) > 0)
        Nb_Lateral_GU_Children = len(lateral_veg_children)

        date_burst = mtg.property('date_burst')
        dates_daugther = dict(collections.Counter([date_burst[child] for child in veg_children if child in date_burst]))
        mostFrequentDate = dates_daugther.items()[0][0]
        #Burst_Date_Children = cycle*100+dateD.month
        cycle_diff = get_cycle(mostFrequentDate)-get_unit_cycle(mtg,uc)
        if not  cycle_diff in [0,1,2]:
            raise ValueError("Cycle difference between a mother and its children is not in [0,1,2]", cycle_diff)
        Burst_Date_Children = mostFrequentDate.month + 100*cycle_diff
        if get_unit_cycle(mtg,uc) == 3:
            Burst_Delta_Date_Children = month_difference(mostFrequentDate,cycle_begining(3))
        else:
            Burst_Delta_Date_Children = month_difference(mostFrequentDate, date_burst[uc])

    return Vegetative_Burst, Lateral_GU_Children, Nb_Lateral_GU_Children, Burst_Date_Children, Burst_Delta_Date_Children

def add_vegetative_dev_variables(dict_uc_prop, mtg, uc, cycle = None):
    Vegetative_Burst, Lateral_GU_Children, Nb_Lateral_GU_Children, Burst_Date_Children, Burst_Delta_Date_Children = vegetative_dev_variables(mtg, uc, cycle)
    dict_uc_prop["Vegetative_Burst"] = Vegetative_Burst
    dict_uc_prop["Has_Lateral_GU_Children"] = Lateral_GU_Children
    dict_uc_prop["Nb_Lateral_GU_Children"] = Nb_Lateral_GU_Children 
    dict_uc_prop["Burst_Date_Children"] = Burst_Date_Children
    dict_uc_prop["Burst_Delta_Date_Children"] = Burst_Delta_Date_Children


def flowering_dev_variables(mtg, uc, cycle = None):
    if cycle is None:
        inflorescence_child = inflorescence_children(mtg,uc)
        inflorescence_child = [inflo for inflo in inflorescence_child if get_unit_cycle(mtg,inflo) > 3]
        is_terminal = len(inflorescence_child) > 0
    else:
        is_terminal = int(is_terminal_at_cycle(mtg, uc, cycle)  )      
    if not is_terminal : # We consider only terminal uc can bloom
      Flowering = NotRelevant
      Nb_Inflorescence = NotRelevant
      Flowering_Week = NotRelevant
    else : 
      # The inflorescence children of the uc
      if not cycle is None:
          inflorescence_child = inflorescence_children_at_cycle(mtg,uc,cycle)

      Flowering = int(len(inflorescence_child) > 0)

      if not Flowering : 
        Nb_Inflorescence = NotRelevant
        Flowering_Week = NotRelevant
      else : 
        # A unique entity in the MTG represent all the inflorescence children of a given uc
        inflo = inflorescence_child[0]
        icycle = cycle if not cycle is None else get_unit_cycle(mtg,inflo)
        if mtg.property('nb_inflo_l').get(inflo,'') != '':
            # The number of inflorescence apical and lateral are stored into the two properties
            Nb_Inflorescence = int( mtg.property('nb_inflo_t')[inflo] ) + int( mtg.property('nb_inflo_l')[inflo] )
        else : 
            Nb_Inflorescence = NotAvailable

        # date of full bloom is given by property 'flowering'
        date_flo = mtg.property('flowering').get(inflo,'')
        if date_flo == '' : 
            Flowering_Week = NotAvailable
        else : 
            # We find the week of bloom using the predefined calendar
            for j in xrange(len(date_weeks[icycle])):
                if bloom_weeks[icycle][j][0] <= date_flo[0] <= bloom_weeks[icycle][j][1]:
                    Flowering_Week = j
                    break
    return is_terminal, Flowering, Nb_Inflorescence, Flowering_Week

def add_flowering_dev_variables(dict_uc_prop, mtg, uc, cycle):
    is_terminal, Flowering, Nb_Inflorescence, Flowering_Week = flowering_dev_variables(mtg, uc, cycle)
    dict_uc_prop["is_terminal"] = is_terminal
    dict_uc_prop["Flowering"] = Flowering
    dict_uc_prop["Nb_Inflorescence"] = Nb_Inflorescence 
    dict_uc_prop["Flowering_Week"] = Flowering_Week

def explicative_variables_inside(mtg, uc):
    # get position feature (as mother)
    Position_A   = get_position_uc(mtg,uc)
    # get burst date feature (as mother)
    Burst_Date = mtg.property('date_burst')[uc].month

    # get nature and position of ancestor
    ancestor = get_ancestor(mtg, uc)
    if ancestor:
        # Fred Note: Position value has been changed. 1 for position means Apical
        Nature_Ancestor_V   = get_nature_uc(mtg,ancestor)
        Position_Ancestor_A = get_position_uc(mtg,ancestor)
    else:
        Nature_Ancestor_V   = NotAvailable
        Position_Ancestor_A = NotAvailable

    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_uc(mtg,uc))
    return Position_A, Burst_Date, Nature_Ancestor_V, Position_Ancestor_A, Tree_Fruit_Load


def add_explicative_variables_inside(dict_uc_prop, mtg, uc):
    Position_A, Burst_Date, Nature_Ancestor_V, Position_Ancestor_A, Tree_Fruit_Load = explicative_variables_inside(mtg, uc)
    dict_uc_prop["Position_A"] = Position_A
    dict_uc_prop["Burst_Date"] = Burst_Date
    dict_uc_prop["Nature_Ancestor_V"] = Nature_Ancestor_V 
    dict_uc_prop["Position_Ancestor_A"] = Position_Ancestor_A
    dict_uc_prop["Tree_Fruit_Load"] = Tree_Fruit_Load

def explicative_variables_transition(mtg, uc):
    # get position feature (as mother)
    Position_A   = get_position_uc(mtg,uc)
    Nature_V     = get_nature_uc(mtg,uc)

    # get burst date feature (as mother)
    if get_unit_cycle(mtg,uc) == 3:
        Burst_Date = NotAvailable
    else:
        Burst_Date =  mtg.property('date_burst')[uc].month

    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_uc(mtg,uc))
    return Position_A, Nature_V, Burst_Date, Tree_Fruit_Load


def add_explicative_variables_transition(dict_uc_prop, mtg, uc):
    Position_A, Nature_V, Burst_Date, Tree_Fruit_Load = explicative_variables_transition(mtg, uc)
    dict_uc_prop["Position_A"] = Position_A
    dict_uc_prop["Nature_V"] = Nature_V 
    dict_uc_prop["Burst_Date"] = Burst_Date
    dict_uc_prop["Tree_Fruit_Load"] = Tree_Fruit_Load

def explicative_variables_null_model(mtg, uc):
    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_uc(mtg,uc))
    return Tree_Fruit_Load


def add_explicative_variables_null_model(dict_uc_prop, mtg, uc):
    Tree_Fruit_Load = explicative_variables_null_model(mtg, uc)
    dict_uc_prop["Tree_Fruit_Load"] = Tree_Fruit_Load

def get_table_within_cycle_for_glm(mtg, cycle=4, loaded = None, variety="cogshall"):
  """ 
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  table_within_cycle_for_glm = []

  ucs_cycle_variety = get_all_ucs_of_cycle(mtg, cycle, loaded, variety)

  for uc in ucs_cycle_variety:
    dict_uc = {}

    add_id_variables(dict_uc, mtg, uc)
    add_vegetative_dev_variables(dict_uc, mtg, uc, cycle)
    add_flowering_dev_variables(dict_uc, mtg, uc, cycle)
    add_explicative_variables_inside(dict_uc, mtg, uc)

    # put the dictionnary on the list
    table_within_cycle_for_glm.append(dict_uc)

  return table_within_cycle_for_glm


def get_table_between_cycle_for_glm(mtg, cycle=3, loaded=None, variety="cogshall"):
  """
  Parameters :
    cycle : an integer 3 or 4
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety. """
  table_between_cycle_for_glm = []
  terminal_ucs = get_terminal_ucs_of_variety_at_cycle(mtg, cycle, loaded, variety)
  for uc in terminal_ucs:

    dict_uc = {}
    add_id_variables(dict_uc, mtg, uc)
    add_vegetative_dev_variables(dict_uc, mtg, uc, None)
    add_explicative_variables_transition(dict_uc, mtg, uc)
    # put the dictionnary on the list
    table_between_cycle_for_glm.append(dict_uc)

  return table_between_cycle_for_glm



def get_table_for_null_model(mtg, loaded=None, variety="cogshall"):
  """
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety. """
  table_for_null_model = []
  ucs_cycle_variety = get_all_ucs_of_variety(mtg, loaded, variety)

  for uc in ucs_cycle_variety:
    dict_uc = {}

    add_id_variables(dict_uc, mtg, uc)
    add_vegetative_dev_variables(dict_uc, mtg, uc, None)
    add_flowering_dev_variables(dict_uc, mtg, uc, None)
    add_explicative_variables_null_model(dict_uc, mtg, uc)

    # put the dictionnary on the list
    table_for_null_model.append(dict_uc)

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
    check_cycle_and_burst_date_coherence(mtg)
    check_terminal3_producing_at_cycle5(mtg)

    export_tables(mtg, 'cogshall', join(share_dir, 'glm_estimate_input','cogshall'))


