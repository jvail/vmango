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
    dict_gu_prop["GU"] = gu
    # tree name
    dict_gu_prop["Tree"] = get_tree_name(mtg,get_tree_of_gu(mtg,gu))
    dict_gu_prop["Code"] = mtg.property('code')[gu]
    dict_gu_prop["Cycle"] = get_unit_cycle(mtg,gu)


def dev_variables_from_children(mtg, gu, children):
    import collections
    Burst = int(len(children) > 0)
    if not Burst:
        Nb_Children = NotRelevant
        Apical_Child  = NotRelevant
        Lateral_Children = NotRelevant
        Nb_Lateral_Children = NotRelevant
        Burst_Date_Children = NotRelevant
        Burst_Delta_Date_Children = NotRelevant
    else:
        nb_apical_children = len([c for c in children if is_apical(mtg,c)])
        assert nb_apical_children in [0,1]
        lateral_children = [c for c in children if is_lateral(mtg,c)]

        Nb_Children = len(children)
        Apical_Child = int(nb_apical_children > 0)
        Lateral_Children = int(len(lateral_children) > 0)
        Nb_Lateral_Children = len(lateral_children)

        dates_daugther = dict(collections.Counter([get_burst_date(mtg,child) for child in children if has_burst_date(mtg,child)]))
        mostFrequentDate = dates_daugther.items()[0][0]
        cycle_diff = get_vegetative_cycle(mostFrequentDate)-get_unit_cycle(mtg,gu)

        if not  cycle_diff in [0,1,2]:
            raise ValueError("Cycle difference between a mother and its children is not in [0,1,2]", cycle_diff)
        Burst_Date_Children = mostFrequentDate.month + 100*cycle_diff
        if get_unit_cycle(mtg,gu) == 3:
            Burst_Delta_Date_Children = month_difference(mostFrequentDate,vegetative_cycle_end(3))
        else:
            Burst_Delta_Date_Children = month_difference(mostFrequentDate, get_burst_date(mtg,gu))

    return Burst, Nb_Children, Apical_Child, Lateral_Children, Nb_Lateral_Children, Burst_Date_Children, Burst_Delta_Date_Children

def vegetative_dev_variables(mtg, gu, cycle = None):
    if cycle is None:
        veg_children = vegetative_children(mtg, gu, purevegetative=True)
        veg_children = [child for child in veg_children if get_unit_cycle(mtg,child) > 3]
    else:
        veg_children = vegetative_children_at_cycle(mtg,gu,cycle, purevegetative=True)
    return dev_variables_from_children(mtg, gu, veg_children)


def add_vegetative_dev_variables(dict_gu_prop, mtg, gu, cycle = None):
    Vegetative_Burst, Nb_Children, Apical_GU_Child, Lateral_GU_Children, Nb_Lateral_GU_Children, Burst_Date_GU_Children, Burst_Delta_Date_GU_Children = vegetative_dev_variables(mtg, gu, cycle)
    dict_gu_prop["Vegetative_Burst"] = Vegetative_Burst
    dict_gu_prop["Nb_GU_Children"] = Nb_Children
    dict_gu_prop["Has_Apical_GU_Child"] = Apical_GU_Child
    dict_gu_prop["Has_Lateral_GU_Children"] = Lateral_GU_Children
    dict_gu_prop["Nb_Lateral_GU_Children"] = Nb_Lateral_GU_Children 
    dict_gu_prop["Burst_Date_GU_Children"] = Burst_Date_GU_Children
    dict_gu_prop["Burst_Delta_Date_GU_Children"] = Burst_Delta_Date_GU_Children

def mixedinflo_dev_variables(mtg, gu, cycle = None):
    if cycle is None:
        mi_children = mixed_inflorescence_children(mtg,gu)
        mi_children = [child for child in mi_children if get_unit_cycle(mtg,child) > 3]
    else:
        mi_children = mixed_inflorescence_children_at_cycle(mtg,gu,cycle)
    return dev_variables_from_children(mtg, gu, mi_children)


def add_mixedinflo_dev_variables(dict_gu_prop, mtg, gu, cycle = None):
    MI_Burst, Nb_MI_Children, Apical_MI_Child, Lateral_MI_Children, Nb_Lateral_MI_Children, Burst_Date_MI_Children, Burst_Delta_Date_MI_Children = mixedinflo_dev_variables(mtg, gu, cycle)
    dict_gu_prop["MixedInflo_Burst"] = MI_Burst
    dict_gu_prop["Nb_MI_Children"] = Nb_MI_Children
    dict_gu_prop["Has_Apical_MI_Child"] = Apical_MI_Child
    dict_gu_prop["Has_Lateral_MI_Children"] = Lateral_MI_Children
    dict_gu_prop["Nb_Lateral_MI_Children"] = Nb_Lateral_MI_Children 
    dict_gu_prop["Burst_Date_MI_Children"] = Burst_Date_MI_Children
    dict_gu_prop["Burst_Delta_Date_MI_Children"] = Burst_Delta_Date_MI_Children

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
      Fruiting, Nb_Fruits, Mean_Fruit_Weight, Harvest_Week = NotRelevant, NotRelevant, NotRelevant, NotRelevant
    else : 
      # The inflorescence children of the gu
      if not cycle is None:
          inflorescence_child = inflorescence_children_at_cycle(mtg,gu,cycle)

      Flowering = int(len(inflorescence_child) > 0)

      if not Flowering : 
        Nb_Inflorescence = NotRelevant
        Flowering_Week = NotRelevant
        Fruiting, Nb_Fruits, Mean_Fruit_Weight, Harvest_Week = NotRelevant, NotRelevant, NotRelevant, NotRelevant
      else : 
        # A unique entity in the MTG represent all the inflorescence children of a given gu
        #if len(inflorescence_child) != 1: print len(inflorescence_child)
        inflo = inflorescence_child[0]
        icycle = cycle if not cycle is None else get_unit_cycle(mtg,inflo)
            # The number of inflorescence apical and lateral are stored into the two properties
        Nb_Inflorescence = [nb_of_inflorescences(mtg, infloi) for infloi in inflorescence_child]
        Nb_Inflorescence = sum([nb for nb in Nb_Inflorescence if nb > 0])
        if Nb_Inflorescence == 0:
            Nb_Inflorescence = NotAvailable

        # date of full bloom is given by property 'flowering'
        date_flo = get_bloom_dates(mtg,inflo)
        # We find the week of bloom using the predefined calendar
        Flowering_Week = get_bloom_week(date_flo[0], icycle) if not date_flo is None else NotAvailable
        
        nbfruits = get_nb_fruits(mtg, inflo)
        Fruiting, Nb_Fruits = int(nbfruits > 0), nbfruits 

        if nbfruits == 0:
            Mean_Fruit_Weight, Harvest_Week = NotRelevant, NotRelevant
        else:
            Mean_Fruit_Weight = get_fruits_weight(mtg, inflo, NotAvailable)
            if Mean_Fruit_Weight != NotAvailable : Mean_Fruit_Weight /= Nb_Fruits
            date_harv = get_fruits_harvest_date(mtg, inflo)
            Harvest_Week = get_harvest_week(date_harv) if not date_harv is None else NotAvailable
        
    return is_terminal, Flowering, Nb_Inflorescence, Flowering_Week, Fruiting, Nb_Fruits, Mean_Fruit_Weight, Harvest_Week

def add_flowering_dev_variables(dict_gu_prop, mtg, gu, cycle):
    is_terminal, Flowering, Nb_Inflorescences, Flowering_Week, Fruiting, Nb_Fruits, Mean_Fruit_Weight, Harvest_Week = flowering_dev_variables(mtg, gu, cycle)
    dict_gu_prop["is_terminal"] = is_terminal
    dict_gu_prop["Flowering"] = Flowering
    dict_gu_prop["Nb_Inflorescences"] = Nb_Inflorescences 
    dict_gu_prop["Flowering_Week"] = Flowering_Week
    dict_gu_prop["Fruiting"] = Fruiting
    dict_gu_prop["Nb_Fruits"] = Nb_Fruits
    dict_gu_prop["Fruit_Weight"] = Mean_Fruit_Weight
    dict_gu_prop["Harvest_Week"] = Harvest_Week


def explicative_variables_inside(mtg, gu):
    # get position feature (as mother)
    Position_A   = get_position_gu(mtg,gu)
    # get burst date feature (as mother)
    bdate = get_burst_date(mtg,gu)
    Burst_Month = bdate.month

    # get nature and position of ancestor
    ancestor = get_ancestor_of_previous_cycle(mtg, gu)
    if ancestor:
        # Fred Note: Position value has been changed. 1 for position means Apical
        Nature_Ancestor_F   = get_nature_gu(mtg,ancestor) #  0 if get_nature_gu(mtg,ancestor) == 0 else 1
        Position_Ancestor_A = get_position_gu(mtg,ancestor)
    else:
        Nature_Ancestor_F   = NotAvailable
        Position_Ancestor_A = NotAvailable

    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_gu(mtg,gu))

    Mixed_Inflo = 1 if is_gu_mixed_inflorescence(mtg, gu) else 0

    Reiteration = 1 if is_reiteration(mtg, gu) else 0

    return Position_A, Burst_Month, Nature_Ancestor_F, Position_Ancestor_A, Tree_Fruit_Load, Mixed_Inflo, Reiteration


def add_explicative_variables_inside(dict_gu_prop, mtg, gu):
    Position_A, Burst_Month, Nature_Ancestor_F, Position_Ancestor_A, Tree_Fruit_Load, Mixed_Inflo, Reiteration = explicative_variables_inside(mtg, gu)
    dict_gu_prop["Position_A"] = Position_A
    dict_gu_prop["Burst_Month"] = Burst_Month
    dict_gu_prop["Nature_Ancestor_F"] = Nature_Ancestor_F
    dict_gu_prop["Position_Ancestor_A"] = Position_Ancestor_A
    dict_gu_prop["Tree_Fruit_Load"] = Tree_Fruit_Load
    dict_gu_prop["Mixed_Inflo"] = Mixed_Inflo
    dict_gu_prop["Reiteration"] = Reiteration

def explicative_variables_transition(mtg, gu):
    # get position feature (as mother)
    Position_A   = get_position_gu(mtg,gu)
    Nature_F     = get_nature_gu(mtg,gu) # 0 if get_nature_gu(mtg,gu) == 0 else 1

    # get burst date feature (as mother)
    if get_unit_cycle(mtg,gu) == 3:
        Burst_Month = NotAvailable
    else:
        Burst_Month =  get_burst_date(mtg,gu).month

    # get loaded feature
    Tree_Fruit_Load = load_state(mtg, get_tree_of_gu(mtg,gu))

    Mixed_Inflo = 1 if is_gu_mixed_inflorescence(mtg, gu) else 0

    Reiteration = 1 if is_reiteration(mtg, gu) else 0

    return Position_A, Nature_F, Burst_Month, Tree_Fruit_Load, Mixed_Inflo, Reiteration


def add_explicative_variables_transition(dict_gu_prop, mtg, gu):
    Position_A, Nature_F, Burst_Month, Tree_Fruit_Load, Mixed_Inflo, Reiteration = explicative_variables_transition(mtg, gu)
    dict_gu_prop["Position_A"] = Position_A
    dict_gu_prop["Nature_F"] = Nature_F 
    dict_gu_prop["Burst_Month"] = Burst_Month
    dict_gu_prop["Tree_Fruit_Load"] = Tree_Fruit_Load
    dict_gu_prop["Mixed_Inflo"] = Mixed_Inflo
    dict_gu_prop["Reiteration"] = Reiteration

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

    if type(cycle) == int:
        gus_cycle_variety = [(cycle, get_all_gus_of_cycle(mtg, cycle, loaded, variety))]
    else:
        gus_cycle_variety = []
        for c in cycle: gus_cycle_variety.append( (c, get_all_gus_of_cycle(mtg, c, loaded, variety)))

      

    for cycle, gus in gus_cycle_variety:
        gus = filter_reiterations_and_descendants(mtg, gus)
        for gu in gus:
            if not is_reiteration(mtg,gu):
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
    add_mixedinflo_dev_variables(dict_gu, mtg, gu, None)
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
  assert loaded in [None, eLoaded, eNotLoaded]
  gus_cycle_variety = get_all_gus_of_variety(mtg, loaded=loaded, variety=variety)

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
    from pandas import DataFrame
    from os.path import join, exists 
    if not exists(outputpath): 
        from os import makedirs
        makedirs(outputpath)

    loaded = True
    table_within_cycle_for_glm_04 = DataFrame( get_table_within_cycle_for_glm(mtg, cycle=4, loaded=loaded, variety=variety) )
    table_within_cycle_for_glm_05 = DataFrame( get_table_within_cycle_for_glm(mtg, cycle=5, loaded=loaded, variety=variety) )
    table_within_cycle_for_glm_0405 = DataFrame( get_table_within_cycle_for_glm(mtg, cycle=[4,5], loaded=loaded, variety=variety) )
    column_names = list( table_within_cycle_for_glm_04.columns )
    #
    table_within_cycle_for_glm_04.to_csv(join(outputpath,"table_within_cycle_04.csv"),header=column_names, index=False)
    table_within_cycle_for_glm_05.to_csv(join(outputpath,"table_within_cycle_05.csv"),header=column_names, index=False)
    table_within_cycle_for_glm_0405.to_csv(join(outputpath,"table_within_cycle_0405.csv"),header=column_names, index=False)

    table_between_cycle_for_glm_03to04 = DataFrame( get_table_between_cycle_for_glm(mtg, cycle=3, loaded=loaded, variety=variety) )
    table_between_cycle_for_glm_04to05 = DataFrame( get_table_between_cycle_for_glm(mtg, cycle=4, loaded=loaded, variety=variety) )
    # convert data frame in csv table
    column_names = list( table_between_cycle_for_glm_03to04.columns )
    table_between_cycle_for_glm_03to04.to_csv(join( outputpath, "table_between_cycle_03to0405.csv"),header=column_names, index=False)
    table_between_cycle_for_glm_04to05.to_csv(join( outputpath, "table_between_cycle_04to05.csv"),header=column_names, index=False)


    table_for_null_model = DataFrame( get_table_for_null_model(mtg, loaded=loaded, variety=variety) )
    column_names = list( table_for_null_model.columns )
    table_for_null_model.to_csv(join( outputpath, "table_for_null_model.csv"),header=column_names, index=False)

def main():
    mtg = get_mtg()   
    export_tables(mtg, 'cogshall', join(share_dir, 'glm_estimate_input','cogshall'))

if __name__ == '__main__' :
    main()

