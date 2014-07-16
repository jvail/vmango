from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

import pandas as pd
from numpy import cumsum
from numpy.random import binomial, poisson, uniform, randint
from datetime import datetime, date


colnames = ["is_loaded","burst_date_mother","position_mother_L","position_ancestor_L","nature_ancestor_V","nature_mother_V"]
def get_dict_from_table(table_prob, family):
  """
  Argument : 
    table_prob : a data frame from pandas which contain the parameter(s) of a law fitted by glm's method. There are variables which were use for the glm.
    family : a string, it can be "binomial", "poisson", "vglm".
  Return :  
    a dictionnary. The keys are tuples of variables'value and the values are a list of parameter(s).
  """
  name_variables = [ name for name in list(table_prob.columns) if name in colnames]
  name_probs = [name_p for name_p in list(table_prob.columns) if name_p not in colnames]
  subset_table_var = table_prob[name_variables]
  subset_table_prob = table_prob[name_probs]
  dict_table = {}
  for ind in xrange(len(table_prob)):
    key_dict = tuple(subset_table_var.ix[ind])
    dict_table[key_dict] = list(subset_table_prob.ix[ind])
  dict_table["colnames"] = name_variables
  dict_table["family"] = family
  dict_table["probs_name"] = name_probs
  return dict_table

#table_prob_glm_burst_04 = pd.read_csv(share_dir+"/model_glm/glm_complet/by_tree/B10/"+"table_prob_glm_burst_04.csv")
#dict_table_prob_glm_burst_03_04 = get_dict_from_table(table_prob_glm_burst_04, "binomial")

#table_prob_vglm_Burst_date_child_03_04 = pd.read_csv(share_dir+"/model_glm/glm_complet/by_tree/B10/"+"table_prob_vglm_Burst_date_child_03_04.csv")
#dict_table_prob_vglm_Burst_date_child_03_04 = get_dict_from_table(table_prob_vglm_Burst_date_child_03_04, "vglm")


# is_loaded = 1
# burst_date_mother = 10
# position_mother_L = 0
# position_ancestor_L = 0
# nature_ancestor_V = 0
# nature_mother_V = 1
# name_table_prob_glm = "glm_burst_04"
# name_table_prob_glm = "vglm_Date_burst_daughter_04"



def get_value_simulation_glm_distribution(name_table_prob_glm,is_loaded,burst_date_mother,position_mother_L,position_ancestor_L,nature_ancestor_V,nature_mother_V):
  """
  """
  # dict_table_prob_glm = get_dict_from_table(*table_prob_glm)
  dict_table_prob_glm = dict_table_glm[name_table_prob_glm]
  dict_variables = {"is_loaded" : is_loaded, "burst_date_mother" : burst_date_mother,"position_mother_L":position_mother_L,"position_ancestor_L": position_ancestor_L,"nature_ancestor_V":nature_ancestor_V,"nature_mother_V":nature_mother_V}
  variables_name = dict_table_prob_glm["colnames"]
  key_dict_table = tuple(dict_variables[name] for name in variables_name)
  if not key_dict_table in dict_table_prob_glm.keys() : print dict_table_prob_glm
  probs = dict_table_prob_glm[key_dict_table]
  if dict_table_prob_glm["family"]=="binomial" :
    proba = probs[0]
    value = int( binomial(1,proba,1) )
    response = True if value==1 else False
  elif dict_table_prob_glm["family"]=="poisson" :
    proba = probs[0]
    value = int( poisson(proba,1) )
    response = value +1
  else:
    cumsum_probs = list( cumsum(probs) )
    unif_value = float( uniform(0,1,1) )
    cumsum_probs.insert(0,0)
    cumsum_probs[-1] = 1
    i = 1
    while unif_value >= cumsum_probs[i] : i += 1
    response = int(dict_table_prob_glm["probs_name"][i-1])
  return response
# if cumsum_probs[i] <= unif_value < cumsum_probs[i+1] :

response_variables = {"glm_burst_03_04" : "binomial", "glm_Lateral_GU_daughter_03_04" : "binomial", "glm_no_lateral_GU_03_04" : "poisson", "vglm_Burst_date_child_03_04" : "vglm",
    "glm_burst_04" : "binomial", "glm_Lateral_GU_daughter_04" : "binomial", "glm_no_lateral_GU_04" : "poisson","vglm_Date_burst_daughter_04" : "vglm",
    "glm_Flowering_04" : "binomial", "glm_No_inflorescences_04" : "poisson", "vglm_Flowering_Date_05" : "vglm", 
    "glm_burst_04_05" : "binomial", "glm_Lateral_GU_daughter_04_05" : "binomial", "glm_no_lateral_GU_04_05" : "poisson", "vglm_Burst_date_child_04_05" : "vglm", 
    "glm_burst_05" : "binomial", "glm_Lateral_GU_daughter_05" : "binomial", "glm_no_lateral_GU_05" : "poisson", "vglm_Date_burst_daughter_05" : "vglm", 
    "glm_Flowering_05" : "binomial", "glm_No_inflorescences_05" : "poisson" }
from os.path import join
def get_dict_files_glm(GLM_SELECTED=False, is_loaded_factor=False, feature_tree="B12", key_dict=response_variables):
  """
  The aim is to get all tables of probability of variables
  Parameters : 
    GLM_SELECTED : boolean ,  
      if False, we choose a complet glm
      if True, we choose a selected glm
    is_loaded_factor : boolean , 
      if True, is_loaded is taken to a factor in glm
      if False, is_loaded is not as factor in glm ==> glm for each tree, for loaded trees or for not loaded trees
  """
  if not GLM_SELECTED : # glm complet
    path_file = join(share_dir, "model_glm", "glm_complet")
  else : # glm selected
    path_file = join(share_dir,"model_glm","glm_selected")
  if is_loaded_factor: # glm with factor is_loaded
    sub_file = "by_all_trees"
  else : # glm for each tree and for loaded and not loaded trees
    sub_file = feature_tree
  key_dict_files = key_dict.keys()
  dict_files = {}
  for key in key_dict_files:
    final_path = join(path_file , sub_file, "table_prob_"+key+".csv")
    dict_files[key]= (pd.read_csv(final_path),key_dict[key])
  return dict_files

list_glm_selected = [True, False]
list_is_loaded_factor = [True, False]

def set_dicts_files_glm():
  """
  """
  list_feature_tree = ["B10","B12","B14","F2","F6"]+["loaded","notloaded"]
  tuple_keys_dict_files_glm_0 = [(glm_selected,is_loaded_factor) for glm_selected in list_glm_selected for is_loaded_factor in list_is_loaded_factor]
  tuple_keys_dict_files_glm1 = [ (tuple1,feature_tree) for tuple1 in tuple_keys_dict_files_glm_0 for feature_tree in list_feature_tree if  not tuple1[1] ]
  tuple_keys_dict_files_glm2 = [ (tuple1,None) for tuple1 in tuple_keys_dict_files_glm_0 if tuple1[1] ]
  tuple_keys_dict_files_glm = tuple_keys_dict_files_glm1 + tuple_keys_dict_files_glm2
  dicts_files_glm = {}
  for tuple_keys in tuple_keys_dict_files_glm:
    dicts_files_glm[tuple_keys] = get_dict_files_glm(tuple_keys[0][0], tuple_keys[0][1], tuple_keys[1])
  return dicts_files_glm

dicts_files_glm = set_dicts_files_glm()

dict_table_glm = {}
def init_glm_table( GLM_SELECTED=False, is_loaded_factor=True, feature_tree=None):
   """
   Parameters : 
    GLM_SELECTED : boolean , 
      if False, we choose a complet glm
      if True, we choose a selected glm
    is_loaded_factor : boolean , 
      if True, is_loaded is taken to a factor in glm
      if False, is_loaded is not as factor in glm ==> glm for each tree, for loaded trees or for not loaded trees
    feature_tree : a string,
      it could be the name of a tree ( for example "B12", "F2"), "loaded" or "notloaded"
   """
   if is_loaded_factor : feature_tree = None
   key_dicts_files_glm = ((GLM_SELECTED, is_loaded_factor),feature_tree)
   dict_file_glm = dicts_files_glm[key_dicts_files_glm]
   global dict_table_glm
   for name, info in dict_file_glm.iteritems():
      dict_table_glm[name] = get_dict_from_table(*info)
  
#init_glm_table(dicts_files_glm, GLM_SELECTED = False, is_loaded_factor =False , feature_tree ="B12" )

def set_values_variables_2003(is_loaded=None, position_mother=None, nature_mother=None):
  """
  """
  No_lateral_next_cycle, date_child = 0, None
  Burst_next_cycle = get_value_simulation_glm_distribution("glm_burst_03_04",is_loaded,None, position_mother,None,None,nature_mother)
  if Burst_next_cycle:
    Burst_date_child = get_value_simulation_glm_distribution("vglm_Burst_date_child_03_04",is_loaded,None, position_mother,None,None,nature_mother)
    date_child = get_date_child(Burst_date_child)
    Lateral_next_cycle = get_value_simulation_glm_distribution("glm_Lateral_GU_daughter_03_04",is_loaded, None, position_mother,None,None,nature_mother)
    if Lateral_next_cycle:
      No_lateral_next_cycle = get_value_simulation_glm_distribution("glm_no_lateral_GU_03_04",is_loaded, None, position_mother,None ,None,nature_mother)
  return No_lateral_next_cycle, date_child

def set_values_variables(cycle,is_loaded, burst_date_mother, position_mother, position_ancestor, nature_ancestor, nature_mother):
  """
  """
  Nb_child_cycle, date_burst_children, No_inflo, date_inflo, No_lateral_next_cycle, date_burst_child_next, new_position_ancestor,new_nature_ancestor, new_nature_mother = None,None,0,None,0,None,position_ancestor,None,None
  Burst = get_value_simulation_glm_distribution("glm_burst_0"+str(cycle),is_loaded, burst_date_mother.month, position_mother,position_ancestor,nature_ancestor,nature_mother)
  if Burst :
    new_nature_mother = 1
    new_nature_ancestor = nature_ancestor
    Month_burst_child = get_value_simulation_glm_distribution("vglm_Date_burst_daughter_0"+str(cycle),is_loaded,burst_date_mother.month, position_mother,position_ancestor,nature_ancestor,nature_mother)
    if 6<= burst_date_mother.month <=12 : 
      if 6<= Month_burst_child <=12 :
        date_burst_children = date(burst_date_mother.year,Month_burst_child,15)
        if date_burst_children < burst_date_mother : print "error children before mother in cycle "+str(cycle)+"..."
      else : 
        date_burst_children = date(burst_date_mother.year+1,Month_burst_child,15)
        if date_burst_children < burst_date_mother : print "error children before mother in cycle "+str(cycle)+"..."
    else :
      date_burst_children = date(burst_date_mother.year,Month_burst_child,15)
      if date_burst_children < burst_date_mother : print "error children before mother in cycle "+str(cycle)+"..."
    Lateral = get_value_simulation_glm_distribution("glm_Lateral_GU_daughter_0"+str(cycle),is_loaded,burst_date_mother.month, position_mother,position_ancestor,nature_ancestor,nature_mother)
    if Lateral:
      No_lateral = get_value_simulation_glm_distribution("glm_no_lateral_GU_0"+str(cycle),is_loaded,burst_date_mother.month, position_mother,position_ancestor,nature_ancestor,nature_mother)
    else : No_lateral = 0 
    Nb_child_cycle = 1 + No_lateral
  else : 
      Flowering = get_value_simulation_glm_distribution("glm_Flowering_0"+str(cycle),is_loaded,burst_date_mother.month, position_mother,position_ancestor,nature_ancestor,nature_mother)
      if Flowering:
        new_nature_ancestor, new_nature_mother = 0, 0
        No_inflo = get_value_simulation_glm_distribution("glm_No_inflorescences_0"+str(cycle),is_loaded, burst_date_mother.month, position_mother,position_ancestor,nature_ancestor,nature_mother)
        week_inflo = get_value_simulation_glm_distribution("vglm_Flowering_Date_05",is_loaded,burst_date_mother.month,position_mother,position_ancestor,nature_ancestor,nature_mother)
        date_inflo = get_date_flo(week_inflo,cycle)
      else:
        new_nature_ancestor, new_nature_mother = 1, 1
      Burst_next_cycle = get_value_simulation_glm_distribution("glm_burst_04_05",is_loaded, burst_date_mother.month, position_mother,position_ancestor,new_nature_ancestor,new_nature_mother)
      if Burst_next_cycle:
        Month_burst_child_next_cycle = get_value_simulation_glm_distribution("vglm_Burst_date_child_04_05",is_loaded,burst_date_mother.month, position_mother,position_ancestor,new_nature_ancestor,new_nature_mother)
        if 6<= burst_date_mother.month <=12:
          if 6<= Month_burst_child_next_cycle <=12:
            date_burst_child_next = date(burst_date_mother.year+1, Month_burst_child_next_cycle,15)
            if date_burst_child_next < burst_date_mother : print "error children before mother between cycle 4 and 5 ..."
          else : 
            date_burst_child_next = date(burst_date_mother.year+2, Month_burst_child_next_cycle,15)
            if date_burst_child_next < burst_date_mother : print "error children before mother between cycle 4 and 5 ..."
        else : 
          if 6<= Month_burst_child_next_cycle <=12:
            date_burst_child_next = date(burst_date_mother.year, Month_burst_child_next_cycle,15)
            if date_burst_child_next < burst_date_mother : print "error children before mother between cycle 4 and 5 ..."
          else : 
            date_burst_child_next = date(burst_date_mother.year+1, Month_burst_child_next_cycle,15)
            if date_burst_child_next < burst_date_mother : print "error children before mother between cycle 4 and 5 ..."
        Lateral_next_cycle = get_value_simulation_glm_distribution("glm_Lateral_GU_daughter_04_05",is_loaded, burst_date_mother.month, position_mother,position_ancestor,new_nature_ancestor,new_nature_mother)
        if Lateral_next_cycle:
          No_lateral_next_cycle = get_value_simulation_glm_distribution("glm_no_lateral_GU_04_05",is_loaded, burst_date_mother.month, position_mother,position_ancestor,new_nature_ancestor,new_nature_mother)
      new_position_ancestor = position_mother
  return Nb_child_cycle, date_burst_children, No_inflo, date_inflo, No_lateral_next_cycle, date_burst_child_next, new_position_ancestor,new_nature_ancestor, new_nature_mother


date_weeks_04 = {
0 : (datetime(2004,7,1),datetime(2004,8,7)),
1 : (datetime(2004,8,8),datetime(2004,8,14)),
2 : (datetime(2004,8,15),datetime(2004,8,21)),
3 : (datetime(2004,8,22),datetime(2004,8,28)),
4 : (datetime(2004,8,29),datetime(2004,9,4)),
5 : (datetime(2004,9,5),datetime(2004,9,11)),
6 : (datetime(2004,9,12),datetime(2004,9,18)),
7 : (datetime(2004,9,19),datetime(2004,9,25)),
8 : (datetime(2004,9,26),datetime(2004,10,2)),
9 : (datetime(2004,10,3),datetime(2004,10,9)),
10 : (datetime(2004,10,10),datetime(2004,10,16)),
11 : (datetime(2004,10,17),datetime(2004,10,23)),
12 : (datetime(2004,10,24),datetime(2004,10,30))  }

date_weeks_05 = {
0 : (datetime(2005,7,1),datetime(2005,8,7)),
1 : (datetime(2005,8,8),datetime(2005,8,14)),
2 : (datetime(2005,8,15),datetime(2005,8,21)),
3 : (datetime(2005,8,22),datetime(2005,8,28)),
4 : (datetime(2005,8,29),datetime(2005,9,4)),
5 : (datetime(2005,9,5),datetime(2005,9,11)),
6 : (datetime(2005,9,12),datetime(2005,9,18)),
7 : (datetime(2005,9,19),datetime(2005,9,25)),
8 : (datetime(2005,9,26),datetime(2005,10,2)),
9 : (datetime(2005,10,3),datetime(2005,10,9)),
10 : (datetime(2005,10,10),datetime(2005,10,16)),
11 : (datetime(2005,10,17),datetime(2005,10,23)),
12 : (datetime(2005,10,24),datetime(2005,10,30))  }
date_weeks = {4 : date_weeks_04, 5 : date_weeks_05}

def get_date_flo(week,cycle):
  """ 
  """
  if week == 0:
    date_flo = datetime(2000+cycle,8,1)
  else:
    date_flo = date_weeks[cycle][week][0]
  return date_flo.date()

def get_cycle(current_date):
  """ """ 
  if current_date < date(2003,6,1):
    cycle = 3
  elif date(2003,6,1) <= current_date < date(2004,5,31):
    cycle = 4
  else : cycle = 5
  return cycle



############################
# for mango_asynchrony_glm2 :

def get_date_child(month):
  """
  """
  if 6 < month <13:
    date_child = date(2003,month,15)
  else: date_child = date(2004,month,15)
  return date_child






