from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

import pandas as pd
from numpy import cumsum
from numpy.random import binomial, poisson, uniform, randint
from datetime import datetime, date
from dateutil.relativedelta import relativedelta


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


def get_value_simulation_glm_distribution(name_table_prob_glm):
  """
  """
  # dict_table_prob_glm = get_dict_from_table(*table_prob_glm)
  dict_table_prob_glm = dict_table_glm[name_table_prob_glm]
  variables_name = dict_table_prob_glm["colnames"]
  probs = dict_table_prob_glm[()]
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

response_variables = {"glm_burst_03_04" : "binomial", "glm_Lateral_GU_daughter_03_04" : "binomial", "glm_no_lateral_GU_03_04" : "poisson", "vglm_Burst_date_child_03_04" : "vglm",
    "glm_burst_04" : "binomial", "glm_Lateral_GU_daughter_04" : "binomial", "glm_no_lateral_GU_04" : "poisson","vglm_Delta_burst_date_child_04" : "vglm",
    "glm_Flowering_04" : "binomial", "glm_No_inflorescences_04" : "poisson", "vglm_Flowering_Date" : "vglm",  
    "glm_burst_05" : "binomial", "glm_Lateral_GU_daughter_05" : "binomial", "glm_no_lateral_GU_05" : "poisson", "vglm_Delta_burst_date_child_05" : "vglm", 
    "glm_Flowering_05" : "binomial", "glm_No_inflorescences_05" : "poisson" }


def get_dict_files_glm(SELECT_TREE = False , feature_tree="B12", key_dict=response_variables):
  """
  The aim is to get all tables of probability of variables
  Parameters : 
    feature_tree : a string, name of a tree, "loaded" or "notloaded"
      model for each tree, for loaded trees or for not loaded trees
    key_dict : a dictionary, containing name of table and family of the variable taken
  """
  path_file = share_dir+"/model_glm/model_nul/"
  if SELECT_TREE : 
    sub_file = "by_tree/"+feature_tree+"/"  
  else : 
    sub_file = "by_all_trees/"  
  key_dict_files = key_dict.keys()
  dict_files = {}
  for key in key_dict_files:
    final_path = path_file + sub_file+ "table_prob_"+key+".csv"
    dict_files[key]= (pd.read_csv(final_path),key_dict[key])
  return dict_files


def set_dicts_files_glm():
  """
  """
  list_feature_tree = ["B10","B12","B14","F2","F6"]+["loaded","notloaded"]
  tuple_keys_dict_files_nul_0 = [(True, feature_tree) for feature_tree in list_feature_tree]
  tuple_keys_dict_files_nul_1 = [(False,None)]
  tuple_keys_dict_files_nul = tuple_keys_dict_files_nul_0 + tuple_keys_dict_files_nul_1
  dicts_files_glm = {}
  for tuple_keys in tuple_keys_dict_files_nul:
    dicts_files_glm[tuple_keys] = get_dict_files_glm(tuple_keys[0], tuple_keys[1])
  return dicts_files_glm

dicts_files_glm = set_dicts_files_glm()

dict_table_glm = {}
def init_model_nul_table(dicts_files_glm , SELECT_TREE = False, feature_tree="B12"):
   """
   Parameters : 
    feature_tree : a string,
      it could be the name of a tree ( for example "B12", "F2"), "loaded" or "notloaded"
   """
   if not SELECT_TREE : feature_tree = None
   key_dicts_files_glm = (SELECT_TREE, feature_tree)
   dict_file_glm = dicts_files_glm[key_dicts_files_glm]
   global dict_table_glm
   for name, info in dict_file_glm.iteritems():
      dict_table_glm[name] = get_dict_from_table(*info)
  
#init_model_nul_table(dicts_files_glm, SELECT_TREE = False, feature_tree ="B12" )


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

def get_table_cycle(current_date):
  """ """ 
  if current_date < date(2003,6,1):
    cycle = 3
  elif date(2003,6,1) <= current_date < date(2004,5,31):
    cycle = 4
  else : cycle = 5
  return cycle

def get_cycle(current_date):
  """ """ 
  if current_date < date(2003,6,1):
    cycle = 3
  elif date(2003,6,1) <= current_date < date(2004,5,31):
    cycle = 4
  elif date(2004,6,1) <= current_date < date(2005,5,31):
    cycle = 5
  else : cycle = 6
  return cycle


############################
# for base 2003 :

def get_date_child(month):
  """
  """
  if 6 < month <13:
    date_child = date(2003,month,15)
  else: date_child = date(2004,month,15)
  return date_child
############################

def set_values_variables_2003():
  """
  """
  No_lateral_next_cycle, date_child = 0, None
  Burst_next_cycle = get_value_simulation_glm_distribution("glm_burst_03_04")
  if Burst_next_cycle:
    Burst_date_child = get_value_simulation_glm_distribution("vglm_Burst_date_child_03_04")
    date_child = get_date_child(Burst_date_child)
    Lateral_next_cycle = get_value_simulation_glm_distribution("glm_Lateral_GU_daughter_03_04")
    if Lateral_next_cycle:
      No_lateral_next_cycle = get_value_simulation_glm_distribution("glm_no_lateral_GU_03_04")
  return No_lateral_next_cycle, date_child

def set_values_variables(burst_date):
  """
  """
  cycle = get_table_cycle(burst_date)
  Nb_child, date_burst_children, No_inflo, date_inflo = 0,None,0,None
  Burst = get_value_simulation_glm_distribution("glm_burst_0"+str(cycle))
  if Burst :
    Lateral = get_value_simulation_glm_distribution("glm_Lateral_GU_daughter_0"+str(cycle))
    if Lateral:
      No_lateral = get_value_simulation_glm_distribution("glm_no_lateral_GU_0"+str(cycle))
    else : No_lateral = 0 
    #
    delta_date_month = get_value_simulation_glm_distribution("vglm_Delta_burst_date_child_0"+str(cycle))
    date_burst_children = burst_date + relativedelta(months= delta_date_month)
    #
    cycle_mother = get_cycle(burst_date)
    cycle_daughter = get_cycle(date_burst_children)
    if cycle_mother < cycle_daughter :
      Flowering = get_value_simulation_glm_distribution( "glm_Flowering_0"+str(cycle) )
      if Flowering:
        No_inflo = get_value_simulation_glm_distribution( "glm_No_inflorescences_0"+str(cycle) )
        week_inflo = get_value_simulation_glm_distribution( "vglm_Flowering_Date" )
        date_inflo = get_date_flo(week_inflo,cycle)
        Nb_child = No_lateral
      else : Nb_child = 1 + No_lateral
    else : Nb_child = 1 + No_lateral
  return Nb_child, date_burst_children, No_inflo, date_inflo











