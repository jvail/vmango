from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

import pandas as pd
from numpy import cumsum
from numpy.random import binomial, poisson, uniform, randint
from datetime import datetime, date


colnames = ["is_loaded","burst_date_mother","position_mother","position_ancestor","nature_ancestor","nature_mother"]
def get_dict_from_table(table_prob, family):
  """
  Argument : 
    table_prob : a data frame from pandas which contain the parameter(s) of a law fitted by glm's method. There are variables which were use for the glm.
    family : a string, it can be "binomial", "poisson", "polr".
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

#table_prob_glm_burst_03_04 = pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_burst_03_04.csv")
#dict_table_prob_glm_burst_03_04 = get_dict_from_table(table_prob_glm_burst_03_04, "binomial")

#table_prob_polr_Burst_date_child_03_04 = pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_polr_Burst_date_child_03_04.csv")
#dict_table_prob_polr_Burst_date_child_03_04 = get_dict_from_table(table_prob_polr_Burst_date_child_03_04, "polr")


is_loaded = "yes"
burst_date_mother = 10
position_mother = "A"
position_ancestor = "A"
nature_ancestor = "F"
nature_mother = "V"




def get_value_simulation_glm_distribution(name_table_prob_glm,is_loaded,burst_date_mother,position_mother,position_ancestor,nature_ancestor,nature_mother):
  """
  """
  # dict_table_prob_glm = get_dict_from_table(*table_prob_glm)
  dict_table_prob_glm = dict_table_glm[name_table_prob_glm]
  dict_variables = {"is_loaded" : is_loaded, "burst_date_mother" : burst_date_mother,"position_mother":position_mother,"position_ancestor": position_ancestor,"nature_ancestor":nature_ancestor,"nature_mother":nature_mother}
  variables_name = dict_table_prob_glm["colnames"]
  key_dict_table = tuple(dict_variables[name] for name in variables_name)
  #if not key_dict_table in dict_table_prob_glm.keys() : print dict_table_prob_glm
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
    response = dict_table_prob_glm["probs_name"][i-1]
  return response
# if cumsum_probs[i] <= unif_value < cumsum_probs[i+1] :

dict_file_glm_complet = {
  "Burst_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_burst_03_04.csv"),"binomial"),
  "Lateral_GU_daughter_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Lateral_GU_daughter_03_04.csv"),"binomial"),
  "No_lateral_GU_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_no_lateral_GU_03_04.csv"),"poisson"),
  "Burst_date_child_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_vglm_Burst_date_child_03_04.csv"),"vglm"),
  "Burst_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Burst_04.csv"),"binomial"),
  "Lateral_GU_daughter_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Lateral_GU_daughter_04.csv"),"binomial"),
  "No_lateral_GU_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_no_lateral_GU_04.csv"),"poisson"),
  "Delta_burst_date_child_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_vglm_Delta_Burst_date_child_04.csv"),"vglm"),
  "Flowering_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Flowering_04.csv"),"binomial"),
  "No_inflo_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_No_inflorescences_04.csv"),"poisson"),
  "Date_inflo_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_vglm_Date_inflo_05.csv"),"vglm"),
  "Burst_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_burst_04_05.csv"),"binomial"),
  "Lateral_GU_daughter_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Lateral_GU_daughter_04_05.csv"),"binomial"),
  "No_lateral_GU_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_no_lateral_GU_04_05.csv"),"poisson"),
  "Delta_burst_date_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_vglm_Delta_burst_date_04_05.csv"),"vglm"),
  "Burst_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Burst_05.csv"),"binomial"),
  "Lateral_GU_daughter_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Lateral_GU_daughter_05.csv"),"binomial"),
  "No_lateral_GU_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_no_lateral_GU_05.csv"),"poisson"),
  "Delta_burst_date_child_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_vglm_Delta_Burst_date_child_05.csv"),"vglm"),
  "Flowering_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_Flowering_05.csv"),"binomial"),
  "No_inflo_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_complet/"+"table_prob_glm_No_inflorescences_05.csv"),"poisson")
  }

dict_file_glm_selected = {
  "Burst_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_burst_03_04.csv"),"binomial"),
  "Lateral_GU_daughter_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Lateral_GU_daughter_03_04.csv"),"binomial"),
  "No_lateral_GU_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_no_lateral_GU_03_04.csv"),"poisson"),
  "Burst_date_child_03_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_vglm_Burst_date_child_03_04.csv"),"vglm"),
  "Burst_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Burst_04.csv"),"binomial"),
  "Lateral_GU_daughter_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Lateral_GU_daughter_04.csv"),"binomial"),
  "No_lateral_GU_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_no_lateral_GU_04.csv"),"poisson"),
  "Delta_burst_date_child_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_vglm_Delta_Burst_date_child_04.csv"),"vglm"),
  "Flowering_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Flowering_04.csv"),"binomial"),
  "No_inflo_04" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_No_inflorescences_04.csv"),"poisson"),
  "Date_inflo_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_vglm_Date_inflo_05.csv"),"vglm"),
  "Burst_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_burst_04_05.csv"),"binomial"),
  "Lateral_GU_daughter_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Lateral_GU_daughter_04_05.csv"),"binomial"),
  "No_lateral_GU_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_no_lateral_GU_04_05.csv"),"poisson"),
  "Delta_burst_date_04_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_vglm_Delta_burst_date_04_05.csv"),"vglm"),
  "Burst_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Burst_05.csv"),"binomial"),
  "Lateral_GU_daughter_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Lateral_GU_daughter_05.csv"),"binomial"),
  "No_lateral_GU_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_no_lateral_GU_05.csv"),"poisson"),
  "Delta_burst_date_child_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_vglm_Delta_Burst_date_child_05.csv"),"vglm"),
  "Flowering_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_Flowering_05.csv"),"binomial"),
  "No_inflo_05" : (pd.read_csv(share_dir+"/model_glm/table_prob_glm_selected/"+"table_prob_glm_No_inflorescences_05.csv"),"poisson")
  }

dict_table_glm = {}

def init_glm_table(is_selected):
   if is_selected : dict_file_glm = dict_file_glm_selected
   else : dict_file_glm = dict_file_glm_complet
   global dict_table_glm
   for name, info in dict_file_glm.iteritems():
      dict_table_glm[name] = get_dict_from_table(*info)
  
init_glm_table(False)

date_weeks_04 = {
'0' : (datetime(2004,7,1),datetime(2004,8,7)),
'1' : (datetime(2004,8,8),datetime(2004,8,14)),
'2' : (datetime(2004,8,15),datetime(2004,8,21)),
'3' : (datetime(2004,8,22),datetime(2004,8,28)),
'4' : (datetime(2004,8,29),datetime(2004,9,4)),
'5' : (datetime(2004,9,5),datetime(2004,9,11)),
'6' : (datetime(2004,9,12),datetime(2004,9,18)),
'7' : (datetime(2004,9,19),datetime(2004,9,25)),
'8' : (datetime(2004,9,26),datetime(2004,10,2)),
'9' : (datetime(2004,10,3),datetime(2004,10,9)),
'10' : (datetime(2004,10,10),datetime(2004,10,16)),
'11' : (datetime(2004,10,17),datetime(2004,10,23)),
'12' : (datetime(2004,10,24),datetime(2004,10,30))  }

date_weeks_05 = {
'0' : (datetime(2005,7,1),datetime(2005,8,7)),
'1': (datetime(2005,8,8),datetime(2005,8,14)),
'2' : (datetime(2005,8,15),datetime(2005,8,21)),
'3' : (datetime(2005,8,22),datetime(2005,8,28)),
'4' : (datetime(2005,8,29),datetime(2005,9,4)),
'5' : (datetime(2005,9,5),datetime(2005,9,11)),
'6' : (datetime(2005,9,12),datetime(2005,9,18)),
'7' : (datetime(2005,9,19),datetime(2005,9,25)),
'8' : (datetime(2005,9,26),datetime(2005,10,2)),
'9' : (datetime(2005,10,3),datetime(2005,10,9)),
'10' : (datetime(2005,10,10),datetime(2005,10,16)),
'11' : (datetime(2005,10,17),datetime(2005,10,23)),
'12' : (datetime(2005,10,24),datetime(2005,10,30))  }
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







