from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

def load_obj(filename, dirname = '.'):
  import cPickle as pickle
  import os
  gfname =os.path.join(dirname,filename)
  if os.path.exists(gfname ):
    pkl_file = open(gfname,'rb')
    obj = pickle.load(pkl_file)
    pkl_file.close()
    return obj
  else:
    raise ValueError(filename)

def dump_obj(obj,filename, dirname = '.'):
  import cPickle as pickle
  import os
  gfname =os.path.join(dirname,filename)
  pkl_file = open(gfname,'wb')
  pickle.dump(obj,pkl_file)
  pkl_file.close()

def load_mtg(name = 'mango_mtg.pkl'):
  g = load_obj(name,share_dir)
  return g

g = load_mtg()

features_names = g.property_names()


def get_trees_loaded_or_not(loaded=True, variety = "cogshall"):
  """ Return a list of trees from a given vairety and which are loaded or not.
  Parameters : 
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  if loaded : loaded_prop = 'C'
  else : loaded_prop = 'NC'
  if variety == "all": trees_loaded_or_not = [i for i,v in g.property('var').items() if g.property('fr_load')[i]==loaded_prop]
  else: trees_loaded_or_not = [i for i,v in g.property('var').items() if v== variety and g.property('fr_load')[i]==loaded_prop]
  return trees_loaded_or_not

def get_total_ucs_tree_cycle(tree, cycle):
  """Return the number of entire unit growth for a tree and for a cycle 
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5"""
  ucs_tree = [i for i in g.components_at_scale(tree,scale=4)]
  ucs_tree_cycle = [i for i,y in g.property('year').items() if y==cycle and i in ucs_tree]
  ucs_veget_tree_cycle = [i for i in ucs_tree_cycle if g.label(i)!='F'] # keep vegetative unit growth (remove inflorescence)
  return ucs_veget_tree_cycle 

def get_ucs_cycle(cycle = 4,loaded = True, variety = "cogshall"):
  """ 
  Parameters : 
    cycle : an integer, cycle 4 or 5
    loaded : a booleen, if true, return trees which are loaded, if false, return trees which are not loaded.
    variety : a string, the choice of variety is : 
            'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  ucs_Cycle = []
  trees = get_trees_loaded_or_not(loaded,variety)
  for tree in trees :
    ucs_Cycle += get_total_ucs_tree_cycle(tree, cycle)
  return ucs_Cycle

ucs_04_loaded_cogshall = get_ucs_cycle(4,True,"cogshall")
ucs_05_loaded_cogshall = get_ucs_cycle(5,True,"cogshall")
ucs_04_notloaded_cogshall = get_ucs_cycle(4,False,"cogshall")
ucs_05_notloaded_cogshall = get_ucs_cycle(5,False,"cogshall")
uc_cycle_variety = {(4,True,"cogshall") : ucs_04_loaded_cogshall ,(5,True,"cogshall") : ucs_05_loaded_cogshall, 
  (4,False,"cogshall"): ucs_04_notloaded_cogshall, (5,False,"cogshall"): ucs_05_notloaded_cogshall}
  
def get_cycle_uc(uc):
  """Return the cycle of the uc.  """
  if g.label(uc)=='M' or g.label(uc)=='P': cycle_uc = 3
  else: cycle_uc = g.property('year')[uc]
  return cycle_uc

Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }

def order_uc_date(string):
  """From string = 'month-year', it return a string : 'year-month' but month is an integer. The aim is to order dates. """
  m,y = string.split(".")
  if Month[m] > 9: month = str(Month[m]) 
  else: month = '0'+str(Month[m])
  order_ucs_date = str(y) + '-' + month 
  return order_ucs_date

def get_delta_date(date_a,date_b):
  """ Return the difference of dates in the scale of month. 
  Be carreful, date_a must be older than date_b (date_a < date_b). If it is not the case, it will return a negative number.
  Parameters : 
    date_a : a string, date in that form "year-month"
    date_b : a string, date in that form "year-month"
  """
  from datetime import date, timedelta
  a = date(2000+int(date_a.split("-")[0]),int(date_a.split("-")[1]),1)
  b = date(2000+int(date_b.split("-")[0]),int(date_b.split("-")[1]),1)
  delta_date_month = (b-a).days/30
  return delta_date_month


def is_dead_in_cycle(uc,cycle):
  """Return a boolean that indicates if a unit growth is still dead at the end of a cycle. """
  if uc not in g.property('dead') : is_dead_in_cycle = False
  else : 
    date_dead = g.property('dead')[uc]
    date_end_cycle = "0"+str(cycle)+ "-07"
    if order_uc_date(date_dead) < date_end_cycle : is_dead_in_cycle = True
    else: is_dead_in_cycle = False
  return is_dead_in_cycle


def get_ucs_tree_cycle_in_extremity(tree, cycle):
  """Return a list of unit growth which are in the extremity of the canopy (for a tree and for a cycle).
  If a unit growth dead in the cycle, it is remove from the list of terminal unit growth.
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5"""
  extremity = []
  if cycle == 4 or cycle == 5:
    ucs_extremity_cycleMinus1 = get_ucs_tree_cycle_in_extremity(tree,cycle-1)
    for i1 in ucs_extremity_cycleMinus1:
      children_extremity = [i2 for i2 in g.children(i1) if g.label(i2)!='F' and get_cycle_uc(i2)==cycle]
      if children_extremity == [] and not is_dead_in_cycle(i1,cycle): extremity.append(i1)
  ucs_veget_tree_cycle = get_total_ucs_tree_cycle(tree, cycle)
  for i in ucs_veget_tree_cycle :
    childrens_in_cycle = [child for child in g.children(i) if g.label(child)!='F' and get_cycle_uc(child)==cycle ]
    if childrens_in_cycle == [] : extremity.append(i)
  return extremity

yes = 1
no = 0
unknow = None

#def get_mother_ucs(cycle=4, loaded=True, variety="cogshall"):
#  """Return the list of growth unit which can give daugthers in the cycle.
#  Parameters :
#    cycle : an integer 4 or 5
#    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
#    variety : a string, the choice or variety is : 
#             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
#  """
#  mother_ucs = []
#  trees = get_trees_loaded_or_not(loaded,variety)
#  for tree in trees : 
#    ucs_tree_cycle_extremity = get_ucs_tree_cycle_in_extremity(tree, cycle)
#    total_ucs_tree_cycle = get_total_ucs_tree_cycle(tree, cycle)
#    mother_ucs += [(uc,no) for uc in total_ucs_tree_cycle if uc not in ucs_tree_cycle_extremity]
#    ucs_tree_cycleMinus1_extremity = get_ucs_tree_cycle_in_extremity(tree, cycle-1)
#    mother_ucs += [(UC,yes) for UC in ucs_tree_cycleMinus1_extremity]
#  return mother_ucs
#mothers_uc_04_loaded_cogshall = get_mother_ucs(4,True,"cogshall")
#mothers_uc_05_loaded_cogshall = get_mother_ucs(5,True,"cogshall")
#mothers_uc_04_notloaded_cogshall = get_mother_ucs(4,False,"cogshall")
#mothers_uc_05_notloaded_cogshall = get_mother_ucs(5,False,"cogshall")


#mother_uc = {(4,True,"cogshall"):mothers_uc_04_loaded_cogshall,
#  (5,True,"cogshall"): mothers_uc_05_loaded_cogshall,
#  (4,False,"cogshall"): mothers_uc_04_notloaded_cogshall,
#  (5,False,"cogshall"): mothers_uc_05_notloaded_cogshall}
import collections


def get_nature_position_ancestor(uc):
  """ """
  nature_ancestor = None
  position_ancestor = None
  if g.property('year')[uc] > 3 : 
    cycle_uc = g.property('year')[uc]
    parent_uc = g.parent(uc)
    while g.property('year')[parent_uc] > cycle_uc -1:
      parent_uc = g.parent(parent_uc)
    ancestor = parent_uc
    flo_children = [flo for flo in g.children(ancestor) if g.label(flo)=='F' and g.property('year')[flo]==cycle_uc-1]
    if len(flo_children) > 0: nature_ancestor = "inflorescence"
    else : nature_ancestor = "vegetative"
    position_ancestor = "A" if g.property('edge_type')[ancestor]=="<" else "L"
  return (nature_ancestor, position_ancestor)

def order_flo_date(string):
  """Put an order in date.
  Parameter : 
    string = day-month-year  
  Return : 
    year-month-day which month is an integer"""
  if len( string.split("/") ) == 3: 
    d,m,y = string.split("/")
    order_date = '0'+ str(int(y)-2000) + '-' + m + '-' + d
  else: order_date = ''
  return order_date

date_weeks_04 = {
0 : ('04-07-01','04-08-07'),
1 : ('04-08-08','04-08-14'),
2 : ('04-08-15','04-08-21'),
3 : ('04-08-22','04-08-28'),
4 : ('04-08-29','04-09-04'),
5 : ('04-09-05','04-09-11'),
6 : ('04-09-12','04-09-18'),
7 : ('04-09-19','04-09-25'),
8 : ('04-09-26','04-10-02'),
9 : ('04-10-03','04-10-09'),
10 : ('04-10-10','04-10-16'),
11 : ('04-10-17','04-10-23'),
12 : ('04-10-24','04-10-30')  }

date_weeks_05 = {
0 : ('05-07-01','05-08-07'),
1 : ('05-08-08','05-08-14'),
2 : ('05-08-15','05-08-21'),
3 : ('05-08-22','05-08-28'),
4 : ('05-08-29','05-09-04'),
5 : ('05-09-05','05-09-11'),
6 : ('05-09-12','05-09-18'),
7 : ('05-09-19','05-09-25'),
8 : ('05-09-26','05-10-02'),
9 : ('05-10-03','05-10-09'),
10 : ('05-10-10','05-10-16'),
11 : ('05-10-17','05-10-23'),
12 : ('05-10-24','05-10-30')  }
date_weeks = {4 : date_weeks_04, 5 : date_weeks_05}

def get_extremity_ucs_variety(cycle=4, loaded=True, variety="cogshall"):
  """Return the list of terminal growth unit in the cycle and for the given variety.
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice or variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  extremity_ucs = []
  trees = get_trees_loaded_or_not(loaded,variety)
  for tree in trees : 
    extremity_ucs += get_ucs_tree_cycle_in_extremity(tree, cycle)
  return extremity_ucs

extremity_03_loaded_cogshall= get_extremity_ucs_variety(3,True,"cogshall")
extremity_04_loaded_cogshall= get_extremity_ucs_variety(4,True,"cogshall")
extremity_05_loaded_cogshall= get_extremity_ucs_variety(5,True,"cogshall")
extremity_03_notloaded_cogshall= get_extremity_ucs_variety(3,False,"cogshall")
extremity_04_notloaded_cogshall= get_extremity_ucs_variety(4,False,"cogshall")
extremity_05_notloaded_cogshall= get_extremity_ucs_variety(5,False,"cogshall")
extremity_variety = {(3,True,"cogshall"): extremity_03_loaded_cogshall,(4,True,"cogshall"):extremity_04_loaded_cogshall, (5,True,"cogshall"): extremity_05_loaded_cogshall, 
  (3,False,"cogshall"): extremity_03_notloaded_cogshall,(4,False,"cogshall"): extremity_04_notloaded_cogshall, (5,False,"cogshall"):extremity_05_notloaded_cogshall}

from pandas import DataFrame, concat

def get_table_INSIDE_for_glm(dict_uc_cycle_variety,extremity_variety, cycle=4, loaded=True, variety="cogshall"):
  """ 
  Parameters :
    cycle : an integer 4 or 5
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety.
  """
  table_INSIDE_for_glm = []
  ucs_cycle_variety = dict_uc_cycle_variety[(cycle, loaded, variety)]
  ucs_extremity = extremity_variety[(cycle, loaded, variety)]
  for i in xrange(len(ucs_cycle_variety)):
    uc = ucs_cycle_variety[i]
    dict_uc = {}
    dict_uc["uc"] = uc
    # get Burst variable
    veg_children = [child for child in g.children(uc) if g.label(child)!='F' and get_cycle_uc(child)==cycle]
    if veg_children==[]: dict_uc["Burst"] = "no"
    else : dict_uc["Burst"] = "yes"
    #### get Dead variable
    #if is_dead_in_cycle(uc,cycle)==True : dict_uc["Dead"] = yes
    #else : dict_uc["Dead"] = no
    # get Lateral GU variable
    if len(veg_children)==0 : dict_uc["Lateral_GU_daughter"] = unknow
    elif len(veg_children)==1 : dict_uc["Lateral_GU_daughter"] = "no"
    else: dict_uc["Lateral_GU_daughter"] = "yes"
    # get Number of lateral GU variable
    lateral_veg_children = [c for c in veg_children if g.property('edge_type')[c]=='+']
    if dict_uc["Lateral_GU_daughter"] == "yes" : dict_uc["No_Lateral_GU"] = len(lateral_veg_children)
    else : dict_uc["No_Lateral_GU"] = unknow
    # get nature ancestor
    dict_uc["nature_ancestor"] = get_nature_position_ancestor(uc)[0]
    # get position ancestor
    dict_uc["position_ancestor"] = get_nature_position_ancestor(uc)[1]
    # get mother position feature 
    if g.property('edge_type')[uc] ==  '+' : dict_uc["lateral_position_mother"] = "L"
    else : dict_uc["position_mother"] = "A"
    # get ancestor position feature
    ancestor = g.parent(uc)	
    if g.property('edge_type')[ancestor] ==  '+' : dict_uc["lateral_position_mother"] = "L"
    else : dict_uc["lateral_position_mother"] = "A"
    # get burst date feature (of mother)
    dateM = order_uc_date( g.property('date_burst')[uc] )
    dict_uc["burst_date_mother"] = int(dateM.split("-")[1])
    # get loaded feature
    if loaded==True : dict_uc["is_loaded"] = "yes"
    else : dict_uc["is_loaded"] = "no"
    # get Delta burst date variable (beetween mother and daughters)
    if dict_uc["Burst"] == "no": 
      dict_uc["Delta_Burst_date_child"] = unknow
      dict_uc["Date_burst_daughter"] = unknow
    else : 
      dates_daugther = dict(collections.Counter([order_uc_date(d) for childs,d in g.property('date_burst').items() if childs in veg_children]))
      dateD = dates_daugther.items()[0][0]
      dict_uc["Date_burst_daughter"] = dateD
      diff_date = get_delta_date(dateM,dateD)
      dict_uc["Delta_Burst_date_child"] = diff_date
    # get extremity feature
    if uc in ucs_extremity : dict_uc["is_in_extremity"]= "yes"
    else : dict_uc["is_in_extremity"]= "no"
    # get varaibles in extremity for flowering
    if dict_uc["is_in_extremity"]=="no" : 
      dict_uc["Flowering"] = unknow
      dict_uc["No_inflo"] = unknow
      dict_uc["Flowering_Date"] = "NA"
    else : 
      flo_child = [flo for flo in g.children(uc) if g.label(flo)=='F' and g.property('year')[flo]==cycle]
      # get Flowering variable and get Number of inflorescences variable and get Flowering Date variable
      if len(flo_child)==0 : 
        dict_uc["Flowering"] = "no"
        dict_uc["No_inflo"] = 0
        dict_uc["Flowering_Date"] = "NA"
      else : 
        dict_uc["Flowering"] = "yes"
        flo = flo_child[0]
        if g.property('nb_inflo_l')[flo] !='':
          dict_uc["No_inflo"] = int( g.property('nb_inlo_t')[flo] ) + int( g.property('nb_inflo_l')[flo] )
        else : dict_uc["No_inflo"] = unknow
        date_flo = order_flo_date( g.property('date_flo')[flo] )
        if date_flo == '' : dict_uc["Flowering_Date"] = unknow
        else : 
          for j in xrange(len(date_weeks[cycle])):
            if date_weeks[cycle][j][0] <= date_flo <= date_weeks[cycle][j][1]:
              dict_uc["Flowering_Date"] = j
    # put the dictionnary on the list
    table_INSIDE_for_glm.append(dict_uc)
  return table_INSIDE_for_glm


table_INSIDE_for_glm_04_loaded_cogshall = DataFrame( get_table_INSIDE_for_glm(uc_cycle_variety,extremity_variety,cycle=4, loaded=True, variety="cogshall") )
table_INSIDE_for_glm_05_loaded_cogshall = DataFrame( get_table_INSIDE_for_glm(uc_cycle_variety,extremity_variety,cycle=5, loaded=True, variety="cogshall") )
table_INSIDE_for_glm_04_notloaded_cogshall = DataFrame( get_table_INSIDE_for_glm(uc_cycle_variety,extremity_variety,cycle=4, loaded=False, variety="cogshall") )
table_INSIDE_for_glm_05_notloaded_cogshall = DataFrame( get_table_INSIDE_for_glm(uc_cycle_variety,extremity_variety,cycle=5, loaded=False, variety="cogshall") )

table_INSIDE_for_glm_04_cogshall = concat([table_INSIDE_for_glm_04_loaded_cogshall,table_INSIDE_for_glm_04_notloaded_cogshall],ignore_index=True)
table_INSIDE_for_glm_05_cogshall = concat([table_INSIDE_for_glm_05_loaded_cogshall,table_INSIDE_for_glm_05_notloaded_cogshall],ignore_index=True)

column_names = list( table_INSIDE_for_glm_04_cogshall.columns )

table_INSIDE_for_glm_04_cogshall.to_csv(share_dir + "model_glm/table_INSIDE_04_cogshall.csv",header=column_names, index=False)
table_INSIDE_for_glm_05_cogshall.to_csv(share_dir + "model_glm/table_INSIDE_05_cogshall.csv",header=column_names, index=False)

def get_table_TRANSITION_for_glm(extremity_variety,cycle=3, loaded=True, variety="cogshall"):
  """
  Parameters :
    cycle : an integer 3 or 4
    loaded : a booleen, if True ucs are from loaded trees, else they are from not loaded trees
    variety : a string, the choice of variety is : 
             'jose', 'irwin', 'cogshall', 'kent', 'tommyatkins', 'kensingtonpride', 'namdocmai' or "all" for all variety. """
  table_TRANSITION_for_glm = []
  mother_ucs_extremity = extremity_variety[(cycle, loaded, variety)]
  for i in xrange(len(mother_ucs_extremity)):
    uc = mother_ucs_extremity[i]
    dict_uc = {}
    dict_uc["uc"] = uc
    veg_children = [veg for veg in g.children(uc) if g.label(veg)=='U' and g.property('year')[veg]==cycle+1]
    # get Burst variable
    if veg_children==[]: dict_uc["Burst"] = "no"
    else : dict_uc["Burst"] = "yes"
    # get Lateral GU variable
    lateral_veg_children = [c for c in veg_children if g.property('edge_type')[c]=='+']
    if dict_uc["Burst"] == "no": dict_uc["Lateral_GU_daughter"] = unknow
    elif len(lateral_veg_children)==0 : dict_uc["Lateral_GU_daughter"] = "no"
    else: dict_uc["Lateral_GU_daughter"] = "yes"
    # get Number of lateral GU variable
    if dict_uc["Lateral_GU_daughter"] == "yes" : dict_uc["No_Lateral_GU"] = len(lateral_veg_children)
    else : dict_uc["No_Lateral_GU"] = unknow
    # get burst date feature (of mother)
    if get_cycle_uc(uc) == 3: dict_uc["burst_date_mother"]= "unknow"
    else:
      dateM = order_uc_date( g.property('date_burst')[uc] )
      dict_uc["burst_date_mother"] = int(dateM.split("-")[1])
    if cycle ==3 :
      # get Burst Date of daughters
      if dict_uc["Burst"] == "yes" :
        dates_daugther = dict(collections.Counter([order_uc_date(d) for childs,d in g.property('date_burst').items() if childs in veg_children]))
        dateD = dates_daugther.items()[0][0]
        dict_uc["Burst_date_child"] = int(dateD.split("-")[1])
      else : dict_uc["Burst_date_child"] = unknow
    else :
      # get Delta burst date variable (beetween mother and daughters)
      if dict_uc["Burst"] == "no" : dict_uc["Delta_Burst_date_child"] = unknow
      elif get_cycle_uc(uc) ==3 : dict_uc["Delta_Burst_date_child"] = "unknow"
      else : 
        dates_daugther = dict(collections.Counter([order_uc_date(d) for childs,d in g.property('date_burst').items() if childs in veg_children]))
        dateD = dates_daugther.items()[0][0]
        diff_date = get_delta_date(dateM,dateD)
        dict_uc["Delta_Burst_date_child"] = diff_date
    # get mother position feature 
    if g.property('edge_type')[uc] ==  '+' : dict_uc["lateral_position_mother"] = "L"
    else : dict_uc["lateral_position_mother"] = "A"
    # get mother's nature 
    flo_children = [flo for flo in g.children(uc) if g.label(flo)=='F' and get_cycle_uc(flo)==cycle]
    if len(flo_children) > 0 : dict_uc["mother_nature"] = "florifere"
    else : dict_uc["mother_nature"] = "vegetative"
    # get loaded feature
    if loaded==True : dict_uc["is_loaded"] = "yes"
    else : dict_uc["is_loaded"] = "no"
    # put the dictionnary on the list
    table_TRANSITION_for_glm.append(dict_uc)
  return table_TRANSITION_for_glm

table_TRANSITION_for_glm_03to04_loaded_cogshall = DataFrame( get_table_TRANSITION_for_glm(extremity_variety,cycle=3, loaded=True, variety="cogshall") )
table_TRANSITION_for_glm_04to05_loaded_cogshall = DataFrame( get_table_TRANSITION_for_glm(extremity_variety,cycle=4, loaded=True, variety="cogshall") )
table_TRANSITION_for_glm_03to04_notloaded_cogshall = DataFrame( get_table_TRANSITION_for_glm(extremity_variety,cycle=3, loaded=False, variety="cogshall") )
table_TRANSITION_for_glm_04to05_notloaded_cogshall = DataFrame( get_table_TRANSITION_for_glm(extremity_variety,cycle=4, loaded=False, variety="cogshall"))

table_TRANSITION_for_glm_03to04_cogshall = concat([table_TRANSITION_for_glm_03to04_loaded_cogshall,table_TRANSITION_for_glm_03to04_notloaded_cogshall],ignore_index=True)
table_TRANSITION_for_glm_04to05_cogshall = concat([table_TRANSITION_for_glm_04to05_loaded_cogshall,table_TRANSITION_for_glm_04to05_notloaded_cogshall],ignore_index=True)

column_names = list( table_TRANSITION_for_glm_03to04_cogshall.columns )
table_TRANSITION_for_glm_03to04_cogshall.to_csv(share_dir + "model_glm/table_TRANSITION_03to04_cogshall.csv",header=column_names, index=False)
table_TRANSITION_for_glm_04to05_cogshall.to_csv(share_dir + "model_glm/table_TRANSITION_04to05_cogshall.csv",header=column_names, index=False)
