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

cogshall_trees = [i for i, v in g.property('var').items() if v == 'cogshall']
fruit_loaded_prop = g.property('fr_load') 
cogshall_notloaded_trees = [i for i in cogshall_trees if fruit_loaded_prop[i] == 'NC']
cogshall_loaded_trees = [i for i in cogshall_trees if fruit_loaded_prop[i] == 'C']

def get_total_ucs_tree_cycle(tree, cycle):
  """Return the number of entire unit growth for a tree and for a cycle 
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5"""
  ucs_tree = [i for i in g.components_at_scale(tree,scale=4)]
  ucs_tree_cycle = [i for i,y in g.property('year').items() if y==cycle and i in ucs_tree]
  ucs_veget_tree_cycle = [i for i in ucs_tree_cycle if g.label(i)!='F'] # keep vegetative unit growth (remove inflorescence)
  return ucs_veget_tree_cycle 

###### Loaded Cogshall trees :
#nb_ucs_veget_B12_04 = len( get_total_ucs_tree_cycle(71,4) )
#nb_ucs_veget_B12_05 = len( get_total_ucs_tree_cycle(71,5) )
#nb_ucs_veget_B10_04 = len( get_total_ucs_tree_cycle(173,4) )
#nb_ucs_veget_B10_05 = len( get_total_ucs_tree_cycle(173,5) )
#nb_ucs_veget_F2_04 = len( get_total_ucs_tree_cycle(772,4) )
#nb_ucs_veget_F2_05 = len( get_total_ucs_tree_cycle(772,5) )
###### Not loaded Cogshall trees :
#nb_ucs_veget_B14_04 = len( get_total_ucs_tree_cycle(1,4) )
#nb_ucs_veget_B14_05 = len( get_total_ucs_tree_cycle(1,5) )
#nb_ucs_veget_F6_04 = len( get_total_ucs_tree_cycle(1191,4) )
#nb_ucs_veget_F6_05 = len( get_total_ucs_tree_cycle(1191,5) )

#dump_obj( (nb_ucs_veget_B12_04,nb_ucs_veget_B12_05,
#  nb_ucs_veget_B10_04,nb_ucs_veget_B10_05,
#  nb_ucs_veget_F2_04,nb_ucs_veget_F2_05,
#  nb_ucs_veget_B14_04,nb_ucs_veget_B14_05,
#  nb_ucs_veget_F6_04,nb_ucs_veget_F6_05), "total_number_ucs_tree_cycle.pkl")
from os.path import join
(nb_ucs_veget_B12_04,nb_ucs_veget_B12_05,nb_ucs_veget_B10_04,nb_ucs_veget_B10_05,
  nb_ucs_veget_F2_04,nb_ucs_veget_F2_05,nb_ucs_veget_B14_04,nb_ucs_veget_B14_05,
  nb_ucs_veget_F6_04,nb_ucs_veget_F6_05) = load_obj("total_number_ucs_tree_cycle.pkl",join(share_dir,"info_mtg_doralice"))

#def is_given_type_of_reiteration(uc):
#  """Check if a unit growth uc is a type of reiteration or not. It takes only the case when reiteration is between cycle (mother in cycle 4 and daugther in cycle 5)
#  It work too for a mother in cycle 3 and a daugther in cycle 4.  
#  Parameter : 
#    uc : integer in g.components_at_scale(tree,scale=4)"""
#  is_reiteration = False
#  if g.label(uc)=='P' or g.label(uc)=='M':
#    uc_cycle = 3
#  else : uc_cycle = g.property('year')[uc]
#  children = g.children(uc)
#  if children!=[]:
#    uc_vegetative_children = [i for i in children if g.label(i)!='F']
#    children_cycle = [y for i,y in g.property('year').items() if i in uc_vegetative_children]
#    #if len( list( set( children_cycle ) ) ) >1: is_reiteration = True
#    if uc_cycle in children_cycle and uc_cycle+1 in children_cycle : is_reiteration = True
#  return is_reiteration

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

def is_dead_in_cycle(uc,cycle):
  """Return a boolean that indicates if a unit growth is still dead at the end of a cycle. """
  if uc not in g.property('dead') : is_dead_in_cycle = False
  else : 
    date_dead = g.property('dead')[uc]
    date_end_cycle = "0"+str(cycle)+ "-07"
    if order_uc_date(date_dead) < date_end_cycle : is_dead_in_cycle = True
    else: is_dead_in_cycle = False
  return is_dead_in_cycle

def get_cycle_uc(uc):
  """Return the cycle of the uc.  """
  if g.label(uc)=='M' or g.label(uc)=='P': cycle_uc = 3
  else: cycle_uc = g.property('year')[uc]
  return cycle_uc

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


###### Loaded Cogshall trees :  
#nb_ucs_B12_04_in_extremity = len( get_ucs_tree_cycle_in_extremity(71,4) )
#nb_ucs_B12_05_in_extremity = len( get_ucs_tree_cycle_in_extremity(71,5) ) 
#nb_ucs_B10_04_in_extremity = len( get_ucs_tree_cycle_in_extremity(173,4) )
#nb_ucs_B10_05_in_extremity = len( get_ucs_tree_cycle_in_extremity(173,5) )
#nb_ucs_F2_04_in_extremity = len( get_ucs_tree_cycle_in_extremity(772,4) )
#nb_ucs_F2_05_in_extremity = len( get_ucs_tree_cycle_in_extremity(772,5) )
###### Not loaded Cogshall trees :
#nb_ucs_B14_04_in_extremity = len( get_ucs_tree_cycle_in_extremity(1,4) )
#nb_ucs_B14_05_in_extremity = len( get_ucs_tree_cycle_in_extremity(1,5) )
#nb_ucs_F6_04_in_extremity = len( get_ucs_tree_cycle_in_extremity(1191,4) )
#nb_ucs_F6_05_in_extremity = len( get_ucs_tree_cycle_in_extremity(1191,5) )

#dump_obj( (nb_ucs_B12_04_in_extremity,nb_ucs_B12_05_in_extremity,
#  nb_ucs_B10_04_in_extremity,nb_ucs_B10_05_in_extremity,
#  nb_ucs_F2_04_in_extremity,nb_ucs_F2_05_in_extremity,
#  nb_ucs_B14_04_in_extremity,nb_ucs_B14_05_in_extremity,
#  nb_ucs_F6_04_in_extremity,nb_ucs_F6_05_in_extremity) , "nb_ucs_tree_cycle_in_extremity.pkl")


(nb_ucs_B12_04_in_extremity,nb_ucs_B12_05_in_extremity,nb_ucs_B10_04_in_extremity,
  nb_ucs_B10_05_in_extremity,nb_ucs_F2_04_in_extremity,nb_ucs_F2_05_in_extremity,
  nb_ucs_B14_04_in_extremity,nb_ucs_B14_05_in_extremity,nb_ucs_F6_04_in_extremity,
  nb_ucs_F6_05_in_extremity) = load_obj( "nb_ucs_tree_cycle_in_extremity.pkl", join( share_dir, "info_mtg_doralice")

nb_ucs_tree_cycle_in_extremity = {
  (71,4) : nb_ucs_B12_04_in_extremity,
  (71,5) : nb_ucs_B12_05_in_extremity,
  (173,4) : nb_ucs_B10_04_in_extremity,
  (173,5) : nb_ucs_B10_05_in_extremity,
  (772,4) : nb_ucs_F2_04_in_extremity,
  (772,5) : nb_ucs_F2_05_in_extremity,
  (1,4) : nb_ucs_B14_04_in_extremity,
  (1,5) : nb_ucs_B14_05_in_extremity,
  (1191,4) : nb_ucs_F6_04_in_extremity,
  (1191,5) : nb_ucs_F6_05_in_extremity }

#from vplants.mangosim.transfert_mtg_to_lpy import get_cycle
def get_cycle(uc):
  """Return the cycle of unit growth 'uc' """
  cycle = g.property('year')[uc] if uc in g.property('year') else 3
  return cycle
#############################################################################################
#					To fix bug of code of unit growth										#
#																						    #
def get_ucs_reiteration(tree):																#
  """Return unit growth which is a type of reiteration for a given tree """					#
  ucs_tree = g.components_at_scale(tree,scale=4)											#
  ucs_veget_tree = [i for i in ucs_tree if g.label(i)!='F' and g.label(i)!='P']				#
  reiteration = []																			#
  for j in ucs_veget_tree:																	#
    cycle_j = g.property('year')[j]															#
    parent_j = g.parent(j)																	#
    cycle_parent_j = get_cycle(parent_j)													#
    if cycle_parent_j < cycle_j:															#
      sisters_j = g.children(parent_j)														#
      veg_sisters_j = [i for i in sisters_j if g.label(i)!='F']								#
      cycle_sisters = [y for i,y in g.property('year').items() if i in veg_sisters_j]		#
      if len(set(cycle_sisters)) > 1:														#
        reiteration.append(j)																#
  return reiteration																		#
all_trees = [i for i,v in g.property('var').items()]										#
list_id_ucs_reiteration = []																#
for tree in all_trees:																		#
  reiteration_tree = get_ucs_reiteration(tree)												#
  for reit in reiteration_tree:																#
    list_id_ucs_reiteration.append(reit)													#

list_reiteration_to_modify = [(uc,g.property('code')[uc]) for uc,code in g.property('code').items() if uc in list_id_ucs_reiteration and code.split("/")[-1] not in ["R0","R1","R2","R3","R4","R5","R6","R7"] ]

#############################################################################################



def get_nb_ucs_tree_cycle_in_extremity_apical_position(tree,cycle):
  """Return a number of terminal unit growth which are in apical position.
    Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5  """
  ucs_tree_cycle_in_extremity = get_ucs_tree_cycle_in_extremity(tree,cycle)
  ucs_tree_cycle_in_extremity_apical_position = [i for i,e in g.property('edge_type').items() if i in ucs_tree_cycle_in_extremity and e=='<']
  return len(ucs_tree_cycle_in_extremity_apical_position)

###### Loaded Cogshall trees :  
#nb_ucs_B12_04_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(71,4)
#nb_ucs_B12_05_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(71,5)
#nb_ucs_B10_04_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(173,4)
#nb_ucs_B10_05_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(173,5)
#nb_ucs_F2_04_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(772,4)
#nb_ucs_F2_05_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(772,5)
###### Not loaded Cogshall trees :
#nb_ucs_B14_04_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(1,4)
#nb_ucs_B14_05_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(1,5)
#nb_ucs_F6_04_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(1191,4)
#nb_ucs_F6_05_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(1191,5)

#dump_obj( (nb_ucs_B12_04_in_extremity_apical_position,nb_ucs_B12_05_in_extremity_apical_position,
#  nb_ucs_B10_04_in_extremity_apical_position,nb_ucs_B10_05_in_extremity_apical_position,
#  nb_ucs_F2_04_in_extremity_apical_position,nb_ucs_F2_05_in_extremity_apical_position,
#  nb_ucs_B14_04_in_extremity_apical_position,nb_ucs_B14_05_in_extremity_apical_position,
#  nb_ucs_F6_04_in_extremity_apical_position,nb_ucs_F6_05_in_extremity_apical_position) , "nb_ucs_tree_cycle_in_extremity_apical_position.pkl")

(nb_ucs_B12_04_in_extremity_apical_position,nb_ucs_B12_05_in_extremity_apical_position,nb_ucs_B10_04_in_extremity_apical_position,
  nb_ucs_B10_05_in_extremity_apical_position,nb_ucs_F2_04_in_extremity_apical_position,nb_ucs_F2_05_in_extremity_apical_position,
  nb_ucs_B14_04_in_extremity_apical_position,nb_ucs_B14_05_in_extremity_apical_position,nb_ucs_F6_04_in_extremity_apical_position,
  nb_ucs_F6_05_in_extremity_apical_position) = load_obj("nb_ucs_tree_cycle_in_extremity_apical_position.pkl",share_dir+"info_mtg_doralice/")

def get_nb_axe_tree_cycle(tree,cycle):
  """Return a list of axes per ancestor for a tree and for a cycle.
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 4 or 5"""
  ucs_tree_cycleMinus1_in_extremity = get_ucs_tree_cycle_in_extremity(tree, cycle-1)
  nb_children_per_uc = []
  for i in ucs_tree_cycleMinus1_in_extremity:
    veg_children = [child for child in g.children(i) if g.label(child)!='F']
    are_in_cycle = [y==cycle for j,y in g.property('year').items() if j in veg_children]
    import collections
    nb_children_in_cycle = collections.Counter(are_in_cycle)[True] #in case of children born in cycle+1
    nb_children_per_uc.append(nb_children_in_cycle) 
  return nb_children_per_uc

###### Loaded Cogshall trees :
#nb_axe_B12_04 = sum(get_nb_axe_tree_cycle(71,4))
#nb_axe_B12_05 = sum(get_nb_axe_tree_cycle(71,5))
#nb_axe_B10_04 = sum(get_nb_axe_tree_cycle(173,4))
#nb_axe_B10_05 = sum(get_nb_axe_tree_cycle(173,5))
#nb_axe_F2_04 = sum(get_nb_axe_tree_cycle(772,4))
#nb_axe_F2_05 = sum(get_nb_axe_tree_cycle(772,5))
###### Not loaded Cogshall trees :
#nb_axe_B14_04 = sum(get_nb_axe_tree_cycle(1,4))
#nb_axe_B14_05 = sum(get_nb_axe_tree_cycle(1,5))
#nb_axe_F6_04 = sum(get_nb_axe_tree_cycle(1191,4))
#nb_axe_F6_05 = sum(get_nb_axe_tree_cycle(1191,5))

#dump_obj( (nb_axe_B12_04,nb_axe_B12_05,
#  nb_axe_B10_04,nb_axe_B10_05,
#  nb_axe_F2_04,nb_axe_F2_05,
#  nb_axe_B14_04,nb_axe_B14_05,
#  nb_axe_F6_04,nb_axe_F6_05), "nb_axe_tree_cycle.pkl")

(nb_axe_B12_04,nb_axe_B12_05,nb_axe_B10_04,nb_axe_B10_05,nb_axe_F2_04,nb_axe_F2_05,nb_axe_B14_04,
  nb_axe_B14_05,nb_axe_F6_04,nb_axe_F6_05) = load_obj("nb_axe_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

def get_nb_children_per_uc_tree_cycle(tree,cycle):
  """Return the list of number of children per unit growth.
  Parameters : 
    tree: integer in g.property('var')
    cycle: integer 4 or 5  """
  nb_axe_tree_cycle = get_nb_axe_tree_cycle(tree,cycle)
  nb_children_per_uc = nb_axe_tree_cycle
  total_ucs_tree_cycle = get_total_ucs_tree_cycle(tree,cycle)
  ucs_tree_cycle_in_extremity = get_ucs_tree_cycle_in_extremity(tree, cycle)
  for i1 in ucs_tree_cycle_in_extremity:
    if i1 in total_ucs_tree_cycle : total_ucs_tree_cycle.remove(i1)
  for i2 in total_ucs_tree_cycle:
    vegetative_children = [child for child in g.children(i2) if g.label(child)!='F']
    vegetative_children_in_cycle = [i for i,y in g.property('year').items() if y==cycle and i in vegetative_children]
    nb_children_per_uc.append(len(vegetative_children_in_cycle))
  return nb_children_per_uc

###### Loaded Cogshall trees :
#nb_children_per_uc_B12_04 = get_nb_children_per_uc_tree_cycle(71,4)
#nb_children_per_uc_B12_05 = get_nb_children_per_uc_tree_cycle(71,5)
#nb_children_per_uc_B10_04 = get_nb_children_per_uc_tree_cycle(173,4)
#nb_children_per_uc_B10_05 = get_nb_children_per_uc_tree_cycle(173,5)
#nb_children_per_uc_F2_04 = get_nb_children_per_uc_tree_cycle(772,4)
#nb_children_per_uc_F2_05 = get_nb_children_per_uc_tree_cycle(772,5)
###### Not loaded Cogshall trees :
#nb_children_per_uc_B14_04 = get_nb_children_per_uc_tree_cycle(1,4)
#nb_children_per_uc_B14_05 = get_nb_children_per_uc_tree_cycle(1,5)
#nb_children_per_uc_F6_04 = get_nb_children_per_uc_tree_cycle(1191,4)
#nb_children_per_uc_F6_05 = get_nb_children_per_uc_tree_cycle(1191,5)

#dump_obj( (nb_children_per_uc_B12_04,nb_children_per_uc_B12_05,
#  nb_children_per_uc_B10_04,nb_children_per_uc_B10_05,
#  nb_children_per_uc_F2_04,nb_children_per_uc_F2_05,
#  nb_children_per_uc_B14_04,nb_children_per_uc_B14_05,
#  nb_children_per_uc_F6_04,nb_children_per_uc_F6_05) , "nb_children_per_uc_tree_cycle.pkl")

(nb_children_per_uc_B12_04,nb_children_per_uc_B12_05,nb_children_per_uc_B10_04,nb_children_per_uc_B10_05,
  nb_children_per_uc_F2_04,nb_children_per_uc_F2_05,nb_children_per_uc_B14_04,nb_children_per_uc_B14_05,
  nb_children_per_uc_F6_04,nb_children_per_uc_F6_05) = load_obj("nb_children_per_uc_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

def get_nb_descendant_per_axe_tree_cycle(tree,cycle):
  """Return a list of number of descendants per axes for a tree and for a cycle. 
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 4 or 5  """
  ucs_tree_cycleMinus1_in_extremity = get_ucs_tree_cycle_in_extremity(tree, cycle-1)
  nb_ucs_per_axe_tree_cycle = []
  for i in ucs_tree_cycleMinus1_in_extremity:
    descendants = g.Descendants(i)
    veg_descendants_in_cycle = [j for j in descendants if g.label(j)!='F' and g.property('year')[j]==cycle]
    nb_ucs_per_axe_tree_cycle.append(len(veg_descendants_in_cycle))
  return nb_ucs_per_axe_tree_cycle

###### Loaded Cogshall trees :
#nb_ucs_per_axe_B12_04 = get_nb_descendant_per_axe_tree_cycle(71,4)
#nb_ucs_per_axe_B12_05 = get_nb_descendant_per_axe_tree_cycle(71,5)
#nb_ucs_per_axe_B10_04 = get_nb_descendant_per_axe_tree_cycle(173,4)
#nb_ucs_per_axe_B10_05 = get_nb_descendant_per_axe_tree_cycle(173,5)
#nb_ucs_per_axe_F2_04 = get_nb_descendant_per_axe_tree_cycle(772,4)
#nb_ucs_per_axe_F2_05 = get_nb_descendant_per_axe_tree_cycle(772,5)
###### Not loaded Cogshall trees :
#nb_ucs_per_axe_B14_04 = get_nb_descendant_per_axe_tree_cycle(1,4)
#nb_ucs_per_axe_B14_05 = get_nb_descendant_per_axe_tree_cycle(1,5)
#nb_ucs_per_axe_F6_04 = get_nb_descendant_per_axe_tree_cycle(1191,4)
#nb_ucs_per_axe_F6_05 = get_nb_descendant_per_axe_tree_cycle(1191,5)

#dump_obj( (nb_ucs_per_axe_B12_04,nb_ucs_per_axe_B12_05,
#  nb_ucs_per_axe_B10_04,nb_ucs_per_axe_B10_05,
#  nb_ucs_per_axe_F2_04,nb_ucs_per_axe_F2_05,
#  nb_ucs_per_axe_B14_04,nb_ucs_per_axe_B14_05,
#  nb_ucs_per_axe_F6_04,nb_ucs_per_axe_F6_05), "nb_ucs_per_axe_tree_cycle.pkl")

(nb_ucs_per_axe_B12_04,nb_ucs_per_axe_B12_05,nb_ucs_per_axe_B10_04,nb_ucs_per_axe_B10_05,
  nb_ucs_per_axe_F2_04,nb_ucs_per_axe_F2_05,nb_ucs_per_axe_B14_04,nb_ucs_per_axe_B14_05,
  nb_ucs_per_axe_F6_04,nb_ucs_per_axe_F6_05 )= load_obj("nb_ucs_per_axe_tree_cycle.pkl",share_dir+"info_mtg_doralice/")
  

#### Function not ready :   
# def get_nb_inflo_per_uc_cycle(tree, cycle):
  # """ Return the number of inflorescences per Growth Unit
  # """
  # nb_inflo_per_ucs = []
  # uc_florifere = []
  # ucs_extremity_cycle = get_ucs_tree_cycle_in_extremity(tree, cycle)
  # for i in ucs_extremity_cycle:
    # children = g.children(i)
    # children_flo = [flo for flo in children if g.label(flo)=="F" and get_cycle_uc(flo)==cycle]
    # nb_inflo = 0
    # if len(children_flo) >0 :
      # uc_florifere.append(children_flo[0])
      # nb_inflo_t = g.property('nb_inflo_t')[children_flo]
      # nb_inflo_l = g.property('nb_inflo_l')[children_flo]
      # nb_inflo = int(nb_inflo_t) + int(nb_inflo_l)
    # nb_inflo_per_ucs.append( nb_inflo )
  # import collections
  # nb_inflo_per_uc = dict( collections.Counter(nb_inflo_per_ucs) )
  # return nb_inflo_per_uc
  
###### Loaded Cogshall trees :
#nb_inflo_per_uc_B12_04 = get_nb_inflo_per_uc_cycle(71,4)
#nb_inflo_per_uc_B12_05 = get_nb_inflo_per_uc_cycle(71,5)
#nb_inflo_per_uc_B10_04 = get_nb_inflo_per_uc_cycle(173,4)
#nb_inflo_per_uc_B10_05 = get_nb_inflo_per_uc_cycle(173,5)
#nb_inflo_per_uc_F2_04 = get_nb_inflo_per_uc_cycle(772,4)
#nb_inflo_per_uc_F2_05 = get_nb_inflo_per_uc_cycle(772,5)
###### Not loaded Cogshall trees :
#nb_inflo_per_uc_B14_04 = get_nb_inflo_per_uc_cycle(1,4)
#nb_inflo_per_uc_B14_05 = get_nb_inflo_per_uc_cycle(1,5)
#nb_inflo_per_uc_F6_04 = get_nb_inflo_per_uc_cycle(1191,4)
#nb_inflo_per_uc_F6_05 = get_nb_inflo_per_uc_cycle(1191,5)

#dump_obj( (nb_inflo_per_uc_B12_04,nb_inflo_per_uc_B12_05,
#  nb_inflo_per_uc_B10_04,nb_inflo_per_uc_B10_05,
#  nb_inflo_per_uc_F2_04,nb_inflo_per_uc_F2_05,
#  nb_inflo_per_uc_B14_04,nb_inflo_per_uc_B14_05,
#  nb_inflo_per_uc_F6_04,nb_inflo_per_uc_F6_04), "nb_inflo_per_uc_tree_cycle.pkl")

  

def get_nb_uc_giving_inflorescence_tree_cycle(tree,cycle):
  """Return the number of unit growth at the end of the canopy and giving inflorescences for a tree and for a cycle.
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 3, 4 or 5  """
  ucs_tree = [i for i in g.components_at_scale(tree,scale=4)] # all unit growth for the tree
  ucs_tree_cycle = [i for i,y in g.property('year').items() if y==cycle and i in ucs_tree] # take unit growth which born in the cycle
  inflo_tree_cycle = [i for i in ucs_tree_cycle if g.label(i)=='F']
  ucs_giving_inflo_tree_cycle = [g.parent(i) for i in inflo_tree_cycle]
  ucs_tree_cycle_in_extremity = get_ucs_tree_cycle_in_extremity(tree, cycle)
  ucs_giving_inflo_tree_cycle_extremity = [i for i in ucs_tree_cycle_in_extremity if i in ucs_giving_inflo_tree_cycle]
  return len(ucs_giving_inflo_tree_cycle_extremity)

###### Loaded Cogshall trees :  
#nb_uc_giving_inflorescence_B12_04 = get_nb_uc_giving_inflorescence_tree_cycle(71,4)
#nb_uc_giving_inflorescence_B12_05 = get_nb_uc_giving_inflorescence_tree_cycle(71,5)
#nb_uc_giving_inflorescence_B10_04 = get_nb_uc_giving_inflorescence_tree_cycle(173,4)
#nb_uc_giving_inflorescence_B10_05 = get_nb_uc_giving_inflorescence_tree_cycle(173,5)
#nb_uc_giving_inflorescence_F2_04 = get_nb_uc_giving_inflorescence_tree_cycle(772,4)
#nb_uc_giving_inflorescence_F2_05 = get_nb_uc_giving_inflorescence_tree_cycle(772,5)
###### Not loaded Cogshall trees :
#nb_uc_giving_inflorescence_B14_04 = get_nb_uc_giving_inflorescence_tree_cycle(1,4)
#nb_uc_giving_inflorescence_B14_05 = get_nb_uc_giving_inflorescence_tree_cycle(1,5)
#nb_uc_giving_inflorescence_F6_04 = get_nb_uc_giving_inflorescence_tree_cycle(1191,4)
#nb_uc_giving_inflorescence_F6_05 = get_nb_uc_giving_inflorescence_tree_cycle(1191,5)

#dump_obj( (nb_uc_giving_inflorescence_B12_04,nb_uc_giving_inflorescence_B12_05,
#  nb_uc_giving_inflorescence_B10_04,nb_uc_giving_inflorescence_B10_05,
#  nb_uc_giving_inflorescence_F2_04,nb_uc_giving_inflorescence_F2_05,
#  nb_uc_giving_inflorescence_B14_04,nb_uc_giving_inflorescence_B14_05,
#  nb_uc_giving_inflorescence_F6_04,nb_uc_giving_inflorescence_F6_05),"nb_uc_giving_inflorescence_tree_cycle.pkl")

(nb_uc_giving_inflorescence_B12_04,nb_uc_giving_inflorescence_B12_05,nb_uc_giving_inflorescence_B10_04,
  nb_uc_giving_inflorescence_B10_05,nb_uc_giving_inflorescence_F2_04,nb_uc_giving_inflorescence_F2_05,
  nb_uc_giving_inflorescence_B14_04,nb_uc_giving_inflorescence_B14_05,nb_uc_giving_inflorescence_F6_04,
  nb_uc_giving_inflorescence_F6_05) = load_obj("nb_uc_giving_inflorescence_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

###### To check if inflorescences are at the extremity of the canopy
#ucs_B12 = [i for i in g.components_at_scale(71,scale=4)]
#ucs_B12_04 = [i for i,y in g.property('year').items() if y==4 and i in ucs_B12]
#inflo_B12_04 = [i for i in ucs_B12_04 if g.label(i)=='F']
#ucs_giving_inflo_B12_04 = [g.parent(i) for i in inflo_B12_04]
#extremity = []
#for i in ucs_giving_inflo_B12_04:
#    if not is_given_reiteration(i):
#        childrens = g.children(i)
#        if childrens==[] : extremity.append(i)
#        else :
#            for k in childrens:
#                if g.label(k)=='F' :
#                    extremity.append(i)
#                    break
#                else:
#                    year_childrens = [y for j,y in g.property('year').items() if j in childrens]
#                    if cycle+1 in year_childrens :
#                        extremity.append(i)
#                        break
#extremity = list(set(extremity))
###### len(extremity) = 200 but len(ucs_giving_inflo_tree_cycle) = 201
###### ==> modification of get_nb_uc_giving_inflorescence_tree_cycle into keep only uc giving inflo at the extremity

nb_uc_giving_inflorescence_tree_cycle = {
  (71,4) : nb_uc_giving_inflorescence_B12_04,
  (71,5) : nb_uc_giving_inflorescence_B12_05,
  (173,4) : nb_uc_giving_inflorescence_B10_04,
  (173,5) : nb_uc_giving_inflorescence_B10_05,
  (772,4) : nb_uc_giving_inflorescence_F2_04,
  (772,5) : nb_uc_giving_inflorescence_F2_05,
  (1,4) : nb_uc_giving_inflorescence_B14_04,
  (1,5) :nb_uc_giving_inflorescence_B14_05,
  (1191,4) : nb_uc_giving_inflorescence_F6_04,
  (1191,5) : nb_uc_giving_inflorescence_F6_05 }
 
  
def get_flowering_rate_tree_cycle(tree,cycle):
  """Return the flowering rate for a tree and for a cycle.
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 4 or 5  """
  nb_uc_giving_inflorescence = nb_uc_giving_inflorescence_tree_cycle[tree,cycle]
  nb_ucs_in_extremity = nb_ucs_tree_cycle_in_extremity[tree,cycle]
  flowering_rate = nb_uc_giving_inflorescence / float(nb_ucs_in_extremity)
  return flowering_rate

###### Loaded Cogshall trees :
#flowering_rate_B12_04 = get_flowering_rate_tree_cycle(71,4)
#flowering_rate_B12_05 = get_flowering_rate_tree_cycle(71,5)
#flowering_rate_B10_04 = get_flowering_rate_tree_cycle(173,4)
#flowering_rate_B10_05 = get_flowering_rate_tree_cycle(173,5)
#flowering_rate_F2_04 = get_flowering_rate_tree_cycle(772,4)
#flowering_rate_F2_05 = get_flowering_rate_tree_cycle(772,5)
###### Not loaded Cogshall trees :
#flowering_rate_B14_04 = get_flowering_rate_tree_cycle(1,4)
#flowering_rate_B14_05 = get_flowering_rate_tree_cycle(1,5)
#flowering_rate_F6_04 = get_flowering_rate_tree_cycle(1191,4)
#flowering_rate_F6_05 = get_flowering_rate_tree_cycle(1191,5)

#dump_obj( (flowering_rate_B12_04,flowering_rate_B12_05,
#  flowering_rate_B10_04,flowering_rate_B10_05,
#  flowering_rate_F2_04,flowering_rate_F2_05,
#  flowering_rate_B14_04,flowering_rate_B14_05,
#  flowering_rate_F6_04,flowering_rate_F6_05), "flowering_rate_tree_cycle.pkl")

(flowering_rate_B12_04,flowering_rate_B12_05,flowering_rate_B10_04,flowering_rate_B10_05,
  flowering_rate_F2_04,flowering_rate_F2_05,flowering_rate_B14_04,flowering_rate_B14_05,
  flowering_rate_F6_04,flowering_rate_F6_05) = load_obj("flowering_rate_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

  

def get_terminal_apical_rate_tree_cycle(tree,cycle):
  """Return the rate of terminal unit growth in apical position for a tree and for a cycle.
  Parameters : 
    tree: integer in g.property('var')
	cycle: integer 4 or 5 """
  nb_ucs_in_extremity_apical_position = get_nb_ucs_tree_cycle_in_extremity_apical_position(tree,cycle)
  nb_ucs_in_extremity = nb_ucs_tree_cycle_in_extremity[tree,cycle]
  terminal_apical_rate = nb_ucs_in_extremity_apical_position /float(nb_ucs_in_extremity)
  return terminal_apical_rate

###### Loaded Cogshall trees :
#terminal_apical_rate_B12_04 = get_terminal_apical_rate_tree_cycle(71,4)
#terminal_apical_rate_B12_05 = get_terminal_apical_rate_tree_cycle(71,5)
#terminal_apical_rate_B10_04 = get_terminal_apical_rate_tree_cycle(173,4)
#terminal_apical_rate_B10_05 = get_terminal_apical_rate_tree_cycle(173,5)
#terminal_apical_rate_F2_04 = get_terminal_apical_rate_tree_cycle(772,4)
#terminal_apical_rate_F2_05 = get_terminal_apical_rate_tree_cycle(772,5)
###### Not loaded Cogshall trees :
#terminal_apical_rate_B14_04 = get_terminal_apical_rate_tree_cycle(1,4)
#terminal_apical_rate_B14_05 = get_terminal_apical_rate_tree_cycle(1,5)
#terminal_apical_rate_F6_04 = get_terminal_apical_rate_tree_cycle(1191,4)
#terminal_apical_rate_F6_05 = get_terminal_apical_rate_tree_cycle(1191,5)

#dump_obj( (terminal_apical_rate_B12_04,terminal_apical_rate_B12_05,
#  terminal_apical_rate_B10_04,terminal_apical_rate_B10_05,
#  terminal_apical_rate_F2_04,terminal_apical_rate_F2_05,
#  terminal_apical_rate_B14_04,terminal_apical_rate_B14_05,
#  terminal_apical_rate_F6_04,terminal_apical_rate_F6_05) , "terminal_apical_rate_tree_cycle.pkl")

(terminal_apical_rate_B12_04,terminal_apical_rate_B12_05,terminal_apical_rate_B10_04,
  terminal_apical_rate_B10_05,terminal_apical_rate_F2_04,terminal_apical_rate_F2_05,
  terminal_apical_rate_B14_04,terminal_apical_rate_B14_05,terminal_apical_rate_F6_04,
  terminal_apical_rate_F6_05) = load_obj("terminal_apical_rate_tree_cycle.pkl",share_dir+"info_mtg_doralice/")



date_04 = ["03-07","03-08","03-09","03-10","03-11","03-12","04-01","04-02","04-03","04-04","04-05","04-06"]
date_05 = ["04-07","04-08","04-09","04-10","04-11","04-12","05-01","05-02","05-03","05-04","05-05","05-06"]
date = {4: date_04, 5: date_05}

def get_monthly_date_ucs_tree_cycle(tree,cycle):
  """Return the frequency of burst dates of unit growth.
  Parameters : 
    tree: integer in g.property('var')
    cycle: integer 4 or 5  
  Return : 
    a dictionary, burst date is 'year-month' but month is an integer"""
  total_ucs_tree_cycle = get_total_ucs_tree_cycle(tree,cycle)
  monthly_date_ucs_tree_cycle = [order_uc_date(d) for i,d in g.property('date_burst').items() if i in total_ucs_tree_cycle]
  import collections
  freq_monthly_date_ucs_tree_cycle  = dict(collections.Counter(monthly_date_ucs_tree_cycle) )
  for d in date[cycle]:
    if d not in freq_monthly_date_ucs_tree_cycle.keys(): freq_monthly_date_ucs_tree_cycle[d]=0
  return freq_monthly_date_ucs_tree_cycle

###### Loaded Cogshall trees :
#freq_monthly_date_ucs_B12_04 = get_monthly_date_ucs_tree_cycle(71,4)
#freq_monthly_date_ucs_B12_05 = get_monthly_date_ucs_tree_cycle(71,5)
#freq_monthly_date_ucs_B10_04 = get_monthly_date_ucs_tree_cycle(173,4)
#freq_monthly_date_ucs_B10_05 = get_monthly_date_ucs_tree_cycle(173,5)
#freq_monthly_date_ucs_F2_04 = get_monthly_date_ucs_tree_cycle(772,4)
#freq_monthly_date_ucs_F2_05 = get_monthly_date_ucs_tree_cycle(772,5)
###### Not loaded Cogshall trees :
#freq_monthly_date_ucs_B14_04 = get_monthly_date_ucs_tree_cycle(1,4)
#freq_monthly_date_ucs_B14_05 = get_monthly_date_ucs_tree_cycle(1,5)
#freq_monthly_date_ucs_F6_04 = get_monthly_date_ucs_tree_cycle(1191,4)
#freq_monthly_date_ucs_F6_05 = get_monthly_date_ucs_tree_cycle(1191,5)

#dump_obj( (freq_monthly_date_ucs_B12_04,freq_monthly_date_ucs_B12_05,
#  freq_monthly_date_ucs_B10_04,freq_monthly_date_ucs_B10_05,
#  freq_monthly_date_ucs_F2_04,freq_monthly_date_ucs_F2_05,
#  freq_monthly_date_ucs_B14_04,freq_monthly_date_ucs_B14_05,
#  freq_monthly_date_ucs_F6_04,freq_monthly_date_ucs_F6_05) , "monthly_date_ucs_tree_cycle.pkl")

(freq_monthly_date_ucs_B12_04,freq_monthly_date_ucs_B12_05,freq_monthly_date_ucs_B10_04,
  freq_monthly_date_ucs_B10_05,freq_monthly_date_ucs_F2_04,freq_monthly_date_ucs_F2_05,
  freq_monthly_date_ucs_B14_04,freq_monthly_date_ucs_B14_05,freq_monthly_date_ucs_F6_04,
  freq_monthly_date_ucs_F6_05) = load_obj("monthly_date_ucs_tree_cycle.pkl",share_dir+"info_mtg_doralice/")
