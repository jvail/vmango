def load_obj(filename, dirname = '.'):
  import cPickle as pickle
  import os
  gfname = os.path.join(dirname,filename)
  if os.path.exists(gfname ):
    pkl_file = open(gfname,'rb')
    obj = pickle.load(pkl_file)
    pkl_file.close()
    return obj
  else:
    raise ValueError(gfname)

#from openalea.deploy.shared_data import shared_data
#import vplants.mangosim
#share_dir = shared_data(vplants.mangosim, share_path = "share")
#g = load_obj("mango_B12_asynchrony_pierre_002.bmtg", share_dir+"simulations_mangotree_model_pierre/Cogshall_B12/")
# g = load_obj("mango_asynchrony_pierre.bmtg", "..")
#features_names = g.property_names()

from datetime import date

begin_date = { 4 : date(2003,7,1) , 5 : date(2004,7,1) }
end_date = { 4 : date(2004,7,1), 5 : date(2005,7,1) }


def get_ucs_cycle(g,cycle):
  """Return a list of unit growth growing in the given cycle
  Parameters : 
    g : is a mtg object
    cycle : is an integer (3, 4 or 5)	"""
  if cycle ==3 :
    ucs_cycle = [i for i,l in g.property('order_k').items() ]
  elif cycle == 4 or cycle == 5:
    ucs_cycle = [i for i,o in g.property('order').items() if begin_date[cycle] <= g.property('burst_date')[i] < end_date[cycle] and g.label(i)!='Flower']
  return ucs_cycle



def get_uc_cycle(g,uc):
  """Return the cycle which the unit growth uc was born. Takes the case of inflorescence. """
  if g.label(uc)=='B_UC':
    uc_cycle = 3
  elif g.label(uc)=='Flower':
    uc_cycle = g.property('burst_date')[uc].year -2000
  else:
    uc_year = g.property('burst_date')[uc].year
    uc_month = g.property('burst_date')[uc].month
    if uc_month > 6 : 
      uc_cycle = uc_year + 1 -2000
    else:
      uc_cycle = uc_year -2000
  return uc_cycle



#def is_given_reiteration(g,uc):
#  """Return a booleen which indicates that this uc gives a reiteration or not"""
#  is_given_reiteration = False
#  children = g.children(uc)
#  children_veg = [j for j in children if g.label(j)!='Flower']
#  if children_veg!=[]:
#    children_cycle = [get_uc_cycle(g,i) for i in children_veg]
#    if len( list( set( children_cycle ) ) ) >1: is_given_reiteration = True
#  return is_given_reiteration

def get_ucs_cycle_in_extremity(g, cycle, ucs ):
  """Return a list of unit growth which are in the extremity of the canopy at the end of a cycle.
  For the moment, if a unit growth gives no children during 2 cycle (so we can consider, it is dead), we not remove it from this list.
  Parameters : 
    g: a mtg object
    cycle: integer 3, 4 or 5"""
  extremity = []
  if cycle == 4 or cycle == 5:
    ucs_extremity_cycleMinus1 = get_ucs_cycle_in_extremity(g,cycle-1, ucs)
    for i1 in ucs_extremity_cycleMinus1:
      children_extremity = [i2 for i2 in g.children(i1) if g.label(i2)!='Flower' and get_uc_cycle(g,i2)==cycle]
      if children_extremity == [] : extremity.append(i1) #and not is_dead_in_cycle(i1,cycle): extremity.append(i1)
  ucs_veget_cycle = ucs[cycle]
  for i in ucs_veget_cycle :
    childrens_in_cycle = [child for child in g.children(i) if g.label(child)!='Flower' and get_uc_cycle(g,child)==cycle ]
    if childrens_in_cycle == [] : extremity.append(i)
  return extremity


def get_nb_ucs_cycle_in_extremity_apical_position(g,cycle, ucs_in_extremity ):
  """Return a number of terminal unit growth which are in apical position.
    Parameters : 
    g : a mtg object
	cycle : integer 4 or 5  """
  ucs_cycle_in_extremity = ucs_in_extremity[cycle]
  ucs_cycle_in_extremity_apical_position = [i for i,e in g.property('edge_type').items() if i in ucs_cycle_in_extremity and e=='<']
  return len(ucs_cycle_in_extremity_apical_position)


def get_nb_axe_cycle(g,cycle, ucs_in_extremity):
  """Return a list of axes per ancestor for a cycle.
  Parameters : 
    g : a mtg object
    cycle: integer 4 or 5"""
  import collections
  ucs_cycleMinus1_in_extremity = ucs_in_extremity[cycle-1]
  nb_children_per_uc = []
  for i in ucs_cycleMinus1_in_extremity:
    children = g.children(i)
    children_veg = [j for j in children if g.label(j)!='Flower']
    are_in_cycle = [get_uc_cycle(g,k)==cycle for k in children_veg]
    nb_children_in_cycle = collections.Counter(are_in_cycle)[True]
    nb_children_per_uc.append(nb_children_in_cycle)
  return nb_children_per_uc



def get_nb_children_per_uc_cycle(g,cycle,ucs, ucs_in_extremity, nb_axe):
  """Return the frequency of number of children per unit growth.
  Parameters : 
    g : a mtg object
    cycle: integer 4 or 5  """
  import copy
  nb_children_per_ucs = copy.deepcopy(nb_axe[cycle]) 
  ucs_cycle = copy.deepcopy(ucs[cycle])
  ucs_in_extremity_cycle = ucs_in_extremity[cycle]
  for i1 in ucs_in_extremity_cycle:
    if i1 in ucs_cycle : ucs_cycle.remove(i1)
  for i2 in ucs_cycle:
    children = g.children(i2)
    children_in_cycle = [j for j in children if get_uc_cycle(g,j)==cycle and g.label(j)!='Flower']
    nb_children_per_ucs.append(len(children_in_cycle))
    import collections
    nb_children_per_uc = dict( collections.Counter(nb_children_per_ucs) )
  return nb_children_per_uc


def get_nb_descendant_per_axe_cycle(g,cycle, ucs_in_extremity):
  """Return the frequency of number of descendants per axes for a tree and for a cycle. 
  Parameters : 
    g : a mtg object
	cycle: integer 4 or 5  """
  ucs_in_extremity_cycleMinus1 = ucs_in_extremity[cycle-1]
  nb_descendant_per_axe_cycle = []
  for i in ucs_in_extremity_cycleMinus1:
    descendants = g.Descendants(i)
    veg_descendants_in_cycle = [j for j in descendants if g.label(j)!='Flower' and get_uc_cycle(g,j)==cycle]
    nb_descendant_per_axe_cycle.append(len(veg_descendants_in_cycle))
  import collections 
  nb_ucs_per_axe = dict( collections.Counter(nb_descendant_per_axe_cycle) )
  return nb_ucs_per_axe



def get_nb_uc_giving_inflorescence_cycle(g,cycle, ucs, ucs_in_extremity):
  """Return the number of unit growth at the end of the canopy (for a cycle) and giving inflorescences.
  Parameters : 
    g : a mtg object
    cycle: integer 4 or 5  """
  ucs_extremity_cycle = ucs_in_extremity[cycle]
  ucs_giving_inflo_cycle = []
  for i in ucs_extremity_cycle:
    children_inflo = [k for k in g.children(i) if g.label(k)=='Flower']
    if children_inflo != [] :
      date_children_inflo = [g.property('burst_date')[j].year == 2000+cycle for j in children_inflo]
      if True in date_children_inflo : ucs_giving_inflo_cycle.append(i)
  return len(ucs_giving_inflo_cycle)


#ucs_given_inflo_04 = [i for i in ucs_04 if g.property('nature')[i]==1]
#extremity = []
#for i in ucs_given_inflo_04:
#    if not is_given_reiteration(i):
#        children = g.children(i)
#        children_veg = [j for j in children if g.label(j)!='Flower']
#        if children_veg==[] : extremity.append(i)
#        else :
#            for k in children:
#                if g.label(k)=='F' : extremity.append(i)
#                cycles_children = [get_uc_cycle(i) for i in children if g.label(i)!='Flower']
#                if 4+1 in cycles_children :
#                    extremity.append(i)
#                    break
#extremity = list(set(extremity))


def get_flowering_rate_cycle(g,cycle, ucs_in_extremity , nb_uc_giving_inflorescence):
  """Return the flowering rate for a cycle.
  Parameters : 
    g : a mtg object
	cycle: integer 4 or 5  """
  nb_uc_giving_inflorescence_cycle = nb_uc_giving_inflorescence[cycle]
  nb_ucs_in_extremity = len( ucs_in_extremity[cycle] )
  flowering_rate = nb_uc_giving_inflorescence_cycle / float(nb_ucs_in_extremity)
  return flowering_rate



def get_terminal_apical_rate_cycle(g,cycle, ucs_in_extremity , nb_ucs_in_extremity_apical_position):
  """Return the rate of terminal unit growth in apical position.
  Parameters : 
    g : a mtg object
	cycle: integer 4 or 5 """
  nb_ucs_cycle_in_extremity_apical_position = nb_ucs_in_extremity_apical_position[cycle]
  nb_ucs_in_extremity = len (ucs_in_extremity[cycle] )
  terminal_apical_rate = nb_ucs_cycle_in_extremity_apical_position /float(nb_ucs_in_extremity)
  return terminal_apical_rate


def order_date(datetime):
  """Return a string "year-month" from a datetime. The aim is to group dates by month and to order these dates. """ 
  from datetime import date
  year = datetime.year - 2000
  month = datetime.month
  if month > 9: m = str(month)
  else: m = '0'+str(month)
  order_date = '0'+str(year) + '-' + m
  return order_date


date_04 = ["03-07","03-08","03-09","03-10","03-11","03-12","04-01","04-02","04-03","04-04","04-05","04-06"]
date_05 = ["04-07","04-08","04-09","04-10","04-11","04-12","05-01","05-02","05-03","05-04","05-05","05-06"]
date = {4: date_04, 5: date_05}

def get_monthly_date_ucs_cycle(g,cycle, ucs):
  """Return the frequency of burst dates of unit growth.
  Parameters : 
    g : a mtg object
    cycle: integer 4 or 5  """
  ucs_cycle = ucs[cycle]
  monthly_date_ucs_cycle = [order_date(d) for i,d in g.property('burst_date').items() if i in ucs_cycle]
  import collections
  freq_monthly_date_ucs_cycle = dict( collections.Counter(monthly_date_ucs_cycle) )
  for d in date[cycle]:
    if d not in freq_monthly_date_ucs_cycle.keys(): 
      freq_monthly_date_ucs_cycle[d]=0
  return freq_monthly_date_ucs_cycle


#def add_freq_dict(dict_a,dict_b):
#  """Return a dictionary which have added the frequency of both dictionaries dict_a and dict_b. """
#  freq_dict = dict( (key, dict_a.get(key, 0) + dict_b.get(key, 0)) for key in set(dict_a)|set(dict_b) )
#  return freq_dict

def extract_info_from_mtg(filename):
    g = load_obj(filename)
    ucs_03 = get_ucs_cycle(g,3)
    ucs_04 = get_ucs_cycle(g,4)
    ucs_05 = get_ucs_cycle(g,5)
    ucs = { 3 : ucs_03, 4 : ucs_04, 5 : ucs_05 }
    ucs_03_in_extremity = get_ucs_cycle_in_extremity(g,3,ucs)
    ucs_04_in_extremity = get_ucs_cycle_in_extremity(g,4,ucs)
    ucs_05_in_extremity = get_ucs_cycle_in_extremity(g,5,ucs)
    ucs_in_extremity = {3: ucs_03_in_extremity , 4: ucs_04_in_extremity ,5: ucs_05_in_extremity }
    nb_ucs_04_in_extremity_apical_position = get_nb_ucs_cycle_in_extremity_apical_position(g,4,ucs_in_extremity )
    nb_ucs_05_in_extremity_apical_position = get_nb_ucs_cycle_in_extremity_apical_position(g,5,ucs_in_extremity)
    nb_ucs_in_extremity_apical_position = { 4 : nb_ucs_04_in_extremity_apical_position, 5 : nb_ucs_05_in_extremity_apical_position }
    terminal_apical_rate_04 = get_terminal_apical_rate_cycle(g,4,ucs_in_extremity,nb_ucs_in_extremity_apical_position)
    terminal_apical_rate_05 = get_terminal_apical_rate_cycle(g,5,ucs_in_extremity,nb_ucs_in_extremity_apical_position)
    terminal_apical_rate = {4: terminal_apical_rate_04, 5:terminal_apical_rate_05}
    nb_uc_giving_inflorescence_04 = get_nb_uc_giving_inflorescence_cycle(g,4,ucs,ucs_in_extremity)
    nb_uc_giving_inflorescence_05 = get_nb_uc_giving_inflorescence_cycle(g,5,ucs,ucs_in_extremity)
    nb_uc_giving_inflorescence = { 4 : nb_uc_giving_inflorescence_04, 5 : nb_uc_giving_inflorescence_05 }
    flowering_rate_04 = get_flowering_rate_cycle(g,4,ucs_in_extremity,nb_uc_giving_inflorescence)
    flowering_rate_05 = get_flowering_rate_cycle(g,5,ucs_in_extremity,nb_uc_giving_inflorescence)
    flowering_rate = {4:flowering_rate_04, 5:flowering_rate_05}
    nb_axe_04 = sum(get_nb_axe_cycle(g,4,ucs_in_extremity))
    nb_axe_05 = sum(get_nb_axe_cycle(g,5,ucs_in_extremity))
    nb_axe = { 4: get_nb_axe_cycle(g,4,ucs_in_extremity), 5: get_nb_axe_cycle(g,5,ucs_in_extremity) }
    nb_children_per_uc_04 = get_nb_children_per_uc_cycle(g,4,ucs,ucs_in_extremity,nb_axe)
    nb_children_per_uc_05 = get_nb_children_per_uc_cycle(g,5,ucs,ucs_in_extremity,nb_axe)
    nb_children_per_uc = {4 : nb_children_per_uc_04, 5 : nb_children_per_uc_05}
    nb_descendant_per_axe_04 = get_nb_descendant_per_axe_cycle(g,4,ucs_in_extremity)
    nb_descendant_per_axe_05 = get_nb_descendant_per_axe_cycle(g,5,ucs_in_extremity)
    nb_descendant_per_axe = {4 : nb_descendant_per_axe_05, 5 : nb_descendant_per_axe_05 }
    monthly_date_ucs_04 = get_monthly_date_ucs_cycle(g,4,ucs)
    monthly_date_ucs_05 = get_monthly_date_ucs_cycle(g,5,ucs)
    monthly_date_ucs = {4: monthly_date_ucs_04, 5:monthly_date_ucs_05}
    return g, ucs, ucs_in_extremity, nb_ucs_in_extremity_apical_position, terminal_apical_rate, nb_uc_giving_inflorescence, flowering_rate, nb_axe_04, nb_axe_05, nb_children_per_uc, nb_descendant_per_axe, monthly_date_ucs
	
(g, ucs, ucs_in_extremity, nb_ucs_in_extremity_apical_position, terminal_apical_rate, 
  nb_uc_giving_inflorescence, flowering_rate, nb_axe_04, nb_axe_05, nb_children_per_uc, 
  nb_descendant_per_axe, monthly_date_ucs ) = extract_info_from_mtg("mango_asynchrony_pierre.bmtg")

