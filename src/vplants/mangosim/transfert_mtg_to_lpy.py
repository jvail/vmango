share_dir = '../../../share/'

def load_mtg(name='mango_mtg.pkl'):
    import cPickle as pickle
    if True : # (share_dir+name).isfile():
        pkl_file = open(share_dir+name, 'rb')
        g = pickle.load(pkl_file)
        pkl_file.close()
    return g

g = load_mtg()

features_names = g.property_names()
cogshall_trees = [i for i, v in g.property('var').items() if v == 'cogshall']

#ucs_cogshall_B14 = [i for i in g.components_at_scale(1,scale=4)]
#ucs_Parents_B14 = [i for i in ucs_cogshall_B14 if g.label(i)=='P']

#parent_B14_code = [g.property('code')[i] for i in ucs_Parents_B14]

#def get_parents(tree=1):
#  ucs_tree = [i for i in g.components_at_scale(tree,scale=4)]
#  ucs_tree_parents = [i for i in ucs_tree if g.label(i)=='P']
#  return ucs_tree_parents

#def get_mothers(tree=1):
#  ucs_tree = [i for i in g.components_at_scale(tree,scale=4)]
#  ucs_tree_mothers = [i for i in ucs_tree if g.label(i)=='M']
#  return ucs_tree_mothers

##### to verifie what give the function children().
#def get_code_children(int):
#  code_father = g.property('code')[int]
#  children = g.children(int)
#  code_children = []
#  for child in children:
#    code_children.append(g.property('code')[child])
#  return code_father,code_children


######### attention: on obtient aussi un parent GU0 inconnu de la base excel
#def get_level_parents(tree=1,nb_str=3):
#  parents = []
#  for i in get_parents(tree):
#    code = g.property('code')[i]
#    split_code = code.split("/")
#    if len(split_code)==nb_str:
#      parents.append(i)
#  return parents


#def get_scaffold_for_ucs3(tree=1):
#  parents = get_parents(tree)
#  mothers = get_mothers(tree)
#  all_scaffold_for_parents = []
#  all_scaffold_for_mothers = []
#  for i in parents:
#    c = g.property('code')[i]
#    code = c.split("/")
#    all_scaffold_for_parents.append(code[1])
#  for i in mothers :
#    c = g.property('code')[i]
#    code = c.split("/")
#    all_scaffold_for_mothers.append(code[1])
#  import collections
#  scaffold_p = collections.Counter(all_scaffold_for_parents)
#  scaffold_m = collections.Counter(all_scaffold_for_mothers)
#  # attention, la fonction compte aussi les parents GU0 inconnu de la base excel
#  return (scaffold_p , scaffold_m )


#def get_max_reiteration_on_parents(tree=1):
#  reiteration_parents = get_level_parents(tree,nb_str=4)
#  reiteration = []
#  for i in reiteration_parents:
#    c = g.property('code')[i]
#    code = c.split("/")
#    reiteration.append(int(code[3]))
#  max_reiteration = -min(reiteration)
#  return max_reiteration


###### code to verify if all description are in mtg
#list_children = [4]
#for p in ucs_Parents_B14:
#  child = g.children(p)
#  for c in child:
#    if g.label(c)=='P':
#      list_children.append(c)

#set(list_children)==set(ucs_Parents_B14)
###### gives True!

def get_children_cycle3(father):
  """Return only children whose burst in cycle 3. If the father have nochildren in cycle 3, it return an empty list. """
  if g.label(father)=='P' or g.label(father)=='M' :
    children = [i for i in g.children(father) if g.label(i)!='F']
  else: children = []
  return children

def get_apical_lateral_position(child_list):
  """Return a tuple of an integer and a list. The integer is the child in apical position, the list gives children in lateral position."""
  if type(child_list)==int : child_list=[child_list]
  apical_position = None
  lateral_position = []
  for child in child_list:
    try: g.property('edge_type')[child]
    except: edge = 'no'
    else: edge = g.property('edge_type')[child]
    if edge == '<':
      apical_position = child
    else: lateral_position.append(child)
  return (apical_position,lateral_position)

def get_cycle(uc):
  """Return the cycle of unit growth 'uc' """
  cycle = g.property('year')[uc] if uc in g.property('year') else 3
  return cycle


def get_nature_2003(uc_03):
  """Return an integer 0 or 6 which means the nature of the unit growth 'uc_03'. 
  If it is 0, the unit growth will give only unit growth, if it is 6, the unit growth will give one or more inflorescences. """
  nature_2003 = g.property('nature')[uc_03]
  state = nature_2003.split("-")[0]
  nature = 0 if state == 'NF' else 6
  return nature


Month = {'janv' : 1, 'fev' : 2, 'mars' : 3, 'avril' : 4, 'mai' : 5, 'juin':6,'juil':7,'aout':8,'sept':9,'oct':10,'nov':11,'dec':12}

def get_date_first_uc_in_cycle4(uc_vegetative):
    """Return a date from datetime type of burst date of the unit growth 'uc_vegetative'. """
    if g.label(uc_vegetative)!='F':
        from datetime import date
        d = g.property('date_burst')[uc_vegetative]
        month,year = d.split(".")
        date_burst = date(2000+int(year),Month[month],1)
    return date_burst

vegetative = 0
terminal_inflo = 1
lateral_inflo = 2
States = {1: (vegetative, "03-07", "04-02"),
  2 : (vegetative, "04-03","04-06"),
  3 : (vegetative, "04-07", "04-10"),
  4 : (vegetative, "04-11", "05-02"),
  5 : (vegetative, "05-03", "05-06"),
  7 : (terminal_inflo, "03-11", "04-02"),
  8 : (terminal_inflo, "04-03", "04-06"),
  9 : (terminal_inflo, "04-07", "05-02"),
  10 : (terminal_inflo, "05-03", "05-06"),
  11 : (lateral_inflo, "03-11", "04-06"),
  12 : (lateral_inflo, "04-11", "05-02") }

def order_uc_date(string):
  """From string = 'month-year', it return a string : 'year-month' but month is an integer. The aim is to order dates. """
  m,y = string.split(".")
  if Month[m] > 9: month = str(Month[m]) 
  else: month = '0'+str(Month[m])
  order_ucs_date = str(y) + '-' + month 
  return order_ucs_date

def get_state_first_uc_in_cycle4(uc_vegetative):
  """ Return state of the unit growth uc_vegetative. 
  If the burst date of the unit growth is not between dates given by States, we put arbitrarily the state 1.
  If the unit growth has no child, we put arbitrarily the state 1.
  """
  nature_children = [g.label(child) for child in g.children(uc_vegetative)]
  if len(nature_children)==0 : nature = vegetative
  else :  
    if 'F' in nature_children :
      flo_children = [nat for nat in nature_children if nat=='F']
      nature = lateral_inflo if len(flo_children)> 1  else terminal_inflo
    else : nature = vegetative
  date_uc = order_uc_date( g.property('date_burst')[uc_vegetative] )
  states = [stat for stat in States.keys() if States[stat][0]==nature and States[stat][1] <= date_uc <= States[stat][2] ]
  state = 1 if len(states)==0 else states[0]
  return state




