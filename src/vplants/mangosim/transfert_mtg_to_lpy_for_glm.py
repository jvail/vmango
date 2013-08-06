from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

def load_mtg(name='mango_mtg.pkl'):
    import cPickle as pickle
    if True : # (share_dir+name).isfile():
        pkl_file = open(share_dir+"//"+ name, 'rb')
        g = pickle.load(pkl_file)
        pkl_file.close()
    return g

g = load_mtg()

features_names = g.property_names()
cogshall_trees = [i for i, v in g.property('var').items() if v == 'cogshall']


def get_children_P_M(vertice):
  """
  Return a tuple of two list. 
  The first list contain children of vertice whith 'P' label. The second list contain children of vertice whith 'M' label. 
  If the father have no children whith 'P' label or 'M' label, it return empty lists.
  """
  if g.label(vertice)=='P' or 'M':
    Pchilds = [Pchild for Pchild in g.children(vertice) if g.label(Pchild)=='P']
    Mchilds = [Mchild for Mchild in g.children(vertice) if g.label(Mchild)=='M']
  else:
    Pchilds = []
    Mchilds = []
  return (Pchilds,Mchilds)


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

def get_nature(vertice):
  """Return the nature of the vertice.
  """
  children = [g.label(c) for c in g.children(vertice)]
  if 'F' in children : nature = 'F'
  else : nature = 'V'
  return nature




