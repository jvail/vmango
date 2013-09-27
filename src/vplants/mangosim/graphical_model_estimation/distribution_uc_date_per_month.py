from vplants.mangosim.tools import *
from vplants.mangosim.repository import *

from datetime import date


#ucs_B12 = [i for i in g.components_at_scale(71,scale=4)]
#ucs_B12_04 = [i for i,y in g.property('year').items() if y==4 and i in ucs_B12]
##### to check if inflorescences are counting
#[(i,c) for i,c in g.property('code').items() if i in ucs_B12_04] # ==> yes they are counting


def set_datetime_type(string):
  """From string = 'month-year', it return a date from datetime : datetime.date(year,month,day) """
  m,y = string.split(".")
  order_ucs_date = date(2000+int(y),Month[m],15)
  return order_ucs_date


def get_ucs_burst_period_trees(trees = cogshall_loaded_trees, period = 'E') :
  """
  """
  if type(trees)==int : trees = [trees]
  ucs_trees_period = []
  for tree in trees :
    ucs_tree = [i for i in g.components_at_scale(tree,scale=4) if g.label(i)=='U']
    date_ucs_tree = { ii : set_datetime_type(d) for ii,d in g.property('date_burst').items() if ii in ucs_tree }
    if period=='E':
      ucs_tree_period = [uc for uc in ucs_tree if date_ucs_tree[uc].month in beg_end_period['E']]
    if period=='I':
      ucs_tree_period = [uc for uc in ucs_tree if date_ucs_tree[uc].month in beg_end_period['I']]
    if period=='L':
      ucs_tree_period = [uc for uc in ucs_tree if date_ucs_tree[uc].month in beg_end_period['L']]
    ucs_trees_period += ucs_tree_period
  return ucs_trees_period

  
def set_proba_monthly_in_period_trees(ucs_burst_period_trees = None, trees = cogshall_loaded_trees, period = 'E'):
  """
  """
  if type(trees)==int : trees = [trees]
  ucs_trees_period = ucs_burst_period_trees
  nb_ucs_trees_period = len(ucs_trees_period)
  list_month = list( set( [set_datetime_type(d).month for uc,d in g.property('date_burst').items() if uc in ucs_trees_period] ) )
  dict_nb_ucs_monthly = { }
  for i in xrange(len(list_month)):
    ucs_monthly = [uc for uc,d in g.property('date_burst').items() if uc in ucs_trees_period and set_datetime_type(d).month==list_month[i] ]
    dict_nb_ucs_monthly[list_month[i]]=len(ucs_monthly)
  f = lambda x : x/float(nb_ucs_trees_period)
  dict_proba_monthly_in_period = { k : f(v) for k,v in dict_nb_ucs_monthly.items() }
  unlikely_month = [m for m in beg_end_period[period] if m not in list_month]
  if len(unlikely_month)>0:
    for month in unlikely_month:
      dict_proba_monthly_in_period[month]=0
  return dict_proba_monthly_in_period
  

def loop_dump_proba_monthly_variety(dict_trees_variety = dict_trees_variety, variety = "cog"):
  """
  """
  dict_probas_monthly_variety = {}
  for trees in dict_trees_variety[variety]:
    dict_probas_monthly = {}
    for period in periods:
      ucs_burst_period_trees = get_ucs_burst_period_trees(trees,period)
      probas_monthly_period_trees = set_proba_monthly_in_period_trees(ucs_burst_period_trees,trees,period)
      dict_probas_monthly[period] = probas_monthly_period_trees
    if type(trees)==int : dict_probas_monthly_variety[g.property('code')[trees]] = dict_probas_monthly
    elif len(trees)==3 : dict_probas_monthly_variety["C"] = dict_probas_monthly
    else : dict_probas_monthly_variety["NC"] = dict_probas_monthly
  dump_obj(dict_probas_monthly_variety, "dict_probas_monthly_for_period_"+variety+".pkl", join( share_dir, "parameters_data_for_graphic_model","Cogshall" ) )



def estimate() :
    """
    """
    ########## Cogshall loaded trees
    ## count UCs
    ucs_burst_E_loaded_trees = get_ucs_burst_period_trees(cogshall_loaded_trees,'E')
    ucs_burst_I_loaded_trees = get_ucs_burst_period_trees(cogshall_loaded_trees,'I')
    ucs_burst_L_loaded_trees = get_ucs_burst_period_trees(cogshall_loaded_trees,'L')
    dict_ucs_burst_period_loaded_trees = { 'E' : ucs_burst_E_loaded_trees, 'I' : ucs_burst_I_loaded_trees, 'L' : ucs_burst_L_loaded_trees}
    ## probability of monthy occurence
    probas_monthly_E_loaded_trees = set_proba_monthly_in_period_trees(dict_ucs_burst_period_loaded_trees)
    #
    periods = ['E','I','L']
    from copy import deepcopy
    trees_Cogshall_variety = deepcopy(cogshall_trees)
    trees_Cogshall_variety.append( deepcopy( cogshall_loaded_trees))
    trees_Cogshall_variety.append( deepcopy( cogshall_notloaded_trees))
    dict_trees_variety = {"cog" : trees_Cogshall_variety}
    loop_dump_proba_monthly_variety(dict_trees_variety = dict_trees_variety, variety = "cog")
    

if __name__=="__main__" : 
    print "estimation de la distribution des UCs par mois et par periode"
    estimate()

    
    
dict_proba_monthly_for_period = load_obj( dict_probas_monthly_for_period_cog_file )





# def get_date_ucs_trees_cycle(trees = cogshall_loaded_trees,cycle = None):
  # """Return date of vegetative unit growth for specific trees and specific cycle
  # Parameters : 
    # trees : integer or list of integer
      # see g.property('var') to choose trees
    # cycle : integer
      # which cycle you want, it could be cycle 4, cycle 5 or both. If you want both, cycle=None	"""
  # if type(trees)==int: trees = [trees]
  # date_ucs_trees_cycle = []
  # for tree in trees:
    # ucs_tree = [i for i in g.components_at_scale(tree,scale=4)]
    # if cycle == 4:
      # ucs_tree_cycle = [i for i,y in g.property('year').items() if y==4 and i in ucs_tree]
      # vegetative_ucs_tree_cycle = [i for i,c in g.property('code').items() if i in ucs_tree_cycle and c.split("/")[-1]!='F04']
    # elif cycle == 5:
      # ucs_tree_cycle = [i for i,y in g.property('year').items() if y==5 and i in ucs_tree]
      # vegetative_ucs_tree_cycle = [i for i,c in g.property('code').items() if i in ucs_tree_cycle and c.split("/")[-1]!='F05']
    # elif cycle == None:
      # ucs_tree_cycle = [i for i,y in g.property('year').items() if (y==4 or y==5) and i in ucs_tree]
      # vegetative_ucs_tree_cycle = [i for i,c in g.property('code').items() if i in ucs_tree_cycle and c.split("/")[-1]!='F04' and c.split("/")[-1]!='F05']
    # for i in vegetative_ucs_tree_cycle:
      # date_ucs_trees_cycle.append(g.property('date_burst')[i])
    # date = map(order_ucs_date,date_ucs_trees_cycle)
  # return date
# #date_ucs_cogshall_loaded_all_cycle = get_date_ucs_trees_cycle()


# def get_freq_date_ucs(trees = cogshall_loaded_trees, cycle = None):
  # """Return number of date burst of vegetative unit growth for given trees and given cycle, and return frequency of these dates.
    # Parameters : 
    # trees : integer or list of integer
      # see g.property('var') to choose trees
    # cycle : integer
      # which cycle you want, it could be cycle 4, cycle 5 or both. If you want both, cycle=None	"""
  # date_ucs = get_date_ucs_trees_cycle(trees,cycle)
  # N = len(date_ucs)
  # import collections
  # freq_date_ucs = dict( collections.Counter(date_ucs) )
  # return (N, freq_date_ucs)

# def histogramm_month_uc(LOADED = True, cycle = None, Proba = False):
  # """ """
  # import numpy as np
  # import matplotlib.pyplot as plt
  # if LOADED:
    # freq_ucs = get_freq_date_ucs(cycle = cycle)
    # if cycle == None:
      # freq_ucs[1]['03-08'] = 0
      # freq_ucs[1]['04-07'] = 0
    # elif cycle == 4:
      # freq_ucs[1]['03-08'] = 0
  # else:
    # freq_ucs = get_freq_date_ucs(cogshall_notloaded_trees,cycle)
    # if cycle == None:
      # freq_ucs[1]['03-06'] = 0
      # freq_ucs[1]['03-07'] = 0
      # freq_ucs[1]['03-08'] = 0
      # freq_ucs[1]['04-05'] = 0
      # freq_ucs[1]['04-06'] = 0
      # freq_ucs[1]['04-07'] = 0
    # elif cycle == 4:
      # freq_ucs[1]['03-06'] = 0
      # freq_ucs[1]['03-07'] = 0
      # freq_ucs[1]['03-08'] = 0
  # l = [date for date in freq_ucs[1] ]
  # label = list(np.sort(l))
  # freq = [freq_ucs[1][j] for j in label ]
  # pos = np.arange(len(label))
  # width = 1.0 
  # ax = plt.axes()
  # ax.set_xticks(pos + (width / 2))
  # ax.set_xticklabels(label)
  # if not Proba:
    # plt.bar(pos, freq, width, color='r')
    # return plt.show()
  # else:
    # N = float(freq_ucs[0])
    # probas = map(lambda x: x/N,freq)
    # plt.bar(pos, probas, width, color='b')
    # return plt.show()


