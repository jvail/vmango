from vplants.mangosim.tools import *
from vplants.mangosim.repository import *



def order_date(string):
  """Put an order in date.
  Parameter : 
    string = day-month-year  
  Return : 
    year-month-day which month is an integer"""
  d,m,y = string.split('-')
  if Month[m] > 9: month = str(Month[m]) 
  else: month = '0'+str(Month[m])
  order_date = y + '-' + month + '-' + d
  return order_date

def get_date_inflo_trees(trees = cogshall_trees):
  """Return a list of inflorescence date burst for trees and from cycle 05 """
  if type(trees)== int: trees = [trees]
  date_inflo_05 = []
  for tree in trees:
    ucs_tree = [i for i in g.components_at_scale(tree,scale=4)]
    flush_tree = [i for i,f in g.property('flush05').items() if i in ucs_tree]
    code_flush_tree = [(i,c) for i,c in g.property('code').items() if i in flush_tree]
    for j in xrange(len(code_flush_tree) ) :
      splitcode = code_flush_tree[j][1].split("/")
      if splitcode[-1]=='F05':
        line_inflo_05 = code_flush_tree[j][0]
        if g.property('date_flo')[line_inflo_05]!='':
          date_inflo_05.append(g.property('date_flo')[line_inflo_05])
  date_inflo = map(order_date,date_inflo_05)
  return date_inflo



#def get_freq_date_inflo_trees(trees = cogshall_trees):
#  """ """
#  if type(trees)== int: trees = [trees]
#  date_inflo = get_date_inflo_trees(trees)
#  import collections
#  freq_date_inflo = dict(collections.Counter(date_inflo))
#  return freq_date_inflo
#freq_date_inflo_cogshall_loaded = get_freq_date_inflo_trees(cogshall_loaded_trees)
#freq_date_inflo_cogshall_notloaded = get_freq_date_inflo_trees(cogshall_notloaded_trees)


def get_freq_date_inflo_per_week(trees = cogshall_trees):
  """Return the number of inflorescence and the frequency of inflorescences dates. """
  if type(trees)== int: trees = [trees]
  date_inflo = get_date_inflo_trees(trees)
  N = len(date_inflo)
  freq_weeks = {0:0,1:0,2:0,3:0,4:0,5:0,6:0,7:0,8:0,9:0,10:0,11:0,12:0}
  for i,d in enumerate(date_inflo):
    for j in xrange(13):
      if date_weeks[j][0] <= d <= date_weeks[j][1]:
        freq_weeks[j]+=1
  return (N,freq_weeks)



def estimate() :
    """
    """
    g = load_mtg()
    features_names = g.property_names()
    cogshall_trees = [i for i, v in g.property('var').items() if v == 'cogshall']
    fruit_loaded_prop = g.property('fr_load') 
    cogshall_notloaded_trees = [i for i in cogshall_trees if fruit_loaded_prop[i] == 'NC']
    cogshall_loaded_trees = [i for i in cogshall_trees if fruit_loaded_prop[i] == 'C']
    #ucs_cogshall_B14 = [i for i in g.components_at_scale(1,scale=4)]
    #flush_B14 = [i for i,f in g.property('flush05').items() if i in ucs_cogshall_B14]
    #code_flush_B14 = [(i,c) for i,c in g.property('code').items() if i in flush_B14]
    #inflo_05_B14 = []
    #for j in xrange(len(code_flush_B14) ) :
    #  splitcode = code_flush_B14[j][1].split("/")
    #    if splitcode[-1]=='F05':
    #      inflo_05_B14.append(code_flush_B14[j][0])
    #date_inflo_05 = [d for i,d in g.property('date_flo').items() if i in inflo_05_B14]
    #######one missing date
    ####### to check if ok
    #code_inflo_05 = [c for i,c in g.property('code').items() if i in inflo_05_B14]
    
    #date_inflo_cogshall_loaded = get_date_inflo_trees(cogshall_loaded_trees)
    #date_inflo_cogshall_notloaded = get_date_inflo_trees(cogshall_notloaded_trees)
    
    freq_weeks_inflo_cogshall_loaded = get_freq_date_inflo_per_week(cogshall_loaded_trees)
    freq_weeks_inflo_cogshall_notloaded = get_freq_date_inflo_per_week(cogshall_notloaded_trees)
    dump_obj((freq_weeks_inflo_cogshall_loaded,freq_weeks_inflo_cogshall_notloaded), freq_weeks_inflo_file )
    
if __name__=="__main__":
    print "estimation de la distribution du nombre d'inflorescences par semaine"
    estimate()
   
freq_weeks_inflo_cogshall_loaded,freq_weeks_inflo_cogshall_notloaded = load_obj( freq_weeks_inflo_file )


  
  
def get_label(string):
  y,m,d = string.split('-')
  label = d + '-' + m
  return label

def histogramm_week_inflo(LOADED = True, Proba = False):
  import numpy as np
  import matplotlib.pyplot as plt
  if LOADED:
    freq_inflo = freq_weeks_inflo_cogshall_loaded
  else:
    freq_inflo = freq_weeks_inflo_cogshall_notloaded
  freq = [freq_inflo[1][j] for j in xrange(13)]
  label = [get_label(date_weeks[j][1]) for j in xrange(13)]
  pos = np.arange(len(label))
  width = 1.0 
  ax = plt.axes()
  ax.set_xticks(pos + (width / 2))
  ax.set_xticklabels(label)
  if not Proba:
    plt.bar(pos, freq, width, color='r')
  else:
    N = float(freq_inflo[0])
    probas = map(lambda x: x/N,freq)
    plt.bar(pos, probas, width, color='b')
  plt.title("Distribution of burst date of inflorescences")
  plt.xlabel("""Dates : day-month""")
  plt.ylabel("Probability")
  return plt.show()

