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


Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }

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
#date_inflo_cogshall_loaded = get_date_inflo_trees(cogshall_loaded_trees)
#date_inflo_cogshall_notloaded = get_date_inflo_trees(cogshall_notloaded_trees)


#def get_freq_date_inflo_trees(trees = cogshall_trees):
#  """ """
#  if type(trees)== int: trees = [trees]
#  date_inflo = get_date_inflo_trees(trees)
#  import collections
#  freq_date_inflo = dict(collections.Counter(date_inflo))
#  return freq_date_inflo
#freq_date_inflo_cogshall_loaded = get_freq_date_inflo_trees(cogshall_loaded_trees)
#freq_date_inflo_cogshall_notloaded = get_freq_date_inflo_trees(cogshall_notloaded_trees)

date_weeks = {
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
12 : ('05-10-24','05-10-30')
}

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

#freq_weeks_inflo_cogshall_loaded = get_freq_date_inflo_per_week(cogshall_loaded_trees)
#freq_weeks_inflo_cogshall_notloaded = get_freq_date_inflo_per_week(cogshall_notloaded_trees)

#dump_obj((freq_weeks_inflo_cogshall_loaded,freq_weeks_inflo_cogshall_notloaded),'freq_weeks_inflo.pkl')

freq_weeks_inflo_cogshall_loaded,freq_weeks_inflo_cogshall_notloaded = load_obj('freq_weeks_inflo.pkl', share_dir+"//parameters_data_for_graphic_model//Cogshall//")

def simulation_week_inflo(LOADED = True):
  """Return an int which is the week after first august """
  if LOADED:
    freq_inflo = freq_weeks_inflo_cogshall_loaded
  else:
    freq_inflo = freq_weeks_inflo_cogshall_notloaded
  N = float(freq_inflo[0])
  freq = [freq_inflo[1][j] for j in xrange(13)]
  probas = map(lambda x: x/N,freq)
  from numpy import cumsum, random
  intervall_proba = cumsum(probas)
  uniform =  random.rand()
  for i,p in enumerate(intervall_proba):
    if uniform < p: 
      dweek = i
      break
  return dweek

  
  
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

#def min_date(list_date):
#  """ """
#  from datetime import date
#  list_years = [d.year for i,d in enumerate(list_date)]
#  min_year = min(list_years)
#  list_year_months = [d.month for i,d in enumerate(list_date) if d.year==min_year]
#  min_month = min(list_year_months)
#  list_year_month_days = [d.day for i,d in enumerate(list_date) if d.year==min_year and d.month==min_month]
#  min_day = min(list_year_month_days)
#  min_date = date(min_year,min_month,min_day)
#  return min_date