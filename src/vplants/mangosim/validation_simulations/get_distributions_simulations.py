import matplotlib.pyplot as plt
import numpy as np

from vplants.statistic.core.data.marginal.scalar import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")


def get_distribution(list, mtg_value, limit_IC, nb_bar = 10,title =u'Figure 1',subtitle = u'Distribution', xlim = [], ylim = [0,1], xlab= u'x', ylab= u'Probability'):
  """Return a ploting histogram from a list """
  plt.figure()
  if xlim == []:
    xlim = [min(list),max(list)]
  plt.hist(list,nb_bar,normed=True,color="b")
  plt.title(title)
  plt.suptitle(subtitle)
  plt.xlabel(xlab)
  plt.ylabel(ylab)
  plt.axis(xlim+ylim)
  plt.grid(True)
  plt.show()
  plt.axvline(mtg_value , color = 'r', linestyle = "dashed", linewidth=2, label = "Value expected \n from MTG")
  plt.axvline(limit_IC[0], color = 'g', linestyle = "dashed", linewidth=3, label ="IC 95 %" )
  plt.axvline(limit_IC[1], color = 'g', linestyle = "dashed", linewidth=3)
  plt.axvline(limit_IC[2], color = 'chartreuse', linestyle = "dashed", linewidth=3 ,label = "Median")
  plt.legend()


def get_limit_IC(list_to_sort,percentille = 5.0):
  """ """
  a = np.sort(list_to_sort)
  list_sort = list( a)
  length_list = len(list_to_sort)
  min_p = percentille/200.
  max_p = 1 - min_p
  min_ind , max_ind , median_ind = int(length_list*min_p) , int(length_list*max_p), length_list/2 
  return (list_sort[min_ind],list_sort[max_ind],list_sort[median_ind])





def convert_freq_dict_to_list(freq_dict):
  """ """
  list_freq_dict = []
  for key in freq_dict.keys():
    list_freq_dict += [key] * freq_dict[key]
  return list_freq_dict

def get_compared_distribution(freq_dict,mtg_list, title ="", xlab ="", ylab = "Probability "):
  """ """
  simulated_list = convert_freq_dict_to_list(freq_dict)
  plt.figure()
  plt.hist( [simulated_list,mtg_list],bins = tuple(freq_dict.keys() ) , normed=True, label= ("Simulation","MTG"))
  plt.title(title)
  plt.xlabel(xlab)
  plt.ylabel(ylab)
  plt.grid(True)
  plt.legend()
  plt.show()


#### Distribution of number of axes for tree B12
#get_compared_distribution(nb_axe_04,nb_axe_B12_04,
#  title = "Distribution of number of axes in Cycle 4 for tree B12", 
#  xlab="Number of axes in Cycle 4 ")

#get_compared_distribution(nb_axe_05,nb_axe_B12_05,
#  title = "Distribution of number of axes in Cycle 5 for tree B12", 
#  xlab="Number of axes in Cycle 5 ")

#### Distribution of number of children per unit growth for tree B12
#get_compared_distribution(nb_children_per_uc_04,nb_children_per_uc_B12_04,
#  title = "Distribution of number of children par unit growth \n in Cycle 4 for tree B12",
#  xlab = "Number of children per unit growth in Cycle 4")

#get_compared_distribution(nb_children_per_uc_05,nb_children_per_uc_B12_05,
#  title = "Distribution of number of children par unit growth \n in Cycle 5 for tree B12",
#  xlab = "Number of children per unit growth in Cycle 5")


def add_freq_dict(dict_a,dict_b):
  """ """
  freq_dict = dict( (key, dict_a.get(key, 0) + dict_b.get(key, 0)) for key in set(dict_a)|set(dict_b) )
  return freq_dict

#freq_dict_B12_simulate = add_freq_dict(monthly_date_ucs_04,monthly_date_ucs_05)
#freq_dict_B12_mtg = add_freq_dict(freq_monthly_date_ucs_B12_04,freq_monthly_date_ucs_B12_05)


def distribution_burst_date(freq_dict_simulate,freq_dict_mtg,title = "", xlab = "Burst date of unit growth  year-month", ylab = "Probability"):
  label = list(sort(list(set( freq_dict_simulate.keys()+freq_dict_mtg.keys() ))))
  freq_simulate = []
  for j in label:
    if j in freq_dict_simulate.keys():
      freq_simulate.append(freq_dict_simulate[j])
    else: freq_simulate.append(0)
  freq_mtg = []
  for j in label:
    if j in freq_dict_mtg:
      freq_mtg.append(freq_dict_mtg[j])
    else: freq_mtg.append(0)
  pos = np.arange(len(label))
  width = 1.0 
  ax = plt.axes()
  ax.set_xticks(pos + (width / 2))
  ax.set_xticklabels(label)
  N_simulate = float(sum(freq_dict_simulate.values()))
  N_mtg = float(sum(freq_dict_mtg.values()))
  probas_simulate = map(lambda x: x/N_simulate,freq_simulate)
  probas_mtg = map(lambda x: x/N_mtg,freq_mtg)
  plt.bar(pos, probas_simulate, width, color='r',alpha = 0.4,label="Simulation")
  plt.bar(pos, probas_mtg,width, color='b', alpha=0.4,label="Mtg")
  plt.title(title)
  plt.xlabel(xlab)
  plt.ylabel(ylab)
  plt.grid(True)
  plt.legend()
  return plt.show()

#### Distribution of burst date of unit growth
#distribution_burst_date(freq_dict_B12_simulate,freq_dict_B12_mtg,
#  title="Distribution of burst date of unit growth \n for tree B12 in both cycle")

# import pandas
#[max(data3[i]) for i in xrange(len(data2))]
#transformation
#then dataframe


