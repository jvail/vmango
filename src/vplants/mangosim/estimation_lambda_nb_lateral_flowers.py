share_dir = '../../../share/'

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

#####first fonction test
def cogshall_trees_by_year(year,trees = cogshall_trees):
  """It's a list of list """
  if type(trees) == int: trees = [trees]
  cogshall_trees_year = {}
  for tree in trees:
    ucs = g.components_at_scale(tree,scale=4)
    cogshall_trees_year[tree] = [i for i,y in g.property('year').items() if (y==year) and (i in ucs)]
  return cogshall_trees_year
##### return empty list for year == 3
#cycle03 = [i for i,year in g.property('year').items() if year==3]
#cogshall03 = [i for index,i in enumerate(g.components_at_scale(1,scale=4)) if i in cycle03]

##### second fonction : the good one because it return no empty list for cycle 03
def get_trees_by_year(year,trees = cogshall_trees):
  """That get a list of elements of trees and by year """
  if type(trees) == int: trees = [trees] 
  cogshall_trees_year = {}
  cycle_year = [i for i,y in g.property('year').items() if y == year]
  for tree in trees:
    cogshall_trees_year[tree] = [i for index,i in enumerate(g.components_at_scale(tree,scale=4)) if i in cycle_year]  
  return cogshall_trees_year

	
def convert_listoflist_2_list(listoflist):
  """Convert list of a list to an entire list"""
  list = []
  for i in listoflist:
    for j in range(len(listoflist[i])):
	  list.append(listoflist[i][j])
  return list

##### To verify if year means civil's year or cycle
def get_date_for_year(year,tree=1):
  tree_year = cogshall_trees_by_year(year,tree)[tree]
  date_year = [date for i,date in g.property('date_burst').items() if i in tree_year]
  return date_year
#date_04 = get_date_for_year(4)
##### return year of 2003 and 2004
#####=====>> year means cycle

def get_nb_inflo_lateral_by_year(year,variety='cogshall'):
  """To get number of lateral inflorescence for a year.
  Parameters: 
      year : cycle between year-1 and year
      variety : the variety of tree :
                 - 'cogshall'	  """
  if variety == 'cogshall':
    trees = get_trees_by_year(year)
  if type(trees) == list: trees = [[trees]]
  list_trees = convert_listoflist_2_list(trees)
  nb_inflo_lateral_year = [nb for i,nb in g.property('nb_inflo_l').items() if (i in list_trees) and (nb != '0') and (nb != '')]
  nb_inflo_lateral_year = map(int,nb_inflo_lateral_year)
  #index_no_number = [i for i,nb in g.property('nb_inflo_l').items() if (i in list_trees) and (nb == '')]
  return nb_inflo_lateral_year
##### Return sometimes no number ==> why!?
##### Problem : return empty list for year == 3
##### tried :  inflo_t03 = [nb for i,nb in g.property('nb_inflo_t').items() if i in cogshall03] 
##### but gives empty list
##### that normal because there are no informations for cycle 03

#nb_inflo_l_04 =  get_nb_inflo_lateral_by_year(4)
## len_no_number = len([i for i,nb in enumerate(nb_inflo_l_04) if nb==''])
## index04 = [i for i,nb in enumerate(nb_inflo_l_04) if nb=='']
## codes_no_number04 = [code for i,code in g.property('code').items() if i in index04]
#nb_inflo_l_05 = get_nb_inflo_lateral_by_year(5)


def get_histo_nb_inflo_lateral(data):
  """To have the frequency of each number of lateral inflorescence """
  import collections
  freq = collections.Counter(data)
  return freq
#keys = get_histo_nb_inflo_lateral(nb_inflo_l_04).keys()
#values = get_histo_nb_inflo_lateral(nb_inflo_l_04).values()  


##### To estimate the parameter lambda, we have to modifie the data :
#data_inflo_l_04 = [x-1 for x in nb_inflo_l_04]
#data_inflo_l_05 = [x-1 for x in nb_inflo_l_05]

#from numpy import average,var
#lambda04 = average(data_inflo_l_04)
#lambda05 = average(data_inflo_l_05)
##### To verify if mean is closed to variance:
#var04 = var(data_inflo_l_04)
#var05 = var(data_inflo_l_05)
##### It is not really, ==> in R, fit lambda with a glm and family="quasipoisson"
##### The cycle 04, il less fitting than the cycle 05. And the parameters are close, so we take the two cycles.

#data_inflo_l_2cycles = data_inflo_l_04 + data_inflo_l_05
#lambda_2cycles = average(data_inflo_l_2cycles)
##### to see a histogram of these data, cmd : ipython --pylab
##### then >>> 
#import matplotlib.pyplot as plt
#plt.figure()
#plt.hist(data_inflo_l_2cycles,normed=True)
#plt.title("Distribution of number of lateral inflorescences")
#plt.xlabel("Number of lateral inflorescences minus 1")
#plt.ylabel("Probability")
#plt.show()
#plt.axvline(lambda_2cycles , color = 'r', linestyle = "dashed", linewidth=2, label = "The parameter's value \n estimated")
#plt.legend()


#dump_obj(lambda_2cycles,"estimation_lambda_nb_lateral_flowers.pkl")

lambda_2cycles = load_obj("estimation_lambda_nb_lateral_flowers.pkl")