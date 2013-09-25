from datetime import date, timedelta
from random import randint
import numpy as np

from vplants.statistic import *
from vplants import mangostat
from openalea.deploy.shared_data import shared_data
path = shared_data(mangostat, share_path='share')
states = load(path/'mtbp'/'dag'/'not_thinned'/'no_mixture'/'cogshall.lst')





def get_dict_burst_period(list_name_states):
  """ 
  Return a dictionnary of burst period : keys are the name of the state and value is a tuple of start and end of period in this format : (add_year,month,day)
  This is for a date in the begining or cycle
  Parameters : 
    list of state names, names are in this format : "year-flush-GU_nature"
  """
  dict_burst_period = {}
  start_day = 1
  for name_state in list_name_states:
    if name_state=="V" or name_state=="F": continue
    if name_state[0]=="I":
      start_add_year = 0
    elif name_state[0]=="D":
      start_add_year = 1
    else:
      raise ValueError("No 'D' or 'I' in the name")
    if name_state[1]=="E":
      start_month, end_month = 7, 10
      end_day = 30
      start_year, end_year = start_add_year, start_add_year
    elif name_state[1]=="I":
      start_month, end_month = 11, 2
      end_day = 28
      start_year, end_year = start_add_year, start_add_year+1
    elif name_state[1]=="L":
      start_month, end_month = 3, 6
      end_day = 30
      start_year, end_year = start_add_year+1, start_add_year+1
    else : raise ValueError("No flush 'E','I' or 'L' ")
    dict_burst_period[name_state] = ((start_year,start_month,start_day) , (end_year,end_month,end_day))
  return dict_burst_period

burst_period = get_dict_burst_period(states)

def get_uniform_way(begin_flush_date,end_flush_date,current_date):
  """
  """
  # if current_date is before the period 
  if current_date < begin_flush_date : 
    begin_date = begin_flush_date # to add it to gd_mont_burst
  # if current date is in the period
  else :
    begin_date = current_date # must put condition to verify if current_date > end_flush_date and print error if true
  delta_weeks = (end_flush_date - begin_date).days/7 
  if delta_weeks > 6:
    gd_month_burst = randint(6,delta_weeks)
  else:
    gd_month_burst = 6
  date_sons = begin_date + timedelta(weeks=gd_month_burst)
  return date_sons

def get_gaussien_way(begin_flush_date,end_flush_date,current_date):
  """ 
  """
  date_sons = current_date
  if current_date < begin_flush_date :        # if current_date is before the period 
    begin_date = begin_flush_date             # to add it to gd_month_burst
  else :                                      # if current date is in the period
    begin_date = current_date                 # must put condition to verify if current_date > end_flush_date and print error if true
  d_weeks = (end_flush_date - begin_date).days/7
  weeks_father_sons = (date_sons - current_date).days/7
  if d_weeks >= 6 :
    mean_weeks = ( (end_flush_date - begin_flush_date).days/7 )/2
    std_weeks = 9
    while weeks_father_sons < 6:
      gd_month_burst = int(round( np.random.normal(mean_weeks,std_weeks,1) ) )
      date_sons = begin_date + timedelta(weeks=gd_month_burst)
      weeks_father_sons = (date_sons - current_date).days/7
  else:
    mean_weeks = 8
    std_weeks = 2
    while weeks_father_sons < 6:
      gd_month_burst = int(round( np.random.normal(mean_weeks,std_weeks,1) ) )
      date_sons = begin_date + timedelta(weeks=gd_month_burst)
      weeks_father_sons = (date_sons - current_date).days/7
  return date_sons

from distribution_uc_date_per_month import simulate_month_in_period

def get_empiric_distribution_way(begin_flush_date,current_date,SELECT_TREE,name,LOADED):
  """ 
  """
  if current_date > begin_flush_date : 
    begin_flush_date = current_date
  month_sons = simulate_month_in_period(begin_flush_date,SELECT_TREE,name,LOADED)
  if month_sons >=7 : 
    year_sons = begin_flush_date.year
  else : 
    year_sons = begin_flush_date.year +1
  date_sons = date(year_sons, month_sons, 15)
  return date_sons

def get_date(state, current_date, LAW_DATE, SELECT_TREE,name,LOADED):
  """ Return date of burst of sons for a given son's state 
  Parameters : 
    state : a string, state of the son
    current_date : a datetime.date, date of the morther
    GAUSSIEN : a boleen, for refinement the date in week's scale 
  """ 
  beg_date, end_date = burst_period[state]
  # construction of valide date
  # determination of which year we are 
  if 5 < current_date.month < 13 :
    begin_flush_date = date(current_date.year+beg_date[0],beg_date[1],beg_date[2])
    end_flush_date = date(current_date.year+end_date[0],end_date[1],end_date[2])
  else:
    begin_flush_date = date(current_date.year+beg_date[0]-1,beg_date[1],beg_date[2]) # = begin_flush_date - timedelta(days=366)
    end_flush_date = date(current_date.year+end_date[0]-1,end_date[1],end_date[2])
  #
  if LAW_DATE==0 :
    date_sons = get_uniform_way(begin_flush_date,end_flush_date,current_date)
  elif LAW_DATE==1: # else we have a normal distribution
    date_sons = get_gaussien_way(begin_flush_date,end_flush_date,current_date)
  else : 
    date_sons = get_empiric_distribution_way(begin_flush_date,current_date,SELECT_TREE,name,LOADED)
  return date_sons

vegetative , inflorescence = range(2)

def get_nature(state):
  """ return the nature of the father (vegetative or flowering) """
  if state[0]=="V": nature = vegetative
  elif state[0]=="F" : nature = inflorescence
  elif state[2]=="V": nature =  vegetative
  else: nature = inflorescence
  return nature



import itertools
def set_order_appearance(list_name_states):
  """Return the order of appearance of buds
  Parameter : 
    list_name_states : a list of strings, 
  """
  tuple_order_case = itertools.product("DI","LIE","VTL")
  order_case = ["".join(case) for case in list(tuple_order_case)]
  order_appearance = [o_case for o_case in order_case if o_case in list_name_states]
  return order_appearance



late2early = set_order_appearance(states)
early2late = list(reversed(late2early))

def get_apical_state(sons):
  """It gives the state which will be place in apical position and gives liste of states in lateral position """
  apical_state, lateral_position_states = (None, [])
  if sum(sons) != 0 : 
    dict_sons = dict(zip(states,sons))
    for a_state in early2late:
      if dict_sons[a_state] > 0:
        apical_state = a_state
        break
    lateral_position_states = [state for state in late2early if (state != apical_state and dict_sons[state] > 0)]
  return (apical_state, lateral_position_states)



# import numpy as np

def get_nb_lateral_flowers(state):
  """For an inflorescence state, it gives the number of lateral flowers. 
  """
  if state[0]=="V" or state[0]=="F":
    nb_lateral_flowers = 0
  elif state[2]=="T": # only one flower in apical position
    nb_lateral_flowers = 0
  elif state[2]=="L": # flowers in lateral position
    nb_lateral_flowers = np.random.poisson(1.02,1)[0] + 1 # parameter lambda=1.02 where estimated by the mtg
  else: # there are no flowers
    nb_lateral_flowers = 0
  return nb_lateral_flowers


from distribution_inflo_date_per_week import simulation_week_inflo

def get_date_flowers(current_date,LOADED):
  """it gives a date of burst flowers """
  if 5 < current_date.month < 13:
    begin_flowering_date = date(current_date.year+1,8,1)
    end_flowering_date = date(current_date.year+1,10,30)
  #elif 2 < current_date.month < 6: # if the apical is too young, it must be around 7 months before the begin period of flowering
  #  begin_flowering_date = date(current_date.year+2,7,1)
  #  end_flowering_date = date(current_date.year+2,8,1)
  else:
    begin_flowering_date = date(current_date.year,8,1) # = begin_flush_date - timedelta(days=366)
    end_flowering_date = date(current_date.year,10,30)
  dweek = simulation_week_inflo(LOADED)
  flowers_date = begin_flowering_date + timedelta(weeks=dweek)
  return flowers_date


