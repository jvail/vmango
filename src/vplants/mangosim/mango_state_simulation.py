from datetime import date, timedelta
from random import randint
import numpy as np

burst_period = { # for a date in begining of cycle
   0 : ((0,5,1) , (0,6,30) ),
   1 : ((0,11,1) , (1,2,28) ),
   2 : ((1,3,1) , (1,5,31) ),
   3 : ((1,7,1) , (1,10,31) ),
   4 : ((1,11,1) , (2,2,28) ),
   5 : ((2,3,1) , (2,5,31) ),
   6 : ((0,7,1) , (0,9,30) ),
   7 : ((0,11,1) , (1,2,28) ),
   8 : ((1,3,1) , (1,5,31) ),
   9 : ((1,11,1) , (2,2,28) ),
   10 : ((2,3,1) , (2,5,31) ),
   11 : ((0,11,1) , (1,2,28) ),
   12 : ((1,11,1) , (2,2,28) )
}


def get_date(state, current_date, GAUSSIEN):
  """ return date of burst of sons for a given son's state """ 
  beg_date, end_date = burst_period[state]
  # construction of valide date
  # determination of which year we are 
  if 5 < current_date.month < 13 or state == 0 or state == 6:
    begin_flush_date = date(current_date.year+beg_date[0],beg_date[1],beg_date[2])
    end_flush_date = date(current_date.year+end_date[0],end_date[1],end_date[2])
  else:
    begin_flush_date = date(current_date.year+beg_date[0]-1,beg_date[1],beg_date[2]) # = begin_flush_date - timedelta(days=366)
    end_flush_date = date(current_date.year+end_date[0]-1,end_date[1],end_date[2])
  
  if not GAUSSIEN :
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
  
  else: # else we have a normal distribution
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

vegetative , inflorescence = range(2)

def get_nature(state):
  """ return the nature of the father (vegetative or flowering) """
  # if the state is upper than 5, it's an inflorescence
  if state > 5:
    nature = inflorescence
  else:
    nature = vegetative
  return nature



import copy

early2late = [6,0,11,7,1,8,2,3,12,9,4,10,5]

def get_apical_state(sons):
  """It gives the state which will be place in apical position and gives liste of states in lateral position """
  if sum(sons) == 0 : return None, []
  
  for a_state in early2late:
    if sons[a_state] > 0:
      apical_state = a_state
      break
  
  lateral_position_states = [state for state in reversed(early2late) if state != apical_state and sons[state] > 0]
  return (apical_state, lateral_position_states)



# import numpy as np

def get_nb_lateral_flowers(state):
  """For an inflorescence state, it gives the number of lateral flowers. !!!Be carfull, the state 6 wasn't considered !!! """
  if 6 < state <= 10: # only one flower in apical position
    nb_lateral_flowers = 0
  elif 11 <= state <= 12: # flowers in lateral position
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


