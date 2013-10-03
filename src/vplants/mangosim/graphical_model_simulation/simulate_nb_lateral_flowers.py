from vplants.mangosim.tools import *
from vplants.mangosim.repository import *

import numpy as np

lambda_2cycles = load_obj( estimation_lambda_nb_lateral_flowers_file ) 

def get_nb_lateral_flowers(state):
  """For an inflorescence state, it gives the number of lateral flowers. 
  """
  if state[0]=="V" or state[0]=="F":
    nb_lateral_flowers = 0
  elif state[2]=="T": # only one flower in apical position
    nb_lateral_flowers = 0
  elif state[2]=="L": # flowers in lateral position
    nb_lateral_flowers = np.random.poisson( lambda_2cycles,1)[0] + 1 # parameter lambda=1.02 where estimated by the mtg
  else: # there are no flowers
    nb_lateral_flowers = 0
  return nb_lateral_flowers

