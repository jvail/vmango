from vplants.mangosim.tools import *
from vplants.mangosim.repository import *

   
freq_weeks_inflo_cogshall_loaded,freq_weeks_inflo_cogshall_notloaded = load_obj(freq_weeks_inflo_file)

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

  
