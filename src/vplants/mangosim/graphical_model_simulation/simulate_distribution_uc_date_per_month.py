from vplants.mangosim.tools import *
from vplants.mangosim.repository import *


import numpy as np


dict_proba_monthly_for_period = load_obj( dict_probas_monthly_for_period_cog_file )

def simulate_month_in_period(begin_flush_date,SELECT_TREE,name,LOADED):
  """
  """
  month = None
  period = [key for key, value in beg_end_period.items() if begin_flush_date.month in beg_end_period[key] ][0]
  if SELECT_TREE:
    probas_monthly = dict_proba_monthly_for_period[name][period]
  else :
    if LOADED :
      probas_monthly = dict_proba_monthly_for_period['C'][period]
    else:
      probas_monthly = dict_proba_monthly_for_period['NC'][period]
  proba_cumsum = list(np.cumsum(probas_monthly.values()))
  uniform =  np.random.rand() #print str(uniform)
  for ind in xrange(len(probas_monthly.keys())):
    if uniform < proba_cumsum[ind]: 
      month = probas_monthly.keys()[ind]
      break
  return month

