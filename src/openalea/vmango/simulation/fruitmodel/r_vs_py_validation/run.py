import random as rdm
import pandas as pd
import numpy as np
from io import StringIO
import datetime
from openalea.vmango.simulation.fruitmodel.fruit_model_main import growth_main

def run(bloom_date, nb_fruits, nb_leaves, DM_fruit_0, sunlit_bs, input_hourly, input_daily, verbose=False):
    bloom_date = np.datetime64(datetime.datetime.strptime(bloom_date, '%d/%m/%Y')).astype('datetime64[D]')
    res = growth_main(bloom_date, nb_fruits, nb_leaves, DM_fruit_0, sunlit_bs, input_hourly, input_daily)
    res.to_csv('tmp/py.csv', na_rep='nan', index=False)
