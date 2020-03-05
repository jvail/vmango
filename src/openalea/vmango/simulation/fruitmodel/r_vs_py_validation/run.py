import random as rdm
import datetime
from openalea.vmango.simulation.fruitmodel.fruit_model_main import fruit_model_main

def run(bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, verbose=False):
    bloom_date = datetime.datetime.strptime(bloom_date, '%d/%m/%Y').date()
    res = fruit_model_main(bloom_date, nb_fruits, nb_leaves, verbose=verbose, MS_Init=ms_init, k1_fruit_sample=k1_fruit_sample)
    # res.to_csv('tmp/py.csv', na_rep='99999', index=False, float_format='%.8f')
    res.to_csv('tmp/py.csv', na_rep='nan', index=False)
