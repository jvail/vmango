import os, sys, datetime, random, numpy
from timeit import default_timer as timer
import pandas as pd
import numpy as np
from openalea.vmango.simulation.fruitmodel.r_vs_py_validation.run import run as run_py
from openalea.vmango.simulation.fruitmodel.fruitmodel_error import FruitModelValueError

verbose = False
result = pd.DataFrame({
    'run': [],
    'bloom_date': [],
    'nb_fruits': [],
    'nb_leaves': [],
    'ms_init': [],
    'k1_fruit_sample': [],
    'time_r': [],
    'time_py': [],
    'ripe': [],
    'model_error': [],
    'result': []
})

os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))

if sys.platform == 'win32':
    R_HOME = os.environ['R_HOME']
    exe = os.path.join(R_HOME, 'bin', 'Rscript.exe')
    assert os.path.exists(exe)
else:
    exe = 'Rscript'

def run_r(bloom_date, nb_fruits, nb_leaves, verbose, MS_Init, k1_fruit_sample):
    os.system(f'"{exe}" r_vs_py_validation/run.r {os.getcwd()} {bloom_date} {nb_fruits} {nb_leaves} {verbose} {ms_init} {k1_fruit_sample}')

for i in range(5):

    time_r = 0
    time_py = 0

    if os.path.exists('tmp/r.csv'):
        os.remove('tmp/r.csv')
    if os.path.exists ('tmp/py.csv'):
        os.remove('tmp/py.csv')

    bloom_date = '01/11/2002'
    nb_fruits = random.randrange(1, 4)
    nb_leaves = random.randrange(10, 20)
    ms_init = 0.97 * numpy.random.normal(13.9, 4.1) + 0.03 * numpy.random.normal(29.2, 0.66)
    k1_fruit_sample = random.randrange(0, 5)
    model_error = ''
    ripe = False

    try:
        start = timer()
        run_r(bloom_date, nb_fruits, nb_leaves, verbose, ms_init, k1_fruit_sample + 1)
        end = timer()
        time_r = end - start

        start = timer()
        run_py(bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, verbose=verbose)
        end = timer()
        time_py = end - start
    except FruitModelValueError as e:
        print(e)
        model_error = str(e)
        pass
    finally:
        if os.path.exists('tmp/r.csv') and os.path.exists('tmp/py.csv'):
            df_r = pd.read_csv('tmp/r.csv', sep=',').fillna(-99999).round(4)
            ripe = df_r.loc[df_r.shape[0] - 2, 'Saccharose'] >= 0.04 if not model_error else False
            df_py = pd.read_csv('tmp/py.csv', sep=',').fillna(-99999).round(4)
            eq = df_r.eq(df_py)
            is_eq = eq[eq == False].count().sum() == 0
            if is_eq:
                print(i, bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, time_r, time_py, ripe, model_error, is_eq)
            else:
                print(i, bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, time_r, time_py, ripe, model_error, is_eq)
                df_r.to_csv('r_vs_py_validation/df_r.csv', index=False)
                df_py.to_csv('r_vs_py_validation/df_py.csv', index=False)
                df_diff = df_r.loc[:, 'Masse_Fruit':] - df_py.loc[:, 'Masse_Fruit':]
                flat_diff = df_diff[df_diff > 0.001].values.flatten()
                if len(flat_diff[~np.isnan(flat_diff)]) > 0:
                    df_diff.to_csv('r_vs_py_validation/df_py_diff.csv', index=False)
                    print(i, bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, time_r, time_py, ripe, model_error, is_eq)
                    pd.options.display.max_rows = None
                    pd.options.display.max_columns = None
                    print(df_diff)
                    pd.options.display.max_rows = 10
                    pd.options.display.max_columns = 10
                    break
        else:
            is_eq = False

        result.loc[len(result)] = [i, bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, time_r, time_py, ripe, model_error, is_eq]

result.to_csv('r_vs_py_validation/r_vs_py.csv', na_rep='nan', index=False)
