import os, sys, datetime, random, numpy
from timeit import default_timer as timer
import pandas as pd
import numpy as np

from openalea.vmango.simulation.fruitmodel.r_vs_py_validation.run import run as run_py, run_v2 as run_py_v2
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

absdir = os.path.dirname(os.path.abspath(__file__))
os.chdir(os.path.join(absdir, '..'))

weather_hourly_file_path = os.path.join(absdir, '../../../../../../share/environment/rayostpierre2002.csv')
weather_daily_file_path = os.path.join(absdir, '../../../../../../share/environment/tempstpierre2002.csv')

def initialize_input():
    weather_hourly = pd.read_csv(weather_hourly_file_path,
        sep=';', parse_dates=['Date'], dayfirst=True, usecols=['HEURE', 'Rayonnement', 'Temperature_Air', 'HR', 'Date'])
    weather_daily = pd.read_csv(weather_daily_file_path,
        sep=';', parse_dates=['DATE'], dayfirst=True, usecols=['DATE', 'TM'])

    weather_hourly.rename(columns={'Date':'DATE'}, inplace=True)
    weather_hourly['DATE'] = weather_hourly['DATE'].astype('datetime64[D]')
    weather_daily['DATE'] = weather_daily['DATE'].astype('datetime64[D]')

    weather = weather_daily.merge(weather_hourly, on='DATE')
    weather.sort_values(['DATE', 'HEURE'], inplace=True)

    weather_hour_count = weather.groupby(['DATE']).count()

    if len(weather_hour_count[weather_hour_count['HEURE'] != 24].values) > 0:
        print('Input data has days with less than 24 h')

    input_hourly = pd.DataFrame(weather[['DATE', 'HEURE', 'Rayonnement', 'Temperature_Air', 'HR']])
    input_daily = pd.DataFrame(weather[['DATE', 'TM']].iloc[::24].reset_index(drop=True))
    return (input_hourly, input_daily)

input_hourly, input_daily = initialize_input()

if sys.platform == 'win32':
    R_HOME = os.environ['R_HOME']
    exe = os.path.join(R_HOME, 'bin', 'Rscript.exe')
    assert os.path.exists(exe)
else:
    exe = 'Rscript'

def run_r(bloom_date, nb_fruits, nb_leaves, verbose, MS_Init, k1_fruit_sample):
    os.system(f'"{exe}" r_vs_py_validation/run.r {os.getcwd()} {bloom_date} {nb_fruits} {nb_leaves} {verbose} {ms_init} {k1_fruit_sample}')

times = []

for i in range(10):

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
        # run_py(bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, verbose=verbose)
        run_py_v2(bloom_date, nb_fruits, nb_leaves, ms_init, k1_fruit_sample, input_hourly, input_daily, verbose=verbose)
        end = timer()
        time_py = end - start

        times.append([time_r, time_py])

    except FruitModelValueError as e:
        end = timer()
        time_py = end - start
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
                flat_diff = df_diff[abs(df_diff) > 0.001].values.flatten()
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

print('py/r', np.mean([t[1]/t[0] for t in times]))
