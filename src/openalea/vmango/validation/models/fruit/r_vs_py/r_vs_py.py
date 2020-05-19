import os, sys, datetime, random
from timeit import default_timer as timer
import pandas as pd
import numpy as np

from openalea.vmango.simulation.fruitmodel.fruit_model_main import growth_main
from openalea.vmango.simulation.fruitmodel.fruitmodel import initialize_input
from openalea.vmango.simulation.fruitmodel.fruitmodel_error import FruitModelValueError

verbose = False
result = pd.DataFrame({
    'run': [],
    'bloom_date': [],
    'nb_fruits': [],
    'nb_leaves': [],
    'DM_fruit_0': [],
    'sunlit_bs_sample': [],
    'time_r': [],
    'time_py': [],
    'ripe': [],
    'model_error': [],
    'result': []
})

absdir = os.path.dirname(os.path.abspath(__file__))
os.chdir(os.path.join(absdir, '..'))

weather_hourly_file_path = os.path.join(absdir, '../../../../../../../share/environment/weather_hourly_stpierre_2002.csv')
weather_daily_file_path = os.path.join(absdir, '../../../../../../../share/environment/weather_daily_stpierre_2002.csv')
sunlit_fractions_file_path = os.path.join(absdir, '../../../../../../../share/environment/sunlit_fractions.csv')
params_file_path = os.path.join(absdir, '../../../../../../../share/parameters/fruitmodel/cogshall.toml')

input_hourly, input_daily, sunlit_fractions, params = initialize_input(
            weather_hourly_file_path, weather_daily_file_path, sunlit_fractions_file_path, params_file_path)

if sys.platform == 'win32':
    R_HOME = os.environ['R_HOME']
    exe = os.path.join(R_HOME, 'bin', 'Rscript.exe')
    assert os.path.exists(exe)
else:
    exe = 'Rscript'

def run_r(bloom_date, nb_fruits, nb_leaves, verbose, DM_fruit_0, light_envir_sample, weather_fname="stpierre_2002"):
    os.system(f'"{exe}" r_vs_py/run.r {os.getcwd()} {bloom_date} {nb_fruits} {nb_leaves} {verbose} {DM_fruit_0} {light_envir_sample} {weather_fname}')

times = []

for i in range(100):

    time_r = 0
    time_py = 0

    if os.path.exists('tmp/r.csv'):
        os.remove('tmp/r.csv')
    if os.path.exists ('tmp/py.csv'):
        os.remove('tmp/py.csv')

    bloom_date = '01/11/2002'
    nb_fruits = random.randrange(1, 4)
    nb_leaves = random.randrange(10, 20)
    weight_1 = params.main.fruitDM0_weight_1
    mu_1 = params.main.fruitDM0_mu_1
    sigma_1 = params.main.fruitDM0_sigma_1
    weight_2 = params.main.fruitDM0_weight_2
    mu_2 = params.main.fruitDM0_mu_2
    sigma_2 = params.main.fruitDM0_sigma_2

    DM_fruit_0 = weight_1 * np.random.normal(mu_1, sigma_1) + weight_2 * np.random.normal(mu_2, sigma_2)
    sunlit_bs_sample = random.randrange(0, 5)
    sunlit_bs = sunlit_fractions.iloc[:,sunlit_bs_sample].to_numpy()
    model_error = ''
    ripe = False

    try:
        start = timer()
        run_r(bloom_date, nb_fruits, nb_leaves, verbose, DM_fruit_0, sunlit_bs_sample + 1)
        end = timer()
        time_r = end - start

        start = timer()
        bloom_date_ = np.datetime64(datetime.datetime.strptime(bloom_date, '%d/%m/%Y'), 'D')
        res = growth_main(
            bloom_date_,
            nb_fruits,
            nb_leaves,
            sunlit_bs,
            input_daily,
            input_hourly,
            params,
            DM_fruit_0,
            DM_fruit_ini=np.nan,
            sim_date_ini=None,
            dd_thresh=np.nan,
            stop_sim_ddcum=np.nan,
            verbose=verbose
        )
        res.to_csv('tmp/py.csv', na_rep='nan', index=False)

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
            ripe = df_r.loc[df_r.shape[0] - 2, 'sucrose'] >= 0.04 if not model_error else False
            df_py = pd.read_csv('tmp/py.csv', sep=',').fillna(-99999).round(4)
            eq = df_r.eq(df_py)
            is_eq = eq[eq == False].count().sum() == 0
            if is_eq:
                print(i, bloom_date, nb_fruits, nb_leaves, DM_fruit_0, sunlit_bs_sample, time_r, time_py, ripe, model_error, is_eq)
            else:
                print(i, bloom_date, nb_fruits, nb_leaves, DM_fruit_0, sunlit_bs_sample, time_r, time_py, ripe, model_error, is_eq)
                df_r.to_csv('r_vs_py/df_r.csv', index=False)
                df_py.to_csv('r_vs_py/df_py.csv', index=False)
                df_diff = df_r.loc[:, 'FM_fruit':] - df_py.loc[:, 'FM_fruit':]
                flat_diff = df_diff[abs(df_diff) > 0.001].values.flatten()
                if len(flat_diff[~np.isnan(flat_diff)]) > 0:
                    df_diff.to_csv('r_vs_py/df_py_diff.csv', index=False)
                    pd.options.display.max_rows = None
                    pd.options.display.max_columns = None
                    print(df_diff)
                    pd.options.display.max_rows = 10
                    pd.options.display.max_columns = 10
                    break
        else:
            is_eq = False

        result.loc[len(result)] = [i, bloom_date, nb_fruits, nb_leaves, DM_fruit_0, sunlit_bs_sample, time_r, time_py, ripe, model_error, is_eq]

result.to_csv('r_vs_py/r_vs_py.csv', na_rep='nan', index=False)

print('py/r', np.mean([t[1]/t[0] for t in times]))
