import os
from openalea.vmango.preparation.glm_estimation.get_data_for_glm import main as get_data_for_glm
import rpy2.robjects as r

os.chdir(
    os.path.join(
        os.path.abspath(__file__),
        '../../src/openalea/vmango/preparation/glm_estimation'
    )
)

# generate input files located at glm_estimate_input/{cultivar} for prob. tables
get_data_for_glm()

# generate prob. tables at glm_output_proba/{cultivar}
r.r(f"source('estimate_nb_glm_order_1_proba.r')")
