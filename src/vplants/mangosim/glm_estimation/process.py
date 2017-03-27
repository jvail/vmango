from get_data_for_glm import main as get_data_for_glm
#get_data_for_glm()

rscript = 'estimate_nb_glm_order_1_proba.r'
try:
  import rpy2.robjects as r
  r.r("source('%s')" % rscript)
except:
    import os
    os.system("R BATCH -F %s" % rscript)

from report_estimation import process as generate_report
generate_report(True, False)

