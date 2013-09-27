from openalea.lpy import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

def run():
   """ """
   l = Lsystem('mango-asynchrony-glm2.lpy')
   l.derive()
   return l.name

from os.path import join
path_file = join(share_dir,"simulation_mangotree","simulations_mangotree_glm","glm_complet","is_loaded_as_factor")

def looprun(nb=1000):
   """ """
   for i in xrange(nb): 
      try :
         name = run()
         outdir = join(path_file,name)
         import shutil, os
         if not os.path.exists(outdir):
            os.makedirs(outdir)
         shutil.copyfile( "mango_asynchrony_glm.bmtg" , join(outdir,"mango_"+name+"_asynchrony_{0:03d}.bmtg".format(i)) )		 
      except :
         import traceback
         traceback.print_exc()
         pass      
#
if __name__ == '__main__' :
   looprun()
