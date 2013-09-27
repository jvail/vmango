from openalea.lpy import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

def run():
   """ """
   l = Lsystem('mango-asynchrony-model-null.lpy')
   l.derive()
   return l.name

from os.path import join
path_file = join("simulation_mangotree","simulations_mangotree_model_null","by_all_trees"

def looprun(nb=1000):
   """ """
   for i in xrange(nb): 
      try :
         name = run()
         outdir = join(path_file,name)
         import shutil, os
         if not os.path.exists(outdir):
            os.makedirs(outdir)
         shutil.copyfile( "mango_asynchrony_model_null.bmtg" , join( share_dir, path_file , name ,"mango_"+name+"_asynchrony_{0:03d}.bmtg".format(i)) )		 
      except :
         import traceback
         traceback.print_exc()
         pass      
#
if __name__ == '__main__' :
   looprun()
