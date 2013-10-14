from openalea.lpy import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")
from os.path import join

def run():
   """ """
   l = Lsystem('mango-asynchrony-glm2.lpy')
   l.EXPORT_MTG = True
   if l.GLM_SELECTED :
      path_glm = "glm_selected"
   else :
      path_glm = "glm_complet"
   if l.LOADED_FACTOR :
      path_choice = "by_all_trees"
   else :
      if l.SELECT_TREE :
         path_choice = "by_tree"
      else : 
         path_choice = "by_feature_loaded_tree"
   path_file = join(share_dir,"simulation_mangotree","simulations_mangotree_glm",path_glm,path_choice)
   l.derive()
   return l.name, path_file


def looprun(nb=1000):
   """ """
   for i in xrange(nb): 
      try :
         name, path_file = run()
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
