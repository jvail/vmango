from openalea.lpy import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

def run():
   """ """
   l = Lsystem('mango-asynchrony-glm2.lpy')
   l.derive()
   return l.name

path_file = "\\simulation_mangotree\\simulations_mangotree_glm\\glm_complet\\by_feature_loaded_tree\\"

def looprun(nb=1000):
   """ """
   for i in xrange(nb): 
      try :
         name = run()	
         import shutil
         shutil.copyfile( "mango_asynchrony_glm.bmtg" , share_dir+ path_file +name+"\\mango_"+name+"_asynchrony_{0:03d}.bmtg".format(i) )		 
      except :
         import traceback
         traceback.print_exc()
         pass      
#
looprun()
