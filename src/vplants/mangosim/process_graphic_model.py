from openalea.lpy import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")
from traceback import print_exc

def run():
   """ """
   param = { 'EXPORT_MTG' : False, 'TREE' : 2}
   l = Lsystem('mango-asynchrony-model-graphic2.lpy')#l.EXPORT_MTG = True    #l.updateNamespace(param)
   l.derive()
   return l.name

path_file = "\\simulation_mangotree\\simulations_mangotree_graphic_model\\nothinned_nomixture\\"


def looprun(nb=1000):
   """ """
   for i in xrange(nb): 
      try :
         name = run()	
         import shutil
         shutil.copyfile( "mango_asynchrony_graphic_model.bmtg" , share_dir + path_file + name + "/mango_" +name + "_asynchrony_{0:03d}.bmtg".format(i) )		 
      except :
         import traceback
         traceback.print_exc()
         pass
#
looprun()
