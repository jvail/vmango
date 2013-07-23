from openalea.lpy import *
share_dir = "../../../share/"

def run():
   """ """
   l = Lsystem('mango-asynchrony-model-pierre2.lpy')
   l.derive()


def looprun(nb=1000):
   """ """
   nb_ucs_04 = []
   nb_ucs_05 = []
   nb_ucs_04_in_extremity = []
   nb_ucs_05_in_extremity = []
   nb_ucs_04_in_extremity_apical_position = []
   nb_ucs_05_in_extremity_apical_position = []
   terminal_apical_rate_04 = []
   terminal_apical_rate_05 = []
   nb_axe_04 = []
   nb_axe_05 = []
   nb_children_per_uc_04 = []
   nb_children_per_uc_05 = []
   nb_descendant_per_axe_04 = []
   nb_descendant_per_axe_05 = []
   nb_uc_giving_inflorescence_04 = []
   nb_uc_giving_inflorescence_05 = []
   flowering_rate_04 = []
   flowering_rate_05 = []
   monthly_date_ucs_04 = []
   monthly_date_ucs_05 = []
   for i in xrange(nb): 
      try :
         run()	
         import shutil
         shutil.copyfile( "mango_asynchrony_pierre.bmtg" , share_dir+"simulations_mangotree_model_pierre/Cogshall_B12/mango_B12_asynchrony_pierre_{0:03d}.bmtg".format(i) )		 
      except :
         pass
      import collections
      import vplants.mangosim.validation_simulations.get_info_simulate_mtg as gism
      reload(gism)
      nb_ucs_04.append(len( gism.ucs[4] ))
      nb_ucs_05.append(len( gism.ucs[5] ))
      nb_ucs_04_in_extremity.append(len( gism.ucs_in_extremity[4] ))
      nb_ucs_05_in_extremity.append(len( gism.ucs_in_extremity[5] ))
      nb_ucs_04_in_extremity_apical_position.append( gism.nb_ucs_in_extremity_apical_position[4] )
      nb_ucs_05_in_extremity_apical_position.append( gism.nb_ucs_in_extremity_apical_position[5] )
      terminal_apical_rate_04.append( gism.terminal_apical_rate[4] )
      terminal_apical_rate_05.append( gism.terminal_apical_rate[5] )
      nb_uc_giving_inflorescence_04.append( gism.nb_uc_giving_inflorescence[4] )
      nb_uc_giving_inflorescence_05.append( gism.nb_uc_giving_inflorescence[5] )
      flowering_rate_04.append( gism.flowering_rate[4] )
      flowering_rate_05.append( gism.flowering_rate[5] )
      nb_axe_04.append( gism.nb_axe_04 )
      nb_axe_05.append( gism.nb_axe_05 ) 
      nb_children_per_uc_04.append( gism.nb_children_per_uc[4] )
      nb_children_per_uc_05.append( gism.nb_children_per_uc[5] )
      nb_descendant_per_axe_04.append( gism.nb_descendant_per_axe[4] )
      nb_descendant_per_axe_05.append( gism.nb_descendant_per_axe[5] )
      monthly_date_ucs_04.append( gism.monthly_date_ucs[4] )
      monthly_date_ucs_05.append( gism.monthly_date_ucs[5] )
   return (nb_ucs_04 , nb_ucs_05 , 
      nb_ucs_04_in_extremity , nb_ucs_05_in_extremity , 
      nb_ucs_04_in_extremity_apical_position, nb_ucs_05_in_extremity_apical_position ,
      terminal_apical_rate_04 , terminal_apical_rate_05 , 
      nb_uc_giving_inflorescence_04 , nb_uc_giving_inflorescence_05 , 
      flowering_rate_04 , flowering_rate_05 , 
      nb_axe_04 , nb_axe_05 , 
      nb_children_per_uc_04 , nb_children_per_uc_05 , 
      nb_descendant_per_axe_04,nb_descendant_per_axe_05,
      monthly_date_ucs_04 , monthly_date_ucs_05 )
#




if __name__ == '__main__':
   nb = 1000
   import sys
   if len(sys.argv) > 1: 
      nb = int(sys.argv[1])
   ( nb_ucs_04 , nb_ucs_05 , 
     nb_ucs_04_in_extremity , nb_ucs_05_in_extremity , 
     nb_ucs_04_in_extremity_apical_position, nb_ucs_05_in_extremity_apical_position ,
     terminal_apical_rate_04 , terminal_apical_rate_05 , 
     nb_uc_giving_inflorescence_04 , nb_uc_giving_inflorescence_05 , 
     flowering_rate_04 , flowering_rate_05 , 
     nb_axe_04 , nb_axe_05 , 
     nb_children_per_uc_04 , nb_children_per_uc_05 , 
     nb_descendant_per_axe_04,nb_descendant_per_axe_05,
     monthly_date_ucs_04 , monthly_date_ucs_05 ) = looprun(nb)





def dump_obj(obj,filename, dirname = '.'):
  import cPickle as pickle
  import os
  gfname =os.path.join(dirname,filename)
  pkl_file = open(gfname,'wb')
  pickle.dump(obj,pkl_file)
  pkl_file.close()


dump_obj( (nb_ucs_04 , nb_ucs_05 , 
     nb_ucs_04_in_extremity , nb_ucs_05_in_extremity , 
     nb_ucs_04_in_extremity_apical_position, nb_ucs_05_in_extremity_apical_position ,
     terminal_apical_rate_04 , terminal_apical_rate_05 , 
     nb_uc_giving_inflorescence_04 , nb_uc_giving_inflorescence_05 , 
     flowering_rate_04 , flowering_rate_05 , 
     nb_axe_04 , nb_axe_05 , 
     nb_children_per_uc_04 , nb_children_per_uc_05 , 
     nb_descendant_per_axe_04,nb_descendant_per_axe_05,
     monthly_date_ucs_04 , monthly_date_ucs_05 ), "info_simulate_B12_mtg.pkl", share_dir+"simulations_mangotree_model_pierre/")



