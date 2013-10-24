from openalea.lpy import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")
from vplants.mangosim.tools import *


path_graphic_model = join("simulation_mangotree","simulations_mangotree_graphic_model")
path_glm = join("simulation_mangotree","simulations_mangotree_glm")
path_model_null = join("simulation_mangotree","simulations_mangotree_model_null")

path_choice_file = join("glm_complet","by_tree")
final_path = join( path_graphic_model , path_choice_file)
name_tree = "B12"

def loop_get_from_simulation(nb=1000):
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
   nb_inflo_per_uc_04 = []
   nb_inflo_per_uc_05 = []
   monthly_date_ucs_04 = []
   monthly_date_ucs_05 = []
   for i in xrange(nb): 
      try :
         sim_g = load_obj( "mango_"+ name_tree +"_asynchrony_{0:03d}.bmtg".format(i), join(share_dir , final_path, name_tree) )		 
      except :
         import traceback
         traceback.print_exc()
         pass
      import collections
      import vplants.mangosim.validation_simulations.get_info_simulate_mtg as gism
      #reload(gism)
      (g, g_ucs, g_ucs_in_extremity, g_nb_ucs_in_extremity_apical_position, g_terminal_apical_rate, 
            g_nb_uc_giving_inflorescence, g_flowering_rate, g_nb_inflo_per_uc, g_nb_axe_04, g_nb_axe_05, g_nb_children_per_uc, 
            g_nb_descendant_per_axe, g_monthly_date_ucs ) = gism.extract_info_from_mtg(sim_g)
      nb_ucs_04.append(len( g_ucs[4] ))
      nb_ucs_05.append(len( g_ucs[5] ))
      nb_ucs_04_in_extremity.append(len( g_ucs_in_extremity[4] ))
      nb_ucs_05_in_extremity.append(len( g_ucs_in_extremity[5] ))
      nb_ucs_04_in_extremity_apical_position.append( g_nb_ucs_in_extremity_apical_position[4] )
      nb_ucs_05_in_extremity_apical_position.append( g_nb_ucs_in_extremity_apical_position[5] )
      terminal_apical_rate_04.append( g_terminal_apical_rate[4] )
      terminal_apical_rate_05.append( g_terminal_apical_rate[5] )
      nb_uc_giving_inflorescence_04.append( g_nb_uc_giving_inflorescence[4] )
      nb_uc_giving_inflorescence_05.append( g_nb_uc_giving_inflorescence[5] )
      flowering_rate_04.append( g_flowering_rate[4] )
      flowering_rate_05.append( g_flowering_rate[5] )
      nb_inflo_per_uc_04.append( g_nb_inflo_per_uc[4] )
      nb_inflo_per_uc_05.append( g_nb_inflo_per_uc[5] )
      nb_axe_04.append( g_nb_axe_04 )
      nb_axe_05.append( g_nb_axe_05 ) 
      nb_children_per_uc_04.append( g_nb_children_per_uc[4] )
      nb_children_per_uc_05.append( g_nb_children_per_uc[5] )
      nb_descendant_per_axe_04.append( g_nb_descendant_per_axe[4] )
      nb_descendant_per_axe_05.append( g_nb_descendant_per_axe[5] )
      monthly_date_ucs_04.append( g_monthly_date_ucs[4] )
      monthly_date_ucs_05.append( g_monthly_date_ucs[5] )
   return ( 
      nb_ucs_04 , nb_ucs_05 , 
      nb_ucs_04_in_extremity , nb_ucs_05_in_extremity , 
      nb_ucs_04_in_extremity_apical_position, nb_ucs_05_in_extremity_apical_position ,
      terminal_apical_rate_04 , terminal_apical_rate_05 , 
      nb_uc_giving_inflorescence_04 , nb_uc_giving_inflorescence_05 , 
      flowering_rate_04 , flowering_rate_05 , 
      nb_inflo_per_uc_04 , nb_inflo_per_uc_05,
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
     nb_inflo_per_uc_04 , nb_inflo_per_uc_05,
     nb_axe_04 , nb_axe_05 , 
     nb_children_per_uc_04 , nb_children_per_uc_05 , 
     nb_descendant_per_axe_04,nb_descendant_per_axe_05,
     monthly_date_ucs_04 , monthly_date_ucs_05 ) = loop_get_from_simulation(nb)




dump_obj( (nb_ucs_04 , nb_ucs_05 , 
     nb_ucs_04_in_extremity , nb_ucs_05_in_extremity , 
     nb_ucs_04_in_extremity_apical_position, nb_ucs_05_in_extremity_apical_position ,
     terminal_apical_rate_04 , terminal_apical_rate_05 , 
     nb_uc_giving_inflorescence_04 , nb_uc_giving_inflorescence_05 , 
     flowering_rate_04 , flowering_rate_05 , 
     nb_inflo_per_uc_04 , nb_inflo_per_uc_05,
     nb_axe_04 , nb_axe_05 , 
     nb_children_per_uc_04 , nb_children_per_uc_05 , 
     nb_descendant_per_axe_04,nb_descendant_per_axe_05,
     monthly_date_ucs_04 , monthly_date_ucs_05 ), "info_simulate_" + name_tree + "_mtg.pkl", join(share_dir , final_path) ) 

