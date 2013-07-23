from vplants.mangosim.validation_simulations.get_distributions_simulations import get_limit_IC , get_distribution
import matplotlib.pyplot as plt

def load_obj(filename, dirname = '.'):
  import cPickle as pickle
  import os
  gfname =os.path.join(dirname,filename)
  if os.path.exists(gfname ):
    pkl_file = open(gfname,'rb')
    obj = pickle.load(pkl_file)
    pkl_file.close()
    return obj
  else:
    raise ValueError(gfname)

share_dir = "../../../share/"

######## Import result of simulation

( nb_ucs_04 , nb_ucs_05 , nb_ucs_04_in_extremity , nb_ucs_05_in_extremity , 
     nb_ucs_04_in_extremity_apical_position, nb_ucs_05_in_extremity_apical_position , terminal_apical_rate_04 , terminal_apical_rate_05 , 
     nb_uc_giving_inflorescence_04 , nb_uc_giving_inflorescence_05 , flowering_rate_04 , flowering_rate_05 , 
     nb_axe_04 , nb_axe_05 , nb_children_per_uc_04 , nb_children_per_uc_05 ,nb_descendant_per_axe_04,nb_descendant_per_axe_05,
     monthly_date_ucs_04 , monthly_date_ucs_05 ) = load_obj("info_simulate_B12_mtg.pkl",share_dir+"simulations_mangotree_model_pierre")
#

######## Import result of the mtg
(nb_ucs_veget_B12_04,nb_ucs_veget_B12_05,nb_ucs_veget_B10_04,nb_ucs_veget_B10_05,
  nb_ucs_veget_F2_04,nb_ucs_veget_F2_05,nb_ucs_veget_B14_04,nb_ucs_veget_B14_05,
  nb_ucs_veget_F6_04,nb_ucs_veget_F6_05) = load_obj("total_number_ucs_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(nb_ucs_B12_04_in_extremity,nb_ucs_B12_05_in_extremity,nb_ucs_B10_04_in_extremity,
  nb_ucs_B10_05_in_extremity,nb_ucs_F2_04_in_extremity,nb_ucs_F2_05_in_extremity,
  nb_ucs_B14_04_in_extremity,nb_ucs_B14_05_in_extremity,nb_ucs_F6_04_in_extremity,
  nb_ucs_F6_05_in_extremity) = load_obj("nb_ucs_tree_cycle_in_extremity.pkl",share_dir+"info_mtg_doralice/")

(nb_ucs_B12_04_in_extremity_apical_position,nb_ucs_B12_05_in_extremity_apical_position,nb_ucs_B10_04_in_extremity_apical_position,
  nb_ucs_B10_05_in_extremity_apical_position,nb_ucs_F2_04_in_extremity_apical_position,nb_ucs_F2_05_in_extremity_apical_position,
  nb_ucs_B14_04_in_extremity_apical_position,nb_ucs_B14_05_in_extremity_apical_position,nb_ucs_F6_04_in_extremity_apical_position,
  nb_ucs_F6_05_in_extremity_apical_position) = load_obj("nb_ucs_tree_cycle_in_extremity_apical_position.pkl",share_dir+"info_mtg_doralice/")

(nb_axe_B12_04,nb_axe_B12_05,nb_axe_B10_04,nb_axe_B10_05,nb_axe_F2_04,nb_axe_F2_05,nb_axe_B14_04,
  nb_axe_B14_05,nb_axe_F6_04,nb_axe_F6_05) = load_obj("nb_axe_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(nb_children_per_uc_B12_04,nb_children_per_uc_B12_05,nb_children_per_uc_B10_04,nb_children_per_uc_B10_05,
  nb_children_per_uc_F2_04,nb_children_per_uc_F2_05,nb_children_per_uc_B14_04,nb_children_per_uc_B14_05,
  nb_children_per_uc_F6_04,nb_children_per_uc_F6_05) = load_obj("nb_children_per_uc_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(nb_ucs_per_axe_B12_04,nb_ucs_per_axe_B12_05,nb_ucs_per_axe_B10_04,nb_ucs_per_axe_B10_05,
  nb_ucs_per_axe_F2_04,nb_ucs_per_axe_F2_05,nb_ucs_per_axe_B14_04,nb_ucs_per_axe_B14_05,
  nb_ucs_per_axe_F6_04,nb_ucs_per_axe_F6_05 )= load_obj("nb_ucs_per_axe_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(nb_uc_giving_inflorescence_B12_04,nb_uc_giving_inflorescence_B12_05,nb_uc_giving_inflorescence_B10_04,
  nb_uc_giving_inflorescence_B10_05,nb_uc_giving_inflorescence_F2_04,nb_uc_giving_inflorescence_F2_05,
  nb_uc_giving_inflorescence_B14_04,nb_uc_giving_inflorescence_B14_05,nb_uc_giving_inflorescence_F6_04,
  nb_uc_giving_inflorescence_F6_05) = load_obj("nb_uc_giving_inflorescence_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(flowering_rate_B12_04,flowering_rate_B12_05,flowering_rate_B10_04,flowering_rate_B10_05,
  flowering_rate_F2_04,flowering_rate_F2_05,flowering_rate_B14_04,flowering_rate_B14_05,
  flowering_rate_F6_04,flowering_rate_F6_05) = load_obj("flowering_rate_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(terminal_apical_rate_B12_04,terminal_apical_rate_B12_05,terminal_apical_rate_B10_04,
  terminal_apical_rate_B10_05,terminal_apical_rate_F2_04,terminal_apical_rate_F2_05,
  terminal_apical_rate_B14_04,terminal_apical_rate_B14_05,terminal_apical_rate_F6_04,
  terminal_apical_rate_F6_05) = load_obj("terminal_apical_rate_tree_cycle.pkl",share_dir+"info_mtg_doralice/")

(freq_monthly_date_ucs_B12_04,freq_monthly_date_ucs_B12_05,freq_monthly_date_ucs_B10_04,
  freq_monthly_date_ucs_B10_05,freq_monthly_date_ucs_F2_04,freq_monthly_date_ucs_F2_05,
  freq_monthly_date_ucs_B14_04,freq_monthly_date_ucs_B14_05,freq_monthly_date_ucs_F6_04,
  freq_monthly_date_ucs_F6_05) = load_obj("monthly_date_ucs_tree_cycle.pkl",share_dir+"info_mtg_doralice/")




#################################### Tree B12 ####################################

#### Number of unit growth for tree B12
limit_IC_nb_ucs_04 = get_limit_IC(nb_ucs_04)
get_distribution(nb_ucs_04,nb_ucs_veget_B12_04, limit_IC_nb_ucs_04,xlab = "Number of unit growth in Cycle 4 for tree B12",subtitle = "",
  title="Distribution of number of unit growth in Cycle 4 for tree B12", 
  ylim = [0,0.014])

limit_IC_nb_ucs_05 = get_limit_IC(nb_ucs_05)
get_distribution(nb_ucs_05,nb_ucs_veget_B12_05, limit_IC_nb_ucs_05,500,xlab = "Number of unit growth in Cycle 5 for tree B12",subtitle = "",
  title="Distribution of number of unit growth in Cycle 5 for tree B12",
  xlim=[700,3000], ylim = [0,0.0035])

#### Number of unit growth for tree B12 at the ends of the canopy
limit_IC_nb_ucs_04_in_extremity = get_limit_IC(nb_ucs_04_in_extremity)
get_distribution(nb_ucs_04_in_extremity,nb_ucs_B12_04_in_extremity, limit_IC_nb_ucs_04_in_extremity,
  xlab = "Number of unit growth in Cycle 4 for tree B12 at the ends of the canopy",subtitle = "",
  title="Distribution of number of unit growth at the ends of the canopy \n in Cycle 4 for tree B12",
  xlim=[250,400],ylim = [0,0.02])

limit_IC_nb_ucs_05_in_extremity = get_limit_IC(nb_ucs_05_in_extremity)
get_distribution(nb_ucs_05_in_extremity,nb_ucs_B12_05_in_extremity, limit_IC_nb_ucs_05_in_extremity,500,
  xlab = "Number of unit growth in Cycle 5 for tree B12 at the ends of the canopy", subtitle = "",ylab="Value = 50*Probability",
  title="Distribution of number of unit growth at the ends of the canopy \n in Cycle 5 for tree B12",
  xlim = [700,3000],ylim = [0,0.0040])

#### Number of unit growth for tree B12 at the ends of the canopy in apical position 
limit_IC_nb_ucs_04_in_extremity_apical_position = get_limit_IC(nb_ucs_04_in_extremity_apical_position)
get_distribution(nb_ucs_04_in_extremity_apical_position,nb_ucs_B12_04_in_extremity_apical_position, limit_IC_nb_ucs_04_in_extremity_apical_position,
  xlab = "Number of terminal unit growth in Cycle 4 for tree B12 in apical position",subtitle = "",
  title="Distribution of number of unit growth at the ends of the canopy \n in apical position in Cycle 4 for tree B12",
  xlim=[130,190],ylim = [0,0.05])

limit_IC_nb_ucs_05_in_extremity_apical_position = get_limit_IC(nb_ucs_05_in_extremity_apical_position)
get_distribution(nb_ucs_05_in_extremity_apical_position,nb_ucs_B12_05_in_extremity_apical_position, limit_IC_nb_ucs_05_in_extremity_apical_position,
  xlab = "Number of terminal unit growth in Cycle 5 for tree B12 in apical position",subtitle = "",
  title="Distribution of number of unit growth at the ends of the canopy \n in apical position in Cycle 5 for tree B12",
  xlim=[150,400],ylim = [0,0.014])

#### Rate of terminal apical position for tree B12
limit_IC_terminal_apical_rate_04 = get_limit_IC(terminal_apical_rate_04)
get_distribution(terminal_apical_rate_04, terminal_apical_rate_B12_04, limit_IC_terminal_apical_rate_04,
  xlab = "Rate of terminal apical position in cycle 4 for tree B12",subtitle = "",
  title="Distribution of rate of terminal apical position \n in cycle 4 for tree B12",
  ylim = [0,18])

limit_IC_terminal_apical_rate_05 = get_limit_IC(terminal_apical_rate_05)
get_distribution(terminal_apical_rate_05,terminal_apical_rate_B12_05, limit_IC_terminal_apical_rate_05,
  xlab = "Rate of terminal apical position in cycle 5 for tree B12",subtitle = "",
  title="Distribution of rate of terminal apical position \n in cycle 5 for tree B12",
  ylim = [0,14])
plt.legend(loc="upper left")

#### Number of unit growth for tree B12 giving inflorescences
limit_IC_nb_uc_giving_inflorescence_04 = get_limit_IC(nb_uc_giving_inflorescence_04)
get_distribution(nb_uc_giving_inflorescence_04,nb_uc_giving_inflorescence_B12_04, limit_IC_nb_uc_giving_inflorescence_04,
  xlab = "Number of unit growth in Cycle 4 for tree B12 giving inflorescences",subtitle = "",
  title="Distribution of number of terminal unit growth giving inflorescences \n in Cycle 4 for tree B12",
  ylim = [0,0.03])

limit_IC_nb_uc_giving_inflorescence_05 = get_limit_IC(nb_uc_giving_inflorescence_05)
get_distribution(nb_uc_giving_inflorescence_05,nb_uc_giving_inflorescence_B12_05, limit_IC_nb_uc_giving_inflorescence_05,500,
  xlab = "Number of unit growth in Cycle 5 for tree B12 giving inflorescences",subtitle = "", ylab= "Value = 50*Probability",
  title="Distribution of number of terminal unit growth giving inflorescences \n in Cycle 5 for tree B12",
  xlim=[400,1400],ylim = [0,0.0045])
plt.legend(loc="upper center")

#### Rate of inflorescences for tree B12
limit_IC_flowering_rate_04 = get_limit_IC(flowering_rate_04)
get_distribution(flowering_rate_04, flowering_rate_B12_04, limit_IC_flowering_rate_04,
  xlab = "Flowering rate in cycle 4 for tree B12",subtitle = "",
  title="Distribution of flowering rate in cycle 4 for tree B12",
  xlim = [0.45,0.72], ylim = [0,11])
plt.legend(loc="upper left")

limit_IC_flowering_rate_05 = get_limit_IC(flowering_rate_05)
get_distribution(flowering_rate_05,flowering_rate_B12_05, limit_IC_flowering_rate_05,
  xlab = "Flowering rate in cycle 5 for tree B12",subtitle = "",
  title="Distribution of flowering rate in cycle 5 for tree B12",
  ylim = [0,10])
plt.legend(loc="upper left")

#### Number of axes for tree B12 
limit_IC_nb_axe_04 = get_limit_IC(nb_axe_04)
get_distribution(nb_axe_04,nb_axe_B12_04, limit_IC_nb_axe_04,
  xlab = "Number of axes in Cycle 4 for tree B12",subtitle = "",
  title="Distribution of number of axes \n in Cycle 4 for tree B12" ,
  ylim = [0,10])
plt.legend(loc="upper center")

limit_IC_nb_axe_05 = get_limit_IC(nb_axe_05)
get_distribution(nb_axe_05,nb_axe_B12_05, limit_IC_nb_axe_05,500,
  xlab = "Number of axes in Cycle 5 for tree B12",subtitle = "",ylab="Value = 50* Probability",
  title="Distribution of number of axes \n in Cycle 5 for tree B12" ,
  xlim = [400,2000],ylim = [0,0.005])

  
  
  
  
  
  
  
  
  
  
  
  
  
#################################### Tree B10 ####################################


















