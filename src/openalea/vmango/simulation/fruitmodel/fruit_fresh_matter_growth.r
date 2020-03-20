# Mod?le de croissance en Mati?re fraiche, Lechaudel (2007).

growth_FM = function(   date,
                            T_air,
                            GR,
                            RH,
                            dd_cum,
                            T_air_daily,
                            DM_fruit,
                            FM_fruit_ini,
                            W_fleshpeel_ini,
                            DM_fruit_0                                                # Parametre pour calculer la conductivit? hydraulique
                        )

{
#----------------------------- Initialisation des donn?es de sorties et entr?es

  h = 0.002027                                            # Pression Seuil Y ~ H * V_olume Fruit, pour la croissance, Parametre ? Estimer.
  phi_max = 0.414                                           #  Taux d'accroissement cellulaire (MPa / jour, article 2007)
  dd_thresh = 2000                                             # DDJ ? partir duquel extensibilit? cellulaire (Phiini d?croit).
  tau = 0.966                                              # Tau de d?croissance de l'extensibilit? cellulaire.
  aLf = 0.3732

  DM_fleshpeel =    0.4086 * DM_fruit[1]^0.7641 + 0.5219 * DM_fruit[1]^1.0584                               # MS dans la peau et la pulpe.
  W_fleshpeel =   W_fleshpeel_ini                                                                          # Eau dans la peau et la pulpe d?termin?e avec relation allom?trique.
  DM_fleshpeel_growth = 0.4086*0.7641*(DM_fruit[-1])^(0.7641-1) + 0.5219*1.0584*(DM_fruit[-1])^(1.0584-1) * diff(DM_fruit)                                                               # D?rivi?e de la m_ati?re s?che dans la peau et la pulpe.
  DM_fleshpeel_growth[DM_fleshpeel_growth < 0] = 0
  FM_fruit = FM_fruit_ini                                                                             # Masse fraiche du fruit
  FM_stone = FM_fruit - (DM_fleshpeel + W_fleshpeel)                                                        # Masse du noyau calcul?e avec relation empirique.

Results_jour = data.frame( water_potential = NA,
                          turgor_pressure = NA,
                          osmotic_pressure = NA,
                          flux_xyleme = NA,
                          flux_phloeme = NA,
                          transpiration = NA,
                          sucrose = NA,
                          soluble_sugars = NA,
                          organic_acids = NA
                            )

Results_jour_suivant = data.frame( date = NA,
                                   FM_fruit = NA,
                                   MS_Fruit = NA,
                                   W_fleshpeel = NA
                                  )

#-------------------------- Determination de la Pression Osmotique (osmotic_pressure)

  dd_cum =   mean(dd_cum)
	DM_flesh    =   0.8226 * DM_fleshpeel                                                   # Mati?re s?che dans la puple. Relation Allom?trique.
	W_flesh   =   0.8959 * W_fleshpeel

	#---- Calcul des prop_ortion des diff?rents compos?s en fonction des relation allom?triques d?termin?es ? partir de la m_ati?re s?che Pulpe et dd_cum.
  prop_mal = 0.06620651 + (-0.0000538797) * dd_cum + (-0.002464413) * DM_flesh + 2.406565e-006 * DM_flesh * dd_cum
	if (prop_mal < 0) {    	prop_mal = 0.0   	}
	prop_pyr = 0.0006896104 + 1.613387e-006 * dd_cum + 0.00005063595 * DM_flesh + (-6.912509e-008) * DM_flesh * dd_cum
	if (prop_pyr < 0) {    	prop_pyr = 0.0   	}
	prop_oxa = 0.004750718 + (-2.113094e-006) * dd_cum + (-0.00002965687) * DM_flesh + 0.0 * DM_flesh * dd_cum
	if (prop_oxa < 0) {    	prop_oxa = 0.0   	}
	prop_K = 0.01394964 + (-5.234608e-006) * dd_cum + (-0.000288464) * DM_flesh + 2.682089e-007 * DM_flesh * dd_cum
	if (prop_K < 0) {    	prop_K = 0.0   	}
	prop_Mg = 0.00115595 + (-7.937479e-007) * dd_cum + (-0.00002320017) * DM_flesh + (2.344528e-008) * DM_flesh * dd_cum
	if (prop_Mg < 0) {    	prop_Mg = 0.0   	}
	prop_Ca = 0.001588606 + (-6.625787e-007) * dd_cum + (-0.0000228527) * DM_flesh + (1.514343e-008) * DM_flesh * dd_cum
	if (prop_Ca < 0) {    	prop_Ca = 0.0   	}
	prop_NH4 = 0.000246011 + 3.741743e-007 * dd_cum + 0.00002495255 * DM_flesh + (-3.010081e-008) * DM_flesh * dd_cum
	if (prop_NH4 < 0) {    	prop_NH4 = 0.0   	}
	prop_Na = 0.0001279568 + 8.15203e-008 * dd_cum + (-1.468235e-006) * DM_flesh + 0.0 * DM_flesh * dd_cum
	if (prop_Na < 0) {    	prop_Na = 0.0   	}
	prop_glc = 0.08074145 + (-0.00006325543) * dd_cum + (-0.001161846) * DM_flesh + 1.161344e-006 * DM_flesh * dd_cum
	if (prop_glc < 0) {    	prop_glc = 0.0   	}
	prop_frc = 0.04972199 + 0.0000966001 * dd_cum + (-0.001078579) * DM_flesh + 0.0 * DM_flesh * dd_cum
	if (prop_frc < 0) {    	prop_frc = 0.0   	}
	prop_ami = -0.1708815 + 0.0004380411 * dd_cum + 0.01923022 * DM_flesh + (-0.00002059459) * DM_flesh * dd_cum
	if (prop_ami < 0) {    	prop_ami = 0.0   	}
  prop_cit = 0.1625024 + (-0.0000640754) * dd_cum + 0.003906348 * DM_flesh + (-4.784292e-006) * DM_flesh * dd_cum
  if (prop_cit < 0) {    	prop_cit = 0.0   	}
  prop_sac = 0.0 + (0.00017695) * dd_cum + (-0.007249) * DM_flesh + 9.03e-006 * DM_flesh * dd_cum
  if (prop_sac < 0) {    	prop_sac = 0.0   	}

  #---- Calcul de la m_asse et du nombre de m_ol des diff?rents compos?s
  m_mal = prop_mal * DM_flesh;      n_mal = m_mal / 134
  m_cit = prop_cit * DM_flesh;      n_cit = m_cit / 192
  m_pyr = prop_pyr * DM_flesh;      n_pyr = m_pyr / 88
  m_oxa = prop_oxa * DM_flesh;      n_oxa = m_oxa / 90
  m_K = prop_K * DM_flesh;          n_K = m_K / 39
  m_Mg = prop_Mg * DM_flesh;        n_Mg = m_Mg / 24
  m_Ca = prop_Ca * DM_flesh;        n_Ca = m_Ca / 40
  m_NH4 = prop_NH4 * DM_flesh;      n_NH4 = m_NH4 / 18
  m_Na = prop_Na * DM_flesh;        n_Na = m_Na / 23
  m_g = prop_glc * DM_flesh;        n_glc = m_g / 180
  m_f = prop_frc * DM_flesh;        n_frc = m_f / 180
  m_sa = prop_sac * DM_flesh;       n_sac = m_sa / 342
  m_am = prop_ami * DM_flesh

  n_solutes =     n_mal + n_cit + n_pyr + n_oxa + n_K + n_Mg + n_Ca + n_NH4 + n_Na + n_glc + n_frc + n_sac
	c_solutes     =     n_solutes / W_flesh
	R = 8.3                                                                        # R gaz constant (cm3 bar m_ol-1 ?K-1
	osmotic_pressure = R * (T_air_daily + 273.15) * c_solutes  + 0.2                # Calcul de la pression osmotique (MPa)
                                                                                #0.2 ?tant la pression osmotique due aux acides amin?s, constant


###----- CALCUL DE LA TRANSPIRATION DU FRUIT -----------------

ro = 5544                                                                       # perm?abilit? cuticulaire en cm/jour (231 cm/h dans papier 2007)
A_fruit = 3.65 * (FM_fruit)^0.73                                            # calcul de la surface du fruit exp?rimental (en cm?)
P_sat = 0.008048 * exp(0.0547 * T_air_daily)                                 # pression de vapeur saturante (Nobel, 1974)
alpha = 18 * P_sat / (83 * (T_air_daily + 273.15))                  # concentration de vapeur d'eau ? saturation

# calcul de la transpiration du fruit (g/j)
transpiration = A_fruit * alpha * ro * (0.996 - (mean(RH) / 100))       # 0.996 = HR dans les espaces remplis d'air ? l'int?rieur du fruit

#----------------------------- Determination de la pression de turgescence (turgor_pressure)-----------------------------------------------------------------------------------------------------

  water_potential_stem = 1.0 * mean(-0.6617105 + (-0.006940179 * T_air) + (0.007888208 * RH) + (0.0000198265 * GR)) # Calcul du potentiel hydrique arbre

  dd_thresh = 20.769 * DM_fruit_0 + 518.87                                    # variante possible, dd_thresh fonction P0.fruit
  #### MODIF MAY17


	if (dd_cum > dd_thresh) {Phi = phi_max * (tau ^(dd_cum-dd_thresh))}else{ Phi = phi_max}               #- Variation de Phi (accroissement volume en fonction de Taux ? partir seuil dd_cum).

#calcul de la conductivit? globale
  ALf = A_fruit * aLf                                                         # produit de la surface et du ratio (membrane composite/surface du fruit du fruit = a) et de la conductivit? hydraulique entre la tige et le fruit (Lf)

# calcul du seuil de pression Y
  Y_o = 0 ; V_o = 0                                                               # param?tres papier 2007
  Y = Y_o + h * (W_fleshpeel - V_o)

#  Pturgescence ? partir equation 13 sans l'?lasticit?
  numerator = Phi * W_fleshpeel * Y + ALf * (water_potential_stem + osmotic_pressure) - transpiration + DM_fleshpeel_growth / 1.60
  denominator = Phi * W_fleshpeel + ALf
  turgor_pressure = numerator / denominator

	if (turgor_pressure < Y) turgor_pressure = water_potential_stem + osmotic_pressure - (transpiration - (DM_fleshpeel_growth / 1.60)) / ALf

	if (turgor_pressure < Y_o) turgor_pressure = 0

	water_potential = turgor_pressure - osmotic_pressure                                   #- Calcul du potentiel hydrique du fruit.

	flux_xyleme  = ALf * (water_potential_stem - water_potential)                           #- Entr?e d'eau par le xyleme
	flux_phloeme = DM_fleshpeel_growth / 1.60                                                                        #- Entr?e d'eau par le phloeme

#---------------------------------- Bilan Hydrique et carbon? ------------------

  DM_fleshpeel       = DM_fleshpeel + DM_fleshpeel_growth                                           # Bilan carbon?
  W_fleshpeel      = W_fleshpeel + flux_xyleme + flux_phloeme - transpiration  # Bilan hydrique
  FM_stone  = 0.1167 * (DM_fleshpeel + W_fleshpeel)
  FM_fruit  = DM_fleshpeel + W_fleshpeel + FM_stone
  #---------------------------------- Sorties qualit? ------------------
  Results_jour$soluble_sugars =                     (m_sa+m_g+m_f)/ W_flesh  # somme glucose, sacchrose, fructose
  Results_jour$organic_acids =                     (m_mal + m_cit)/W_flesh # somme acide citrique et m_alique

#----------------------------------- Resultats ---------------------------------

Results_jour$water_potential=              water_potential
Results_jour$turgor_pressure=                   turgor_pressure
Results_jour$osmotic_pressure =                    osmotic_pressure
Results_jour$flux_xyleme =                         flux_xyleme
Results_jour$flux_phloeme =                        flux_phloeme
Results_jour$transpiration =                  transpiration
Results_jour$sucrose =                     m_sa / W_flesh

Results_jour_suivant$date =                 date + 1
Results_jour_suivant$FM_fruit =          FM_fruit
Results_jour_suivant$MS_Fruit =             DM_fruit[2]
Results_jour_suivant$W_fleshpeel =              W_fleshpeel

Results = list( Results_jour = Results_jour,
                  Results_jour_suivant = Results_jour_suivant)

return(Results)

}
