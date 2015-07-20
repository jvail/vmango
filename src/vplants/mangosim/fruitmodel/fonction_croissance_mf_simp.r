
# Modèle de croissance en Matière fraiche, Lechaudel (2007).

CROISSANCE_MF = function(   Date,
                            Temperature_Air,
                            rayo,
                            humirela,                    
                            ddj,
                            Temp_air_moy,
                            MSfruit,
                            MF_Init,
                            Eaupepu_Init,
                            Poids_Fruit_Init,
                            H,                                                  # Pression Seuil Y ~ H * Volume Fruit, pour la croissance, Parametre à Estimer.
                            Phiini,                                             #  Taux d'accroissement cellulaire
                            DDini,                                              # DDJ à partir duquel extensibilité cellulaire (Phiini décroit).
                            Tau,                                                # Tau de décroissance de l'extensibilité cellulaire.
                            aLf                                                 # Parametre pour calculer la conductivité hydraulique
                        )

{                                 
#----------------------------- Initialisation des données de sorties et entrées
  
  MSpepu =    0.4086 * MSfruit[1]^0.7641 + 0.5219 * MSfruit[1]^1.0584                               # MS dans la peau et la pulpe.
  Eaupepu =   Eaupepu_Init                                                                          # Eau dans la peau et la pulpe déterminée avec relation allométrique.
  partpepu =  0.4086*0.7641*(MSfruit[-1])^(0.7641-1) + 0.5219*1.0584*(MSfruit[-1])^(1.0584-1)
  croissMS = partpepu * diff(MSfruit)                                                               # Dériviée de la matière sèche dans la peau et la pulpe.
  croissMS[croissMS < 0] = 0
  Masse_Fruit = MF_Init                                                                             # Masse fraiche du fruit
  Masse_Noyau = MF_Init - (MSpepu + Eaupepu)                                                        # Masse du noyau calculée avec relation empirique.

Results_jour = data.frame( Potentiel_Hydrique = NA,
                           P_Turgescence = NA,
                           P_Osmotique = NA,
                           Xyleme = NA,
                           Phloeme = NA,
                           Transpiration = NA,
                           saccharose = NA
                         )

Results_jour_suivant = data.frame( Date = NA,
                                   Masse_Fruit = NA,
                                   MS_Fruit = NA,
                                   Eaupepu = NA
                                  )
                                                                                                                                                                                              
#-------------------------- Determination de la Pression Osmotique (P_Osmotique)
	
  degjour =   mean(ddj)
	MSpu    =   0.8226 * MSpepu                                                   # Matière sèche dans la puple. Relation Allométrique.
	Eaupu   =   0.8958 * Eaupepu
	
	#---- Calcul des proportion des différents composés en fonction des relation allométriques déterminées à partir de la matière sèche Pulpe et DDJ.
  propmal = 0.06620651 + (-0.0000538797) * degjour + (-0.002464413) * MSpu + 2.406565e-006 * MSpu * degjour
	if (propmal < 0) {    	propmal = 0.0   	}
	proppyr = 0.0006896104 + 1.613387e-006 * degjour + 0.00005063595 * MSpu + (-6.912509e-008) * MSpu * degjour
	if (proppyr < 0) {    	proppyr = 0.0   	}
	propoxa = 0.004750718 + (-2.113094e-006) * degjour + (-0.00002965687) * MSpu + 0.0 * MSpu * degjour
	if (propoxa < 0) {    	propoxa = 0.0   	}
	propK = 0.01394964 + (-5.234608e-006) * degjour + (-0.000288464) * MSpu + 2.682089e-007 * MSpu * degjour
	if (propK < 0) {    	propK = 0.0   	}
	propMg = 0.00115595 + (-7.937479e-007) * degjour + (-0.00002320017) * MSpu + (2.344528e-008) * MSpu * degjour
	if (propMg < 0) {    	propMg = 0.0   	}
	propCa = 0.001588606 + (-6.625787e-007) * degjour + (-0.0000228527) * MSpu + (1.514343e-008) * MSpu * degjour
	if (propCa < 0) {    	propCa = 0.0   	}
	propNH4 = 0.000246011 + 3.741743e-007 * degjour + 0.00002495255 * MSpu + (-3.010081e-008) * MSpu * degjour
	if (propNH4 < 0) {    	propNH4 = 0.0   	}
	propNa = 0.0001279568 + 8.15203e-008 * degjour + (-1.468235e-006) * MSpu + 0.0 * MSpu * degjour
	if (propNa < 0) {    	propNa = 0.0   	}
	propglc = 0.08074145 + (-0.00006325543) * degjour + (-0.001161846) * MSpu + 1.161344e-006 * MSpu * degjour
	if (propglc < 0) {    	propglc = 0.0   	}
	propfrc = 0.04972199 + 0.0000966001 * degjour + (-0.001078579) * MSpu + 0.0 * MSpu * degjour
	if (propfrc < 0) {    	propfrc = 0.0   	}
	propami = -0.1708815 + 0.0004380411 * degjour + 0.01923022 * MSpu + (-0.00002059459) * MSpu * degjour
	if (propami < 0) {    	propami = 0.0   	}
  propcit = 0.1625024 + (-0.0000640754) * degjour + 0.003906348 * MSpu + (-4.784292e-006) * MSpu * degjour
  if (propcit < 0) {    	propcit = 0.0   	} 
  propsac = 0.0 + (0.00017695) * degjour + (-0.007249) * MSpu + 9.03e-006 * MSpu * degjour
  if (propsac < 0) {    	propsac = 0.0   	} 

  #---- Calcul de la masse et du nombre de mol des différents composés
	mmal = propmal * MSpu;      nmal = mmal / 134 
	mcit = propcit * MSpu;      ncit = mcit / 192
	mpyr = proppyr * MSpu;      npyr = mpyr / 88
	moxa = propoxa * MSpu;      noxa = moxa / 90
	mK = propK * MSpu;          nK = mK / 39  
	mMg = propMg * MSpu;        nMg = mMg / 24
	mCa = propCa * MSpu;        nCa = mCa / 40  
	mNH4 = propNH4 * MSpu;      nNH4 = mNH4 / 18
	mNa = propNa * MSpu;        nNa = mNa / 23
	mg = propglc * MSpu;        nglc = mg / 180
	mf = propfrc * MSpu;        nfrc = mf / 180
	msa = propsac * MSpu;       nsac = msa / 342
	mam = propami * MSpu       

  ncompsol =     nmal + ncit + npyr + noxa + nK + nMg + nCa + nNH4 + nNa + nglc + nfrc + nsac
	Cssm     =     ncompsol / Eaupu
	R = 83                                                                        # R gaz constant (cm3 bar mol-1 °K-1
	P_Osmotique = (R / 10) * (Temp_air_moy + 273.15) * Cssm  + 0.2                # Calcul de la pression osmotique (MPa)
                                                                                #0.2 étant la pression osmotique due aux acides aminés, constant

	
###----- CALCUL DE LA TRANSPIRATION DU FRUIT -----------------        

ro = 5544                                                                       # perméabilité cuticulaire en cm/jour (231 cm/h dans papier 2007)
surfruit = 3.65 * (Masse_Fruit)^0.73                                            # calcul de la surface du fruit expérimental (en cm²)
Petoile = 0.008048 * exp(0.0547 * Temp_air_moy)                                 # pression de vapeur saturante (Nobel, 1974)
transpi.alpha = 18 * Petoile / (83 * (Temp_air_moy + 273.15))                  # concentration de vapeur d'eau à saturation

# calcul de la transpiration du fruit (g/j)
Transpiration_Fruit = surfruit * transpi.alpha * ro * (0.996 - (mean(humirela) / 100))       # 0.996 = HR dans les espaces remplis d'air à l'intérieur du fruit
        
#----------------------------- Determination de la pression de turgescence (P_Turgescence)-----------------------------------------------------------------------------------------------------	
	
  Potentiel_Hydrique_Arbre = 1.0 * mean(-0.6617105 + (-0.006940179 * Temperature_Air) + (0.007888208 * humirela) + (0.0000198265 * rayo)) # Calcul du potentiel hydrique arbre

#  DDini = 20.769 * Poids_Fruit_Init + 518.87                                    # variante possible, DDini fonction P0.fruit
                                                                                 
	if (degjour > DDini) {Phi = Phiini * (Tau ^(degjour-DDini))}else{ Phi = Phiini}               #- Variation de Phi (accroissement volume en fonction de Taux à partir seuil DDJ).

#calcul de la conductivité globale
  A_Lf = surfruit * aLf                                                         # produit de la surface et du ratio (membrane composite/surface du fruit du fruit = a) et de la conductivité hydraulique entre la tige et le fruit (Lf)
  
# calcul du seuil de pression Y                                                 
  Yo = 0 ; Vo = 0                                                               # paramètres papier 2007
  Y = Yo + H * (Eaupepu - Vo)

#  Pturgescence à partir equation 13 sans l'élasticité
  numerateur = Phi * Eaupepu * Y + A_Lf * (Potentiel_Hydrique_Arbre + P_Osmotique) - Transpiration_Fruit + croissMS / 1.60
  denominateur = Phi * Eaupepu + A_Lf
  P_Turgescence = numerateur / denominateur 

	if (P_Turgescence < Y) P_Turgescence = Potentiel_Hydrique_Arbre + P_Osmotique - (Transpiration_Fruit - (croissMS / 1.60)) / A_Lf

	if (P_Turgescence < Yo) P_Turgescence = 0
	  
	Potentiel_Hydrique_Fruit = P_Turgescence - P_Osmotique                                   #- Calcul du potentiel hydrique du fruit.

	Flux_Xyleme  = A_Lf * (Potentiel_Hydrique_Arbre - Potentiel_Hydrique_Fruit)                           #- Entrée d'eau par le xyleme
	Flux_Phloeme = croissMS / 1.60                                                                        #- Entrée d'eau par le phloeme

#---------------------------------- Bilan Hydrique et carboné ------------------

	MSpepu_new       = MSpepu + croissMS                                           # Bilan carboné
  Eaupepu_new      = Eaupepu + Flux_Xyleme + Flux_Phloeme - Transpiration_Fruit  # Bilan hydrique
  Masse_Noyau_new  = 0.1167 * (MSpepu_new + Eaupepu_new)
  Masse_Fruit_new  = MSpepu_new + Eaupepu_new + Masse_Noyau_new  

#----------------------------------- Resultats ---------------------------------

Results_jour$Potentiel_Hydrique=              Potentiel_Hydrique_Fruit
Results_jour$P_Turgescence=                   P_Turgescence
Results_jour$P_Osmotique =                    P_Osmotique
Results_jour$Xyleme =                         Flux_Xyleme
Results_jour$Phloeme =                        Flux_Phloeme
Results_jour$Transpiration =                  Transpiration_Fruit
Results_jour$saccharose =                     msa / Eaupu

Results_jour_suivant$Date =                 Date + 1
Results_jour_suivant$Masse_Fruit =          Masse_Fruit_new
Results_jour_suivant$MS_Fruit =             MSfruit[2]
Results_jour_suivant$Eaupepu =              Eaupepu_new

Results = list( Results_jour = Results_jour,
                  Results_jour_suivant = Results_jour_suivant)

return(Results)

}