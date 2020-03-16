

#localdir = getSrcDirectory(function(x) {x})


### Fonction MS ? partir Model MS Lechaudel, 2005

growth_DM = function( GR,                                          # En watts.m-2
                          T_air,                                      # Temp?rature horaire de l'air
                          T_fruit,                                    # Temp?rature horaire du fruit.
                          sunlit_bs,                                             # Evolution de l'environnement lumineux dans la journ?e
                          DM_fruit_0,                                     # Poids du fruit ? la fin de la division cellulaire en gramme de MS
                          DM_fruit_previous,                                   # en gramme de MS
                          reserve_stem,                                       # en gramme de carbone
                          reserve_leaf,                                      # en gramme de carbone
                          LF                                                    # Rapport feuille / fruit [10, 150]
                        )

    {

dd_delta = sum((T_fruit - 16)/24)                            # Accumulation de DDJ dans la journ?e
GR = GR / 3600 * 10000                                                 # transformation du RG en J/cm2/h en W/m2
PAR = GR * 0.5 * 4.6                                                            # Transformation du RG en watts/m2 en Rayonnement Photosynth?tiquement Actif (papier Varlet-Grancher et al. 1989 dans Agronomie, vol 9:419-139)

#--------------------------------------------------------- Param?tres du Mod?le ----------------------------------------------------------------------------------------------------------------

# PARAMETRES POUR ASSIMILATION
# k1 et k2 : coefficients de pond?ration du rayonnement inter et intra-rameau
    sunlit_ws <- rep(0.88,24)

# PARAMETRES PHOTOSYNTHESE fixe
    p1 <- 3.85                                                                  # coefficients relation Anet = f(PPF)
    p2 <- 33.23
    p3 <- 0.483
    p4 <- 0.034
    k3 <- 0.0529                                                                # coefficient relation PPFshaded = f(PPFsunlit)

# PARAMETRES POUR RESPIRATIONS D'ENTRETIEN
    MRR_stem <- 0.000858                                                      # Respiration de maintenance rameau
    MRR_leaf <- 0.000156                                                    # Respiration de maintenance feuilles
    MRR_fruit <- 0.00115                                                       # Respiration de maintenance fruit
    Q10_stem <- 1.96                                                          # Q10 rameau
    Q10_leaf <- 2.11                                                        # Q10 feuilles
    Q10_fruit <- 1.9                                                           # Q10 fruits

# PARAMETRES POUR DEMANDE DU FRUIT
    r_mobile_leaf <- 0.0162                                                    # remobilisation des r?serves, papier 2005
    r_mobile_stem <- 0.0164                                                      # remobilisation des r?serves, papier 2005
    cc_stem <- 0.4387                                                              # concentration carbone rameau
    cc_leaf <- 0.4051                                                            # concentration carbone feuille
    cc_fruit <- 0.4239                                                            # concentration carbone fruit
    GRC_fruit <- 0.04                                                            # coefficient de respiration de croissance (gCO2.gMS)

# PARAMETRE LOGISTIQUE FONCTION DES DDJ
    RGR_fruit_ini =  0.0105                                                      #  param papier 2005
    a1 =       16.736                                                      #  param papier 2005
    a2 =       0.624                                                       #  param papier 2005
    r_storage =           0.3                                                         # ?
    DM_fruit_max =  a1 * (DM_fruit_0 ^ a2)                            # poids maximum du fruit.

# PARAMETRES DES STRUCTURES
    DM_stem <- (41.83 + 77.41) / 2                                         # moy exp?, fixe
    DM_leaf_unit <- 0.8                                                       # poids sec feuille, fixe
    r_DM_stem_ini <- 0.1                                              # moy exp?
    r_DM_leaf_ini <- 0.074                                          # moy exp?

# PARAMETRES DE STRUCTURES
  DM_structure_stem = DM_stem * (1 - r_DM_stem_ini)               # partie structure du remeau en gC.
  DM_structure_leaf = DM_leaf_unit * (1 - r_DM_leaf_ini) * LF     # partie structure des feuilles en gC.

    LA <- 0.0051 * LF^0.937                                               # Estimation de la surface folliaire calcul surface foliaire (m2)

# ==============================================================================
# ASSIMILATION DE CARBONE
# ==============================================================================

#----- demande de croissance du fruit (gC/j) , ce que veux le fruit en fonction de la croissance potentielle.
  D_fruit =  DM_fruit_previous * RGR_fruit_ini * dd_delta * (1-( DM_fruit_previous / DM_fruit_max)) * (cc_fruit + GRC_fruit)

#----- calcul de la photosynth?se maximale, est fonction de la demande du fruit, surface folaire (la demande tient compte des co?ts (en C) de construction )
  Pmax <- (p1 * (D_fruit / LA) * p2) / (p1 * (D_fruit / LA) + p2)
  if (Pmax >=15) { Pmax = 15 }
  # if (Pmax < 5)  { Pmax = 5  }   # Si pas de fruit, on limite le Pmax à 5.
  #### MODIF MAY17

###----- CALCUL DES ASSIMILATS SUR LA JOURNEE OFFRE DE LA JOURNEE, production et mobilisation des r?serves.

  LA_sunlit = sunlit_bs[PAR>0] * sunlit_ws[PAR>0] * LA                       # on calcul la surface folaire en plein soleil
  LA_shaded = LA - LA_sunlit                                        # surface folaire ? l'ombre

#calcul du rayonnement a l'ombre a l'aide d'une fonction de ponderation du PPFD
  PAR_shaded = k3 * PAR
  P_shaded = ((Pmax + p3)* (1 - exp(- p4 * PAR_shaded[PAR_shaded>0] / (Pmax + p3))))- p3
  photo_shaded = 3600 * sum (P_shaded[P_shaded>0] *  LA_shaded[P_shaded>0]) * 12 / 10^6         # ajout avril 2015 : si PAR_shaded tr?s faible, P_shaded <0, d? ? p3

  P_sunlit = ((Pmax + p3)* (1 - exp(-p4*PAR[PAR>0] / (Pmax + p3)))) - p3
  photo_sunlit = 3600 * sum(P_sunlit[P_sunlit>0] * LA_sunlit[P_sunlit>0]) * 12/10^6                         # ajout avril 2015 : si PAR_shaded tr?s faible, P_shaded <0, d? ? p3

# assimilation sur la journ?e (gC/j)
photo = photo_shaded + photo_sunlit

# r?serves facilement utilisables (gC)
reserve_m = (reserve_leaf * r_mobile_leaf) + (reserve_stem * r_mobile_stem)

# assimilats disponibles totaux
assimilats = photo + reserve_m

# r?serves difficilement utilisables (gC)
reserve_nm_leaf  = reserve_leaf * (1 - r_mobile_leaf)
reserve_nm_stem   = reserve_stem *  (1- r_mobile_stem)

# ==============================================================================
# MAINTENANCE ET CROISSANCE DU FRUIT
# ==============================================================================

MR_stem    = MRR_stem/24 * (Q10_stem^((T_air - 20)/10)) * (DM_structure_stem + (reserve_stem / cc_stem))   # en gC
MR_fruit     = MRR_fruit/24 * (Q10_fruit^((T_fruit -20)/10)) * DM_fruit_previous                              # en gC
MR_leaf  = MRR_leaf * (Q10_leaf^((T_air -20)/10)) * (DM_structure_leaf + reserve_leaf / cc_leaf)          # en gC
MR_leaf = sum(MR_leaf[PAR == 0])                      # Respiration des feuilles uniquement pendant la nuit.
MR_fruit =     sum(MR_fruit)
MR_stem =    sum(MR_stem)

MR_repo = MR_fruit
MR_veget = MR_stem + MR_leaf

      # RAPPEL : r?gles de priorit? d'utiolisation des assimilats :
      # 1- maitenance, 2- croissance reproductive, 3- mise en r?serve dans rameau et feuilles

# 1? utilisation des assimilats disponibles pour la respiration d'entretien

if (assimilats >= MR_veget)   {
    remains_1 = assimilats - MR_veget
}
else {
    if (assimilats + reserve_nm_leaf >= MR_veget) {
        remains_1 = 0
        reserve_nm_leaf = assimilats + reserve_nm_leaf - MR_veget
    }
    else {
        if (assimilats + reserve_nm_leaf + reserve_nm_stem >= MR_veget) {
            remains_1 = 0
            reserve_nm_stem = assimilats + reserve_nm_leaf + reserve_nm_stem - MR_veget
            reserve_nm_leaf <- 0
        }
        else {
            remains_1 = 0
            error = c("Les parties vegetatives s'etouffent: le systeme meurt ...")
            if (exists('idsimu')) {
                write.csv(data.frame(error), paste(localdir,"/tmp/failed-",idsimu,".csv",sep=''))
            } else {
                write.csv(data.frame(error), paste(localdir,"/tmp/r.csv",sep=''), row.names=FALSE)
            }
            stop("Les parties vegetatives s'etouffent: le systeme meurt ...\n")
            return (NULL)
        }
    }
}

#if(is.nan( remains_1)) {  remains_1 = 0 }

if ( remains_1 < MR_repo ) {                                                      ### Sitution d?favorable pour le fruit.
    besoin.fruit <- (MR_fruit - remains_1) / cc_fruit
    if (besoin.fruit >= DM_fruit_previous) {
        error = c("Les parties reproductrices s'etouffent: le systeme meurt ...")
        if (exists('idsimu')) {
            write.csv(data.frame(error), paste(localdir,"/tmp/failed-",idsimu,".csv",sep=''))
        } else {
            write.csv(data.frame(error), paste(localdir,"/tmp/r.csv",sep=''), row.names=FALSE)
        }
        stop("Les parties reproductrices s'etouffent: le systeme meurt ...\n")
    }
    else {
        DM_fruit_previous = DM_fruit_previous - besoin.fruit          # le fruit pompe sur ses r?serves
    }
}

remains_2 <- max(0, remains_1 - MR_repo)


#------ 2? utilisation de ce qui reste pour la croissance du fruit

DM_fruit  = DM_fruit_previous + (min(D_fruit,remains_2)/(cc_fruit + GRC_fruit))

# ======================================================================================
# MISE EN RESERVE de ce qui reste
# ===========================================================================================

remains_3 <- remains_2 - min(D_fruit,remains_2)                                        # Ce qui n'est pas pris par le fruit et qui va dans les r?serves. Distribution rameaux et feuille

reserve_stem_provi    = reserve_nm_stem + min(remains_3, reserve_stem * r_mobile_stem)
reserve_leaf_provi  = reserve_nm_leaf + max(0, remains_3 - reserve_stem * r_mobile_stem)

# cr?ation d'un reserve_MAX de r?serves qui peuvent ?tre stock?es chaque jour :
          # part des r?serves/unit? de struc * nb de strctures (ie nb de feuilles)
reserve_MAX <- (r_storage/(1 - r_storage)) * DM_structure_leaf * cc_leaf

if (reserve_leaf_provi > reserve_MAX){
      reserve_leaf = reserve_MAX
      reserve_stem  = reserve_leaf_provi - reserve_MAX + reserve_stem_provi
}
else {
      reserve_leaf = reserve_leaf_provi
      reserve_stem = reserve_stem_provi
}

Resultats = list( DM_fruit = DM_fruit,
                  reserve_leaf = reserve_leaf,
                  reserve_stem = reserve_stem)

return(Resultats)
}
