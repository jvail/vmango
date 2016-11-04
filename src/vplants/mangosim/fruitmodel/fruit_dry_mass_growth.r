
### Fonction MS à partir Model MS Lechaudel, 2005

CROISSANCE_MS = function( Rayonnement,                                          # En watts.m-2
                          Temperature_Air,                                      # Température horaire de l'air
                          Temperature_Fruit,                                    # Température horaire du fruit.
                          envirlum,                                             # Evolution de l'environnement lumineux dans la journée
                          Poids_Fruit_Init,                                     # Poids du fruit à la fin de la division cellulaire en gramme de MS
                          MS_Fruit_Precedent,                                   # en gramme de MS
                          Reserve_Rameau,                                       # en gramme de carbone
                          Reserve_Feuille,                                      # en gramme de carbone
                          LF                                                    # Rapport feuille / fruit [10, 150] 
                        )

    {
Delta_DDJ_Journee = sum((Temperature_Fruit - 16)/24)                            # Accumulation de DDJ dans la journée
RG = Rayonnement / 3600 * 10000                                                 # transformation du RG en J/cm2/h en W/m2
PAR = RG * 0.5 * 4.6                                                            # Transformation du RG en watts/m2 en Rayonnement Photosynthétiquement Actif (papier Varlet-Grancher et al. 1989 dans Agronomie, vol 9:419-139)

#--------------------------------------------------------- Paramètres du Modèle ----------------------------------------------------------------------------------------------------------------

# PARAMETRES POUR ASSIMILATION
# k1 et k2 : coefficients de pondération du rayonnement inter et intra-rameau
    k1.fin <- envirlum
    k2.fin <- rep(0.88,24)

# PARAMETRES PHOTOSYNTHESE fixe 
    p1 <- 3.85                                                                  # coefficients relation Anet = f(PPF)
    p2 <- 33.23
    p3 <- 0.483
    p4 <- 0.034
    r3 <- 0.0529                                                                # coefficient relation PPFshaded = f(PPFsunlit)

# PARAMETRES POUR RESPIRATIONS D'ENTRETIEN
    MRR_rameau <- 0.000858                                                      # Respiration de maintenance rameau
    MRR_feuilles <- 0.000156                                                    # Respiration de maintenance feuilles
    MRR_fruits <- 0.00115                                                       # Respiration de maintenance fruit
    Q10_rameau <- 1.96                                                          # Q10 rameau
    Q10_feuilles <- 2.11                                                        # Q10 feuilles
    Q10_fruits <- 1.9                                                           # Q10 fruits

# PARAMETRES POUR DEMANDE DU FRUIT 
    gamma.feuilles <- 0.0162                                                    # remobilisation des réserves, papier 2005
    gamma.rameau <- 0.0164                                                      # remobilisation des réserves, papier 2005
    cram <- 0.4387                                                              # concentration carbone rameau
    cfeuil <- 0.4051                                                            # concentration carbone feuille
    cfruit <- 0.4239                                                            # concentration carbone fruit
    GRCfruit <- 0.04                                                            # coefficient de respiration de croissance (gCO2.gMS)

# PARAMETRE LOGISTIQUE FONCTION DES DDJ
    RGRini.fruit =  0.0105                                                      #  param papier 2005
    a.fruit =       16.736                                                      #  param papier 2005
    b.fruit =       0.624                                                       #  param papier 2005
    psi =           0.3                                                         # ?
    DMfmax =  a.fruit * (Poids_Fruit_Init ^ b.fruit)                            # poids maximum du fruit.

# PARAMETRES DES STRUCTURES    
    poids.rameau <- (41.83 + 77.41) / 2                                         # moy expé, fixe
    poids.feuilles <- 0.8                                                       # poids sec feuille, fixe
    partMS.reserves0.rameau <- 0.1                                              # moy expé
    partMS.reserves0.feuilles <- 0.074                                          # moy expé

# PARAMETRES DE STRUCTURES
  Structure_Rameau = poids.rameau * (1 - partMS.reserves0.rameau)               # partie structure du remeau en gC. 
  Structure_Feuille = poids.feuilles * (1 - partMS.reserves0.feuilles) * LF     # partie structure des feuilles en gC.                                                 

    Surf_Fol <- 0.0051 * LF^0.937                                               # Estimation de la surface folliaire calcul surface foliaire (m2)   

# ==============================================================================
# ASSIMILATION DE CARBONE
# ==============================================================================

#----- demande de croissance du fruit (gC/j) , ce que veux le fruit en fonction de la croissance potentielle.
  Dfruit =  MS_Fruit_Precedent * RGRini.fruit * Delta_DDJ_Journee * (1-( MS_Fruit_Precedent / DMfmax)) * (cfruit + GRCfruit)
      
#----- calcul de la photosynthèse maximale, est fonction de la demande du fruit, surface folaire (la demande tient compte des coûts (en C) de construction )
  Pmax <- (p1 * (Dfruit / Surf_Fol) * p2) / (p1 * (Dfruit / Surf_Fol) + p2)
  if (Pmax >=15) {Pmax = 15}                                                    # Plafonnement de la demande du fruit

###----- CALCUL DES ASSIMILATS SUR LA JOURNEE OFFRE DE LA JOURNEE, production et mobilisation des réserves.

  Surf_Fol_Sol = k1.fin[PAR>0] * k2.fin[PAR>0] * Surf_Fol                       # on calcul la surface folaire en plein soleil
  Surf_Fol_Omb = Surf_Fol - Surf_Fol_Sol                                        # surface folaire à l'ombre

#calcul du rayonnement a l'ombre a l'aide d'une fonction de ponderation du PPFD
  PAR.omb = r3 * PAR
  photomb = ((Pmax + p3)* (1 - exp(- p4 * PAR.omb[PAR.omb>0] / (Pmax + p3))))- p3
  assiomb = 3600 * sum (photomb[photomb>0] *  Surf_Fol_Omb[photomb>0]) * 12 / 10^6         # ajout avril 2015 : si PAR.omb très faible, photomb <0, dû à p3
  
  photsol = ((Pmax + p3)* (1 - exp(-p4*PAR[PAR>0] / (Pmax + p3)))) - p3
  assisol = 3600 * sum(photsol[photsol>0] * Surf_Fol_Sol[photsol>0]) * 12/10^6                         # ajout avril 2015 : si PAR.omb très faible, photomb <0, dû à p3

# assimilation sur la journée (gC/j)
photo.fol = assiomb + assisol  

# réserves facilement utilisables (gC)
Reserve_Facile_Util = (Reserve_Feuille * gamma.feuilles) + (Reserve_Rameau * gamma.rameau)

# assimilats disponibles totaux
assimilats = photo.fol + Reserve_Facile_Util

# réserves difficilement utilisables (gC)
Reserve_Dif_Util_Feuille  = Reserve_Feuille * (1 - gamma.feuilles)
Reserve_Dif_Util_Rameau   = Reserve_Rameau *  (1- gamma.rameau)
 
# ==============================================================================
# MAINTENANCE ET CROISSANCE DU FRUIT
# ==============================================================================

Respiration_Rameau    = MRR_rameau/24 * (Q10_rameau^((Temperature_Air - 20)/10)) * (Structure_Rameau + (Reserve_Rameau / cram))   # en gC            
Respiration_Fruit     = MRR_fruits/24 * (Q10_fruits^((Temperature_Fruit -20)/10)) * MS_Fruit_Precedent                              # en gC
Respiration_Feuilles  = MRR_feuilles * (Q10_feuilles^((Temperature_Air -20)/10)) * (Structure_Feuille + Reserve_Feuille / cfeuil)          # en gC
Respiration_Feuilles = sum(Respiration_Feuilles[PAR == 0])                      # Respiration des feuilles uniquement pendant la nuit.
Respiration_Fruit =     sum(Respiration_Fruit)
Respiration_Rameau =    sum(Respiration_Rameau)  

RE.fruct = Respiration_Fruit                
RE.veget = Respiration_Rameau + Respiration_Feuilles

      # RAPPEL : règles de priorité d'utiolisation des assimilats : 
      # 1- maitenance, 2- croissance reproductive, 3- mise en réserve dans rameau et feuilles  
       
# 1° utilisation des assimilats disponibles pour la respiration d'entretien
 
if (assimilats >= RE.veget)   { 
    Reste.RE = assimilats - RE.veget 
} 
else {
    if (assimilats + Reserve_Dif_Util_Feuille >= RE.veget) { 
        Reste.RE = 0
        Reserve_Dif_Util_Feuille = assimilats + Reserve_Dif_Util_Feuille - RE.veget 
    } 
    else {
        if (assimilats + Reserve_Dif_Util_Feuille + Reserve_Dif_Util_Rameau >= RE.veget) { 
            Reste.RE = 0
            Reserve_Dif_Util_Rameau = assimilats + Reserve_Dif_Util_Feuille + Reserve_Dif_Util_Rameau - RE.veget
            Reserve_Dif_Util_Feuille <- 0 
        } 
        else {
            stop("Les parties vegetatives s'etouffent: le systeme meurt ...\n")
            return (NULL)
        }
    }  
}

if (Reste.RE < RE.fruct) {                                                      ### Sitution défavorable pour le fruit.
    besoin.fruit <- (Respiration_Fruit - Reste.RE) / cfruit 
    if (besoin.fruit >= MS_Fruit_Precedent) {
        stop("Les parties reproductrices s'etouffent: le systeme meurt ...\n")
    } 
    else {
        MS_Fruit_Precedent = MS_Fruit_Precedent - besoin.fruit          # le fruit pompe sur ses réserves
    }
} 
 
Reste1 <- max(0, Reste.RE - RE.fruct)

           
#------ 2° utilisation de ce qui reste pour la croissance du fruit

MS_Fruit_New  = MS_Fruit_Precedent + (min(Dfruit,Reste1)/(cfruit + GRCfruit))
      
# ======================================================================================
# MISE EN RESERVE de ce qui reste
# ===========================================================================================

Reste2 <- Reste1 - min(Dfruit,Reste1)                                        # Ce qui n'est pas pris par le fruit et qui va dans les réserves. Distribution rameaux et feuille

Res.rameau.provi    = Reserve_Dif_Util_Rameau + min(Reste2, Reserve_Rameau * gamma.rameau)
Res.feuilles.provi  = Reserve_Dif_Util_Feuille + max(0, Reste2 - Reserve_Rameau * gamma.rameau)

# création d'un seuil de réserves qui peuvent être stockées chaque jour :
          # part des réserves/unité de struc * nb de strctures (ie nb de feuilles) 
seuil <- (psi/(1 - psi)) * Structure_Feuille * cfeuil 

if (Res.feuilles.provi > seuil){
      Reserve_Feuille_New = seuil
      Reserve_Rameau_New  = Res.feuilles.provi - seuil + Res.rameau.provi
} 
else {
      Reserve_Feuille_New = Res.feuilles.provi
      Reserve_Rameau_New = Res.rameau.provi 
}

Resultats = list( MS_Fruit = MS_Fruit_New,
                  Reserve_Feuille = Reserve_Feuille_New,
                  Reserve_Rameau = Reserve_Rameau_New)

return(Resultats)
}  