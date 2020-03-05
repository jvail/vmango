import numpy as np
import pandas as pd

from openalea.vmango.simulation.fruitmodel.fruitmodel_error import FruitModelValueError


### Fonction MS ? partir Model MS Lechaudel, 2005
def fruit_dry_mass_growth(
    Rayonnement,            # En watts.m-2
    Temperature_Air,        # Temp?rature horaire de l'air
    Temperature_Fruit,      # Temp?rature horaire du fruit.
    envirlum,               # Evolution de l'environnement lumineux dans la journ?e
    Poids_Fruit_Init,       # Poids du fruit ? la fin de la division cellulaire en gramme de MS
    MS_Fruit_Precedent,     # en gramme de MS
    Reserve_Rameau,         # en gramme de carbone
    Reserve_Feuille,        # en gramme de carbone
    LF                      # Rapport feuille / fruit [10, 150]
):


    Delta_DDJ_Journee = np.sum((Temperature_Fruit - 16)/24)                            # Accumulation de DDJ dans la journ?e
    RG = Rayonnement / 3600 * 10000                                                 # transformation du RG en J/cm2/h en W/m2
    PAR = RG * 0.5 * 4.6                                                            # Transformation du RG en watts/m2 en Rayonnement Photosynth?tiquement Actif (papier Varlet-Grancher et al. 1989 dans Agronomie, vol 9:419-139)

    # PARAMETRES POUR ASSIMILATION
    # k1 et k2 : coefficients de pond?ration du rayonnement inter et intra-rameau
    k1 = envirlum
    k2 = np.array([0.88] * 24)

    # PARAMETRES PHOTOSYNTHESE fixe
    p1 = 3.85                                       # coefficients relation Anet = f(PPF)
    p2 = 33.23
    p3 = 0.483
    p4 = 0.034
    r3 = 0.0529                                     # coefficient relation PPFshaded = f(PPFsunlit)

    # PARAMETRES POUR RESPIRATIONS D'ENTRETIEN
    MRR_rameau = 0.000858                           # Respiration de maintenance rameau
    MRR_feuilles = 0.000156                         # Respiration de maintenance feuilles
    MRR_fruits = 0.00115                            # Respiration de maintenance fruit
    Q10_rameau = 1.96                               # Q10 rameau
    Q10_feuilles = 2.11                             # Q10 feuilles
    Q10_fruits = 1.9                                # Q10 fruits

    # PARAMETRES POUR DEMANDE DU FRUIT
    gamma_feuilles = 0.0162                         # remobilisation des r?serves, papier 2005
    gamma_rameau = 0.0164                           # remobilisation des r?serves, papier 2005
    cram = 0.4387                                   # concentration carbone rameau
    cfeuil = 0.4051                                 # concentration carbone feuille
    cfruit = 0.4239                                 # concentration carbone fruit
    GRCfruit = 0.04                                 # coefficient de respiration de croissance (gCO2.gMS)

    # PARAMETRE LOGISTIQUE FONCTION DES DDJ
    RGRini_fruit =  0.0105                          #  param papier 2005
    a_fruit =       16.736                          #  param papier 2005
    b_fruit =       0.624                           #  param papier 2005
    psi =           0.3                             # ?
    DMfmax =  a_fruit * (Poids_Fruit_Init**b_fruit) # poids maximum du fruit.

    # PARAMETRES DES STRUCTURES
    poids_rameau = (41.83 + 77.41) / 2              # moy exp?, fixe
    poids_feuilles = 0.8                            # poids sec feuille, fixe
    partMS_reserves0_rameau = 0.1                   # moy exp?
    partMS_reserves0_feuilles = 0.074               # moy exp?

    # PARAMETRES DE STRUCTURES
    Structure_Rameau = poids_rameau * (1 - partMS_reserves0_rameau)                 # partie structure du remeau en gC.
    Structure_Feuille = poids_feuilles * (1 - partMS_reserves0_feuilles) * LF       # partie structure des feuilles en gC.

    Surf_Fol = 0.0051 * LF**0.937                                                   # Estimation de la surface folliaire calcul surface foliaire (m2)

    # ASSIMILATION DE CARBONE

    # demande de croissance du fruit (gC/j) , ce que veux le fruit en fonction de la croissance potentielle.
    Dfruit =  MS_Fruit_Precedent * RGRini_fruit * Delta_DDJ_Journee * (1 - (MS_Fruit_Precedent / DMfmax)) * (cfruit + GRCfruit)

    # calcul de la photosynth?se maximale, est fonction de la demande du fruit, surface folaire (la demande tient compte des co?ts (en C) de construction )
    Pmax = (p1 * (Dfruit / Surf_Fol) * p2) / (p1 * (Dfruit / Surf_Fol) + p2)
    Pmax = min(Pmax.min(), 15)        # Plafonnement de la demande du fruit
    # if (Pmax < 5)  { Pmax = 5  }      # Si pas de fruit, on limite le Pmax à 5.
    #### MODIF MAY17

    # CALCUL DES ASSIMILATS SUR LA JOURNEE OFFRE DE LA JOURNEE, production et mobilisation des r?serves.
    Surf_Fol_Sol = k1[PAR>0] * k2[PAR>0] * Surf_Fol # on calcul la surface folaire en plein soleil
    Surf_Fol_Omb = Surf_Fol - Surf_Fol_Sol

    #calcul du rayonnement a l'ombre a l'aide d'une fonction de ponderation du PPFD
    PAR_omb = r3 * PAR
    photomb = ((Pmax + p3) * (1 - np.exp(- p4 * PAR_omb[PAR_omb>0] / (Pmax + p3)))) - p3
    assiomb = 3600 * sum (photomb[photomb>0] *  Surf_Fol_Omb[photomb>0]) * 12 / 10**6    # ajout avril 2015 : si PAR.omb tr?s faible, photomb <0, d? ? p3

    photsol = ((Pmax + p3) * (1 - np.exp(-p4*PAR[PAR>0] / (Pmax + p3)))) - p3
    assisol = 3600 * sum(photsol[photsol>0] * Surf_Fol_Sol[photsol>0]) * 12 / 10**6 # ajout avril 2015 : si PAR.omb tr?s faible, photomb <0, d? ? p3

    # assimilation sur la journ?e (gC/j)
    photo_fol = assiomb + assisol

    # r?serves facilement utilisables (gC)
    Reserve_Facile_Util = (Reserve_Feuille * gamma_feuilles) + (Reserve_Rameau * gamma_rameau)

    # assimilats disponibles totaux
    assimilats = photo_fol + Reserve_Facile_Util

    # r?serves difficilement utilisables (gC)
    Reserve_Dif_Util_Feuille = Reserve_Feuille * (1 - gamma_feuilles)
    Reserve_Dif_Util_Rameau = Reserve_Rameau * (1- gamma_rameau)

    # ==============================================================================
    # MAINTENANCE ET CROISSANCE DU FRUIT
    # ==============================================================================

    # en gC
    Respiration_Rameau = MRR_rameau / 24 * (Q10_rameau**((Temperature_Air - 20) / 10)) * (Structure_Rameau + (Reserve_Rameau / cram))
    # en gC
    Respiration_Fruit = MRR_fruits / 24 * (Q10_fruits**((Temperature_Fruit -20)/10)) * MS_Fruit_Precedent
    # en gC
    Respiration_Feuilles = MRR_feuilles * (Q10_feuilles**((Temperature_Air -20)/10)) * (Structure_Feuille + Reserve_Feuille / cfeuil)
    # Respiration des feuilles uniquement pendant la nuit.
    Respiration_Feuilles = sum(Respiration_Feuilles[PAR == 0])
    Respiration_Fruit = sum(Respiration_Fruit)
    Respiration_Rameau = sum(Respiration_Rameau)

    RE_fruct = Respiration_Fruit
    RE_veget = Respiration_Rameau + Respiration_Feuilles

    # RAPPEL : r?gles de priorit? d'utiolisation des assimilats :
    # 1- maitenance, 2- croissance reproductive, 3- mise en r?serve dans rameau et feuilles

    # 1? utilisation des assimilats disponibles pour la respiration d'entretien

    if assimilats >= RE_veget:
        Reste_RE = assimilats - RE_veget
    else:
        if assimilats + Reserve_Dif_Util_Feuille >= RE_veget:
            Reste_RE = 0
            Reserve_Dif_Util_Feuille = assimilats + Reserve_Dif_Util_Feuille - RE_veget
        else:
            if assimilats + Reserve_Dif_Util_Feuille + Reserve_Dif_Util_Rameau >= RE_veget:
                Reste_RE = 0
                Reserve_Dif_Util_Rameau = assimilats + Reserve_Dif_Util_Feuille + Reserve_Dif_Util_Rameau - RE_veget
                Reserve_Dif_Util_Feuille = 0
            else:
                Reste_RE = 0
                error = pd.DataFrame({ 'error': ['Les parties vegetatives s\'etouffent: le systeme meurt ...'] })
                if 'idsimu' in globals():
                    error.to_csv(f'/tmp/failed-{idsimu}.csv', index=False)
                else:
                    error.to_csv('tmp/py.csv', index=False)
                raise FruitModelValueError(f'Les parties vegetatives s\'etouffent: le systeme meurt ...')

    ### Sitution d?favorable pour le fruit.
    if Reste_RE < RE_fruct:
        besoin_fruit = (Respiration_Fruit - Reste_RE) / cfruit
        if besoin_fruit >= MS_Fruit_Precedent:
            error = pd.DataFrame({ 'error': ['Les parties reproductrices s\'etouffent: le systeme meurt ...'] })
            if 'idsimu' in globals():
                error.to_csv(f'/tmp/failed-{idsimu}.csv', index=False)
            else:
                error.to_csv('tmp/py.csv', index=False)
            raise FruitModelValueError(f'Les parties reproductrices s\'etouffent: le systeme meurt ...')
        else:
            # le fruit pompe sur ses r?serves
            MS_Fruit_Precedent = MS_Fruit_Precedent - besoin_fruit

    Reste1 = max(0, Reste_RE - RE_fruct)

    #------ 2 utilisation de ce qui reste pour la croissance du fruit

    MS_Fruit_New  = MS_Fruit_Precedent + (min(Dfruit, Reste1) / (cfruit + GRCfruit))

    # ======================================================================================
    # MISE EN RESERVE de ce qui reste
    # ===========================================================================================

    # Ce qui n'est pas pris par le fruit et qui va dans les r?serves. Distribution rameaux et feuille
    Reste2 = Reste1 - min(Dfruit, Reste1)

    Res_rameau_provi = Reserve_Dif_Util_Rameau + min(Reste2, Reserve_Rameau * gamma_rameau)
    Res_feuilles_provi = Reserve_Dif_Util_Feuille + max(0, Reste2 - Reserve_Rameau * gamma_rameau)

    # cr?ation d'un seuil de r?serves qui peuvent ?tre stock?es chaque jour :
            # part des r?serves/unit? de struc * nb de strctures (ie nb de feuilles)
    seuil = (psi / (1 - psi)) * Structure_Feuille * cfeuil

    if Res_feuilles_provi > seuil:
        Reserve_Feuille_New = seuil
        Reserve_Rameau_New  = Res_feuilles_provi - seuil + Res_rameau_provi
    else:
        Reserve_Feuille_New = Res_feuilles_provi
        Reserve_Rameau_New = Res_rameau_provi

    # pd.DataFrame([[local[0], type(local[1]).__name__] for local in locals().items()], columns=['name', 'type']).to_csv(f'{__name__ }_vars.csv', index=False, sep=';')

    return pd.DataFrame({
        'MS_Fruit': [MS_Fruit_New],
        'Reserve_Feuille': [Reserve_Feuille_New],
        'Reserve_Rameau': [Reserve_Rameau_New]
    })
