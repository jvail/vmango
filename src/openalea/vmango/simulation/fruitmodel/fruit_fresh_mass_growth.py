import numpy as np
import pandas as pd
import datetime


def fruit_fresh_mass_growth(
    Date,
    Temperature_Air,
    rayo,
    humirela,
    ddj,
    Temp_air_moy,
    MSfruit,
    MF_Init,
    Eaupepu_Init,
    Poids_Fruit_Init,
    H,                  # Pression Seuil Y ~ H * Volume Fruit pour la croissance Parametre ? Estimer.
    Phiini,             #  Taux d'accroissement cellulaire
    DDini,              # DDJ ? partir duquel extensibilit? cellulaire (Phiini d?croit).
    Tau,                # Tau de d?croissance de l'extensibilit? cellulaire.
    aLf
):


    #----------------------------- Initialisation des donn?es de sorties et entr?es
    # MS dans la peau et la pulpe.
    MSpepu = 0.4086 * MSfruit[0]**0.7641 + 0.5219 * MSfruit[0]**1.0584
    # Eau dans la peau et la pulpe d?termin?e avec relation allom?trique.
    Eaupepu = Eaupepu_Init
    # @todo: check paper for parameters 0.7641 - 1 and 1.0584 - 1
    partpepu = 0.4086 * 0.7641 * (MSfruit[1])**(0.7641 - 1) + 0.5219 * 1.0584 * (MSfruit[1])**(1.0584 - 1)
    # D?rivi?e de la mati?re s?che dans la peau et la pulpe.

    #croissMS = partpepu * np.diff(MSfruit)
    #croissMS[croissMS < 0] = 0.0

    # simplified, because diff does allways returns a vec in py
    croissMS = partpepu * max(0, MSfruit[1] - MSfruit[0])

    # Masse fraiche du fruit
    Masse_Fruit = MF_Init
    Masse_Noyau = MF_Init - (MSpepu + Eaupepu)

    #print(MSpepu, Eaupepu, partpepu, croissMS, Masse_Fruit, MSfruit[0], MSfruit[1])


    # Results_jour = pd.DataFrame({
    #     'Potentiel_Hydrique': [np.nan],
    #     'P_Turgescence': [np.nan],
    #     'P_Osmotique': [np.nan],
    #     'Xyleme': [np.nan],
    #     'Phloeme': [np.nan],
    #     'Transpiration': [np.nan],
    #     'saccharose': [np.nan],
    #     'sucres_solubles': [np.nan],
    #     'acides_organiques': [np.nan]
    # })

    # Results_jour_suivant = pd.DataFrame({ 'Date': [np.nan], 'Masse_Fruit': [np.nan], 'MS_Fruit': [np.nan], 'Eaupepu': [np.nan] })

    #  Determination de la Pression Osmotique (P_Osmotique)

    degjour = np.mean(ddj)
    # Mati?re s?che dans la puple. Relation Allom?trique.
    MSpu = 0.8226 * MSpepu
    Eaupu = 0.8958 * Eaupepu

    # Calcul des proportion des diff?rents compos?s en fonction des relation allom?triques d?termin?es ? partir de la mati?re s?che Pulpe et DDJ.
    propmal = 0.06620651 + (-0.0000538797) * degjour + (-0.002464413) * MSpu + 2.406565e-006 * MSpu * degjour
    if propmal < 0:
        propmal = 0.0

    proppyr = 0.0006896104 + 1.613387e-006 * degjour + 0.00005063595 * MSpu + (-6.912509e-008) * MSpu * degjour
    if proppyr < 0:
        proppyr = 0.0

    propoxa = 0.004750718 + (-2.113094e-006) * degjour + (-0.00002965687) * MSpu + 0.0 * MSpu * degjour
    if propoxa < 0:
        propoxa = 0.0

    propK = 0.01394964 + (-5.234608e-006) * degjour + (-0.000288464) * MSpu + 2.682089e-007 * MSpu * degjour
    if propK < 0:
        propK = 0.0

    propMg = 0.00115595 + (-7.937479e-007) * degjour + (-0.00002320017) * MSpu + (2.344528e-008) * MSpu * degjour
    if propMg < 0:
        propMg = 0.0

    propCa = 0.001588606 + (-6.625787e-007) * degjour + (-0.0000228527) * MSpu + (1.514343e-008) * MSpu * degjour
    if propCa < 0:
        propCa = 0.0

    propNH4 = 0.000246011 + 3.741743e-007 * degjour + 0.00002495255 * MSpu + (-3.010081e-008) * MSpu * degjour
    if propNH4 < 0:
        propNH4 = 0.0

    propNa = 0.0001279568 + 8.15203e-008 * degjour + (-1.468235e-006) * MSpu + 0.0 * MSpu * degjour
    if propNa < 0:
        propNa = 0.0

    propglc = 0.08074145 + (-0.00006325543) * degjour + (-0.001161846) * MSpu + 1.161344e-006 * MSpu * degjour
    if propglc < 0:
        propglc = 0.0

    propfrc = 0.04972199 + 0.0000966001 * degjour + (-0.001078579) * MSpu + 0.0 * MSpu * degjour
    if propfrc < 0:
        propfrc = 0.0

    propami = -0.1708815 + 0.0004380411 * degjour + 0.01923022 * MSpu + (-0.00002059459) * MSpu * degjour
    if propami < 0:
        propami = 0.0

    propcit = 0.1625024 + (-0.0000640754) * degjour + 0.003906348 * MSpu + (-4.784292e-006) * MSpu * degjour
    if propcit < 0:
        propcit = 0.0

    propsac = 0.0 + (0.00017695) * degjour + (-0.007249) * MSpu + 9.03e-006 * MSpu * degjour
    if propsac < 0:
        propsac = 0.0

    #---- Calcul de la masse et du nombre de mol des diff?rents compos?s
    mmal = propmal * MSpu
    nmal = mmal / 134

    mcit = propcit * MSpu
    ncit = mcit / 192

    mpyr = proppyr * MSpu
    npyr = mpyr / 88


    moxa = propoxa * MSpu
    noxa = moxa / 90

    mK = propK * MSpu
    nK = mK / 39

    mMg = propMg * MSpu
    nMg = mMg / 24

    mCa = propCa * MSpu
    nCa = mCa / 40

    mNH4 = propNH4 * MSpu
    nNH4 = mNH4 / 18

    mNa = propNa * MSpu
    nNa = mNa / 23

    mg = propglc * MSpu
    nglc = mg / 180

    mf = propfrc * MSpu
    nfrc = mf / 180

    msa = propsac * MSpu
    nsac = msa / 342
    mam = propami * MSpu

    ncompsol = nmal + ncit + npyr + noxa + nK + nMg + nCa + nNH4 + nNa + nglc + nfrc + nsac
    Cssm = ncompsol / Eaupu
    # R gaz constant (cm3 bar mol-1 ?K-1
    R = 83
     # Calcul de la pression osmotique (MPa), 0.2 ?tant la pression osmotique due aux acides amin?s, constant
    P_Osmotique = (R / 10) * (Temp_air_moy + 273.15) * Cssm  + 0.2

    ###----- CALCUL DE LA TRANSPIRATION DU FRUIT -----------------

    # perm?abilit? cuticulaire en cm/jour (231 cm/h dans papier 2007)
    ro = 5544
    # calcul de la surface du fruit exp?rimental (en cm?)
    surfruit = 3.65 * (Masse_Fruit)**0.73
    # pression de vapeur saturante (Nobel, 1974)
    Petoile = 0.008048 * np.exp(0.0547 * Temp_air_moy)
    # concentration de vapeur d'eau ? saturation
    transpi_alpha = 18 * Petoile / (83 * (Temp_air_moy + 273.15))
    # calcul de la transpiration du fruit (g/j),  # 0.996 = HR dans les espaces remplis d'air ? l'int?rieur du fruit
    Transpiration_Fruit = surfruit * transpi_alpha * ro * (0.996 - (np.mean(humirela) / 100))

    # Determination de la pression de turgescence (P_Turgescence)-----------------------------------------------------------------------------------------------------
    # Calcul du potentiel hydrique arbre
    Potentiel_Hydrique_Arbre = 1.0 * np.mean(-0.6617105 + (-0.006940179 * Temperature_Air) + (0.007888208 * humirela) + (0.0000198265 * rayo))
    # variante possible, DDini fonction P0.fruit
    DDini = 20.769 * Poids_Fruit_Init + 518.87

    #### MODIF MAY17
    if degjour > DDini:
        Phi = Phiini * (Tau**(degjour - DDini))
    else:
        #- Variation de Phi (accroissement volume en fonction de Taux ? partir seuil DDJ).
        Phi = Phiini

    # calcul de la conductivit? globale
    # produit de la surface et du ratio (membrane composite/surface du fruit du fruit = a) et de la conductivit? hydraulique entre la tige et le fruit (Lf)
    A_Lf = surfruit * aLf

    # calcul du seuil de pression Y
    # param?tres papier 2007
    Yo = Vo = 0
    Y = Yo + H * (Eaupepu - Vo)

    #  Pturgescence ? partir equation 13 sans l'?lasticit?
    numerateur = Phi * Eaupepu * Y + A_Lf * (Potentiel_Hydrique_Arbre + P_Osmotique) - Transpiration_Fruit + croissMS / 1.60
    denominateur = Phi * Eaupepu + A_Lf
    P_Turgescence = numerateur / denominateur

    if P_Turgescence < Y:
        P_Turgescence = Potentiel_Hydrique_Arbre + P_Osmotique - (Transpiration_Fruit - (croissMS / 1.60)) / A_Lf

    if P_Turgescence < Yo:
        P_Turgescence = 0

    #- Calcul du potentiel hydrique du fruit.
    Potentiel_Hydrique_Fruit = P_Turgescence - P_Osmotique

    #- Entr?e d'eau par le xyleme
    Flux_Xyleme  = A_Lf * (Potentiel_Hydrique_Arbre - Potentiel_Hydrique_Fruit)
    #- Entr?e d'eau par le phloeme
    Flux_Phloeme = croissMS / 1.60

    # ---------------------------------- Bilan Hydrique et carbon? ------------------
    # Bilan carbon?
    MSpepu_new = MSpepu + croissMS
    # Bilan hydrique
    Eaupepu_new = Eaupepu + Flux_Xyleme + Flux_Phloeme - Transpiration_Fruit
    Masse_Noyau_new  = 0.1167 * (MSpepu_new + Eaupepu_new)
    Masse_Fruit_new  = MSpepu_new + Eaupepu_new + Masse_Noyau_new

    #---------------------------------- Sorties qualit? ------------------
    # somme glucose, sacchrose, fructose
    saccharose =  msa / Eaupu
    sucres_solubles = (msa+mg+mf) / Eaupu
    # somme acide citrique et malique
    acides_organiques = (mmal + mcit) /Eaupu
    # Results_jour['Potentiel_Hydrique'] = Potentiel_Hydrique_Fruit
    # Results_jour['P_Turgescence'] = P_Turgescence
    # Results_jour['P_Osmotique'] = P_Osmotique
    # Results_jour['Xyleme'] = Flux_Xyleme
    # Results_jour['Phloeme'] = Flux_Phloeme
    # Results_jour['Transpiration'] = Transpiration_Fruit

    # Results_jour_suivant['Date'] = Date + np.timedelta64(1, 'D')
    # Results_jour_suivant['Masse_Fruit'] = Masse_Fruit_new
    # Results_jour_suivant['MS_Fruit'] = MSfruit[1]
    # Results_jour_suivant['Eaupepu'] = Eaupepu_new

    # pd.DataFrame([[local[0], type(local[1]).__name__] for local in locals().items()], columns=['name', 'type']).to_csv(f'{__name__ }_vars.csv', index=False, sep=';')

    return ((
        Potentiel_Hydrique_Fruit,
        P_Turgescence,
        P_Osmotique,
        Flux_Xyleme,
        Flux_Phloeme,
        Transpiration_Fruit,
        saccharose,
        sucres_solubles,
        acides_organiques
    ), (
        Date + np.timedelta64(1, 'D'),
        Masse_Fruit_new,
        MSfruit[1],
        Eaupepu_new
    ))
