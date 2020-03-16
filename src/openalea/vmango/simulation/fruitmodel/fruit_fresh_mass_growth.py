import numpy as np
import pandas as pd
import datetime


def growth_FM(
    date,
    T_air,
    GR,
    RH,
    dd_cum,
    T_air_daily,
    DM_fruit,
    FM_fruit_ini,
    W_fleshpeel_ini,
    DM_fruit_0
):

    h = 0.002027                                           # Pression Seuil Y ~ H * V_olume Fruit, pour la croissance, Parametre ? Estimer.
    phi_max = 0.414                                        #  Taux d'accroissement cellulaire (MPa / jour, article 2007)
    dd_thresh = 2000                                             # DDJ ? partir duquel extensibilit? cellulaire (Phiini d?croit).
    tau = 0.966                                          # Tau de d?croissance de l'extensibilit? cellulaire.
    aLf = 0.3732

    #----------------------------- Initialisation des donn?es de sorties et entr?es
    # MS dans la peau et la pulpe.
    DM_fleshpeel = 0.4086 * DM_fruit[0]**0.7641 + 0.5219 * DM_fruit[0]**1.0584
    # Eau dans la peau et la pulpe d?termin?e avec relation allom?trique.
    W_fleshpeel = W_fleshpeel_ini
    # @todo: check paper for parameters 0.7641 - 1 and 1.0584 - 1
    # D?rivi?e de la m_ati?re s?che dans la peau et la pulpe.

    #DM_fleshpeel_growth = partpepu * np.diff(DM_fruit)
    #DM_fleshpeel_growth[DM_fleshpeel_growth < 0] = 0.0

    # simplified, because diff does allways returns a vec in py
    DM_fleshpeel_growth = 0.4086 * 0.7641 * (DM_fruit[1])**(0.7641 - 1) + 0.5219 * 1.0584 * (DM_fruit[1])**(1.0584 - 1) * (DM_fruit[1] - DM_fruit[0])
    DM_fleshpeel_growth = max(0, DM_fleshpeel_growth)
    # Masse fraiche du fruit
    FM_fruit = FM_fruit_ini
    FM_stone = FM_fruit - (DM_fleshpeel + W_fleshpeel)

    #print(DM_fleshpeel, W_fleshpeel, partpepu, DM_fleshpeel_growth, FM_fruit, DM_fruit[0], DM_fruit[1])


    # Results_jour = pd.DataFrame({
    #     'Potentiel_Hydrique': [np.nan],
    #     'turgor_pressure': [np.nan],
    #     'osmotic_pressure': [np.nan],
    #     'Xyleme': [np.nan],
    #     'Phloeme': [np.nan],
    #     'Transpiration': [np.nan],
    #     'sucrose': [np.nan],
    #     'soluble_sugars': [np.nan],
    #     'organic_acids': [np.nan]
    # })

    # Results_jour_suivant = pd.DataFrame({ 'date': [np.nan], 'FM_fruit': [np.nan], 'MS_Fruit': [np.nan], 'W_fleshpeel': [np.nan] })

    #  Determination de la Pression Osmotique (osmotic_pressure)

    dd_cum = np.mean(dd_cum)
    # Mati?re s?che dans la puple. Relation Allom?trique.
    MSpu = 0.8226 * DM_fleshpeel
    W_flesh = 0.8958 * W_fleshpeel

    # Calcul des prop_ortion des diff?rents compos?s en fonction des relation allom?triques d?termin?es ? partir de la m_ati?re s?che Pulpe et dd_cum.
    prop_mal = 0.06620651 + (-0.0000538797) * dd_cum + (-0.002464413) * MSpu + 2.406565e-006 * MSpu * dd_cum
    if prop_mal < 0:
        prop_mal = 0.0

    prop_pyr = 0.0006896104 + 1.613387e-006 * dd_cum + 0.00005063595 * MSpu + (-6.912509e-008) * MSpu * dd_cum
    if prop_pyr < 0:
        prop_pyr = 0.0

    prop_oxa = 0.004750718 + (-2.113094e-006) * dd_cum + (-0.00002965687) * MSpu + 0.0 * MSpu * dd_cum
    if prop_oxa < 0:
        prop_oxa = 0.0

    prop_K = 0.01394964 + (-5.234608e-006) * dd_cum + (-0.000288464) * MSpu + 2.682089e-007 * MSpu * dd_cum
    if prop_K < 0:
        prop_K = 0.0

    prop_Mg = 0.00115595 + (-7.937479e-007) * dd_cum + (-0.00002320017) * MSpu + (2.344528e-008) * MSpu * dd_cum
    if prop_Mg < 0:
        prop_Mg = 0.0

    prop_Ca = 0.001588606 + (-6.625787e-007) * dd_cum + (-0.0000228527) * MSpu + (1.514343e-008) * MSpu * dd_cum
    if prop_Ca < 0:
        prop_Ca = 0.0

    prop_NH4 = 0.000246011 + 3.741743e-007 * dd_cum + 0.00002495255 * MSpu + (-3.010081e-008) * MSpu * dd_cum
    if prop_NH4 < 0:
        prop_NH4 = 0.0

    prop_Na = 0.0001279568 + 8.15203e-008 * dd_cum + (-1.468235e-006) * MSpu + 0.0 * MSpu * dd_cum
    if prop_Na < 0:
        prop_Na = 0.0

    prop_glc = 0.08074145 + (-0.00006325543) * dd_cum + (-0.001161846) * MSpu + 1.161344e-006 * MSpu * dd_cum
    if prop_glc < 0:
        prop_glc = 0.0

    prop_frc = 0.04972199 + 0.0000966001 * dd_cum + (-0.001078579) * MSpu + 0.0 * MSpu * dd_cum
    if prop_frc < 0:
        prop_frc = 0.0

    prop_ami = -0.1708815 + 0.0004380411 * dd_cum + 0.01923022 * MSpu + (-0.00002059459) * MSpu * dd_cum
    if prop_ami < 0:
        prop_ami = 0.0

    prop_cit = 0.1625024 + (-0.0000640754) * dd_cum + 0.003906348 * MSpu + (-4.784292e-006) * MSpu * dd_cum
    if prop_cit < 0:
        prop_cit = 0.0

    prop_sac = 0.0 + (0.00017695) * dd_cum + (-0.007249) * MSpu + 9.03e-006 * MSpu * dd_cum
    if prop_sac < 0:
        prop_sac = 0.0

    #---- Calcul de la m_asse et du nombre de m_ol des diff?rents compos?s
    m_mal = prop_mal * MSpu
    n_mal = m_mal / 134

    m_cit = prop_cit * MSpu
    n_cit = m_cit / 192

    m_pyr = prop_pyr * MSpu
    n_pyr = m_pyr / 88


    m_oxa = prop_oxa * MSpu
    n_oxa = m_oxa / 90

    m_K = prop_K * MSpu
    n_K = m_K / 39

    m_Mg = prop_Mg * MSpu
    n_Mg = m_Mg / 24

    m_Ca = prop_Ca * MSpu
    n_Ca = m_Ca / 40

    m_NH4 = prop_NH4 * MSpu
    n_NH4 = m_NH4 / 18

    m_Na = prop_Na * MSpu
    n_Na = m_Na / 23

    m_g = prop_glc * MSpu
    n_glc = m_g / 180

    m_f = prop_frc * MSpu
    n_frc = m_f / 180

    m_sa = prop_sac * MSpu
    n_sac = m_sa / 342
    m_am = prop_ami * MSpu

    n_solutes = n_mal + n_cit + n_pyr + n_oxa + n_K + n_Mg + n_Ca + n_NH4 + n_Na + n_glc + n_frc + n_sac
    c_solutes = n_solutes / W_flesh
    # R gaz constant (cm3 bar m_ol-1 ?K-1
    R = 8.3
     # Calcul de la pression osmotique (MPa), 0.2 ?tant la pression osmotique due aux acides amin?s, constant
    osmotic_pressure = R * (T_air_daily + 273.15) * c_solutes  + 0.2

    ###----- CALCUL DE LA TRANSPIRATION DU FRUIT -----------------

    # perm?abilit? cuticulaire en cm/jour (231 cm/h dans papier 2007)
    ro = 5544
    # calcul de la surface du fruit exp?rimental (en cm?)
    A_fruit = 3.65 * (FM_fruit)**0.73
    # pression de vapeur saturante (Nobel, 1974)
    P_sat = 0.008048 * np.exp(0.0547 * T_air_daily)
    # concentration de vapeur d'eau ? saturation
    alpha = 18 * P_sat / (83 * (T_air_daily + 273.15))
    # calcul de la transpiration du fruit (g/j),  # 0.996 = HR dans les espaces remplis d'air ? l'int?rieur du fruit
    transpiration = A_fruit * alpha * ro * (0.996 - (np.mean(RH) / 100))

    # Determination de la pression de turgescence (turgor_pressure)-----------------------------------------------------------------------------------------------------
    # Calcul du potentiel hydrique arbre
    water_potential_stem = 1.0 * np.mean(-0.6617105 + (-0.006940179 * T_air) + (0.007888208 * RH) + (0.0000198265 * GR))
    # variante possible, dd_thresh fonction P0.fruit
    dd_thresh = 20.769 * DM_fruit_0 + 518.87

    #### MODIF MAY17
    if dd_cum > dd_thresh:
        Phi = phi_max * (tau**(dd_cum - dd_thresh))
    else:
        #- Variation de Phi (accroissement volume en fonction de Taux ? partir seuil dd_cum).
        Phi = phi_max

    # calcul de la conductivit? globale
    # produit de la surface et du ratio (membrane composite/surface du fruit du fruit = a) et de la conductivit? hydraulique entre la tige et le fruit (Lf)
    ALf = A_fruit * aLf

    # calcul du seuil de pression Y
    # param?tres papier 2007
    Y_o = V_o = 0
    Y = Y_o + h * (W_fleshpeel - V_o)

    #  Pturgescence ? partir equation 13 sans l'?lasticit?
    numerator = Phi * W_fleshpeel * Y + ALf * (water_potential_stem + osmotic_pressure) - transpiration + DM_fleshpeel_growth / 1.60
    denominator = Phi * W_fleshpeel + ALf
    turgor_pressure = numerator / denominator

    if turgor_pressure < Y:
        turgor_pressure = water_potential_stem + osmotic_pressure - (transpiration - (DM_fleshpeel_growth / 1.60)) / ALf

    if turgor_pressure < Y_o:
        turgor_pressure = 0

    #- Calcul du potentiel hydrique du fruit.
    water_potential = turgor_pressure - osmotic_pressure

    #- Entr?e d'eau par le xyleme
    flux_xyleme  = ALf * (water_potential_stem - water_potential)
    #- Entr?e d'eau par le phloeme
    flux_phloeme = DM_fleshpeel_growth / 1.60

    # ---------------------------------- Bilan Hydrique et carbon? ------------------
    # Bilan carbon?
    DM_fleshpeel = DM_fleshpeel + DM_fleshpeel_growth
    # Bilan hydrique
    W_fleshpeel = W_fleshpeel + flux_xyleme + flux_phloeme - transpiration
    FM_stone  = 0.1167 * (DM_fleshpeel + W_fleshpeel)
    FM_fruit  = DM_fleshpeel + W_fleshpeel + FM_stone

    #---------------------------------- Sorties qualit? ------------------
    # somme glucose, sacchrose, fructose
    sucrose =  m_sa / W_flesh
    soluble_sugars = (m_sa + m_g + m_f) / W_flesh
    # somme acide citrique et m_alique
    organic_acids = (m_mal + m_cit) / W_flesh

    return ((
        water_potential,
        turgor_pressure,
        osmotic_pressure,
        flux_xyleme,
        flux_phloeme,
        transpiration,
        sucrose,
        soluble_sugars,
        organic_acids
    ), (
        date + np.timedelta64(1, 'D'),
        FM_fruit,
        DM_fruit[1],
        W_fleshpeel
    ))
