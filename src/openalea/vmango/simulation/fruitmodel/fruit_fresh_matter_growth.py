#########################################################################################################################
###  FRUIT MODEL : FRUIT GROWTH IN FRESH MASS    ########################################################################
#########################################################################################################################

# Original model from : Léchaudel et al. 2007_Tree Physiology 27:219-230
# See also : Fishman and Génard 1998_Plant Cell Environ. 21:739-752
#            Léchaudel 2004_PhD

# - erratum parameter units : alpha [g cm-3] and tau [dimensionless]
# - flesh compartment : comprise flesh and peel

# Differences between the current R code and the original model :
# ------------------------------------------------------------------------------------------------------------------------
# - the model run at a daily time step (vs. hourly time step in the original model)
#   parameter units in Table A2 are updated : ro [cm day-1], aLf [g cm-2 MPa-1 day-1] and phi_max [MPa-1 day-1]
# - fruit growth is based only on plactic component (vs. plastic and elastic components in the original model)
#   Eq.12 becomes : dV/dt = dV_plas/dt
#   Eq.13 becomes : dV/dt = phi*V*(P_f - Y) if P_f > Y
#                   dV/dt = 0               if P_f ≤ Y
# - Eq.6-7 : contribution of amino acids to osmotic pressure (R*T*n_aa/w) is fixed at a constant (osmotic_pressure_aa)
# - MODIF 2017-05_1 : if not fixed as input (cf year-dependent calibrated value in the original model),
#                     dd_thresh value is empirically calculated from fruit dry mass at the end of cell division

import numpy as np
from math import exp

from .constants import R, density_W, MM_water, MM_mal, MM_cit, MM_pyr, MM_oxa, MM_K, MM_Mg, MM_Ca, MM_NH4, MM_Na, MM_glc, MM_frc, MM_suc

def growth_FM(
    date,
    T_air,
    GR,
    RH,
    dd_cum,
    TM_air,
    DM_fruit_0,
    DM_fruit,
    DM_fruit_previous,
    FM_fruit_previous,
    W_fleshpeel_previous,
    dd_thresh,
    params
):

    h = params.h
    phi_max = params.phi_max
    tau = params.tau
    aLf = params.aLf
    osmotic_pressure_aa = params.osmotic_pressure_aa
    ro = params.ro
    RH_fruit = params.RH_fruit
    Y_0 = params.Y_0
    V_0 = params.V_0
    psat_1 = params.psat_1
    psat_2 = params.psat_2
    density_DM = params.density_DM
    e_fruit2peelDM_1 = params.e_fruit2peelDM_1
    e_fruit2peelDM_2 = params.e_fruit2peelDM_2
    e_fruit2fleshDM_1 = params.e_fruit2fleshDM_1
    e_fruit2fleshDM_2 = params.e_fruit2fleshDM_2
    e_fleshpeel2fleshDM = params.e_fleshpeel2fleshDM
    e_fleshpeel2fleshW = params.e_fleshpeel2fleshW
    e_fruitFM2surface_1 = params.e_fruitFM2surface_1
    e_fruitFM2surface_2 = params.e_fruitFM2surface_2
    e_flesh2stoneFM = params.e_flesh2stoneFM
    swp_1 = params.swp_1
    swp_2 = params.swp_2
    swp_3 = params.swp_3
    swp_4 = params.swp_4
    ddthres_1 = params.ddthres_1
    ddthres_2 = params.ddthres_2
    delta_mal = params.delta.mal
    delta_cit = params.delta.cit
    delta_pyr = params.delta.pyr
    delta_oxa = params.delta.oxa
    delta_K = params.delta.K
    delta_Mg = params.delta.Mg
    delta_Ca = params.delta.Ca
    delta_NH4 = params.delta.NH4
    delta_Na = params.delta.Na
    delta_glc = params.delta.glc
    delta_frc = params.delta.frc
    delta_suc = params.delta.suc
    delta_sta = params.delta.sta

    # ========================================================================================================================
    # DRY MASS AND GROWTH RATE OF FRUIT FLESH
    # ========================================================================================================================
    ## from empirical relationships in Léchaudel (2004)

    DM_fleshpeel_previous = (e_fruit2fleshDM_1 * (DM_fruit_previous) ** e_fruit2fleshDM_2) + (e_fruit2peelDM_1 * (DM_fruit_previous) ** e_fruit2peelDM_2)
    DM_fleshpeel_delta = (e_fruit2fleshDM_1 * e_fruit2fleshDM_2 * (DM_fruit) ** (e_fruit2fleshDM_2 - 1) + e_fruit2peelDM_1 * e_fruit2peelDM_2 * (DM_fruit) ** (e_fruit2peelDM_2 - 1)) * (DM_fruit - DM_fruit_previous)
    DM_fleshpeel_delta = max(DM_fleshpeel_delta, 0)
    DM_flesh_previous = e_fleshpeel2fleshDM * DM_fleshpeel_previous
    W_flesh_previous = e_fleshpeel2fleshW * W_fleshpeel_previous

    # ========================================================================================================================
    # OSMOTIC PRESSURE IN THE FRUIT
    # ========================================================================================================================

    ## -- mass proportion of osmotically active solutes & starch in the dry mass of fruit flesh (eq.9) :
    DM_flesh_x_dd_cum = DM_flesh_previous * dd_cum
    prop_mal = max(0, delta_mal[0] + delta_mal[1] * dd_cum + delta_mal[2] * DM_flesh_previous + delta_mal[3] * DM_flesh_x_dd_cum)
    prop_cit = max(0, delta_cit[0] + delta_cit[1] * dd_cum + delta_cit[2] * DM_flesh_previous + delta_cit[3] * DM_flesh_x_dd_cum)
    prop_pyr = max(0, delta_pyr[0] + delta_pyr[1] * dd_cum + delta_pyr[2] * DM_flesh_previous + delta_pyr[3] * DM_flesh_x_dd_cum)
    prop_oxa = max(0, delta_oxa[0] + delta_oxa[1] * dd_cum + delta_oxa[2] * DM_flesh_previous + delta_oxa[3] * DM_flesh_x_dd_cum)
    prop_K = max(0, delta_K[0] + delta_K[1] * dd_cum + delta_K[2] * DM_flesh_previous + delta_K[3] * DM_flesh_x_dd_cum)
    prop_Mg = max(0, delta_Mg[0] + delta_Mg[1] * dd_cum + delta_Mg[2] * DM_flesh_previous + delta_Mg[3] * DM_flesh_x_dd_cum)
    prop_Ca = max(0, delta_Ca[0] + delta_Ca[1] * dd_cum + delta_Ca[2] * DM_flesh_previous + delta_Ca[3] * DM_flesh_x_dd_cum)
    prop_NH4 = max(0, delta_NH4[0] + delta_NH4[1] * dd_cum + delta_NH4[2] * DM_flesh_previous + delta_NH4[3] * DM_flesh_x_dd_cum)
    prop_Na  = max(0, delta_Na[0] + delta_Na[1] * dd_cum + delta_Na[2] * DM_flesh_previous + delta_Na[3] * DM_flesh_x_dd_cum)
    prop_glc = max(0, delta_glc[0] + delta_glc[1] * dd_cum + delta_glc[2] * DM_flesh_previous + delta_glc[3] * DM_flesh_x_dd_cum)
    prop_frc = max(0, delta_frc[0] + delta_frc[1] * dd_cum + delta_frc[2] * DM_flesh_previous + delta_frc[3] * DM_flesh_x_dd_cum)
    prop_suc = max(0, delta_suc[0] + delta_suc[1] * dd_cum + delta_suc[2] * DM_flesh_previous + delta_suc[3] * DM_flesh_x_dd_cum)
    prop_sta = max(0, delta_sta[0] + delta_sta[1] * dd_cum + delta_sta[2] * DM_flesh_previous + delta_sta[3] * DM_flesh_x_dd_cum)

    ## -- mass and number of moles of osmotically active solutes & starch in fruit flesh (eq.8) :
    mass_mal = prop_mal * DM_flesh_previous
    nmol_mal = mass_mal / MM_mal
    mass_cit = prop_cit * DM_flesh_previous
    nmol_cit = mass_cit / MM_cit
    mass_pyr = prop_pyr * DM_flesh_previous
    nmol_pyr = mass_pyr / MM_pyr
    mass_oxa = prop_oxa * DM_flesh_previous
    nmol_oxa = mass_oxa / MM_oxa
    mass_K = prop_K * DM_flesh_previous
    nmol_K = mass_K / MM_K
    mass_Mg = prop_Mg * DM_flesh_previous
    nmol_Mg = mass_Mg / MM_Mg
    mass_Ca = prop_Ca * DM_flesh_previous
    nmol_Ca = mass_Ca / MM_Ca
    mass_NH4 = prop_NH4 * DM_flesh_previous
    nmol_NH4 = mass_NH4 / MM_NH4
    mass_Na = prop_Na * DM_flesh_previous
    nmol_Na = mass_Na / MM_Na
    mass_glc = prop_glc * DM_flesh_previous
    nmol_glc = mass_glc / MM_glc
    mass_frc = prop_frc * DM_flesh_previous
    nmol_frc = mass_frc / MM_frc
    mass_suc = prop_suc * DM_flesh_previous
    nmol_suc = mass_suc / MM_suc
    mass_sta = prop_sta * DM_flesh_previous

    ## -- osmotic pressure in fruit flesh (eq.6-7) :
    nmol_solutes = nmol_mal + nmol_cit + nmol_pyr + nmol_oxa + nmol_K + nmol_Mg + nmol_Ca + nmol_NH4 + nmol_Na + nmol_glc + nmol_frc + nmol_suc
    osmotic_pressure_fruit = (R * (TM_air + 273.15) * nmol_solutes) / (W_flesh_previous / density_W) + osmotic_pressure_aa

    # ========================================================================================================================
    # FRUIT TRANSPIRATION
    # ========================================================================================================================

    ## -- fruit surface (eq.3) :
    A_fruit = e_fruitFM2surface_1 * FM_fruit_previous ** e_fruitFM2surface_2

    ## -- saturation vapor pressure (eq.3 in Fishman and Génard 1998) :
    ##    converted from bar to MPa
    P_sat = psat_1 * exp(psat_2 * TM_air) / 10

    ## -- fruit transpiration_fruit (eq.2) :
    alpha = MM_water * P_sat / (R * (TM_air + 273.15))
    transpiration_fruit = A_fruit * alpha * ro * (RH_fruit - RH / 100)

    # ========================================================================================================================
    # CELL WALL PROPERTIES OF THE FRUIT
    # ========================================================================================================================

    ## -- cell wall extensibility (eq.18) :
    if np.isnan(dd_thresh):                                                                                                       # _MODIF 2017-05_1
        ## if not fixed as input, set from an empirical relationship
        dd_thresh = ddthres_1 * DM_fruit_0 + ddthres_2

    Phi = phi_max * tau ** max(0, dd_cum - dd_thresh)

    # -- threshold pressure (eq.15-16) :
    V = W_fleshpeel_previous / density_W + DM_fleshpeel_previous / density_DM
    Y = Y_0 + h * (V - V_0)

    # ========================================================================================================================
    # TURGOR PRESSURE & WATER POTENTIAL IN THE FRUIT
    # ========================================================================================================================

    ## -- water potential of the stem :
    water_potential_stem = swp_1 + swp_2 * TM_air + swp_3 * RH + swp_4 * GR

    ## -- turgor pressure in the fruit (defined by combining eq.11 and eq.13) :
    ALf = A_fruit * aLf
    numerator = Phi * V * Y  +  ALf * (water_potential_stem + osmotic_pressure_fruit) / density_W - transpiration_fruit / density_W + DM_fleshpeel_delta / density_DM
    denominator = Phi * V + ALf / density_W
    turgor_pressure_fruit = numerator / denominator

    if turgor_pressure_fruit < Y:
        turgor_pressure_fruit = water_potential_stem + osmotic_pressure_fruit - (transpiration_fruit - DM_fleshpeel_delta * density_W / density_DM) / ALf

    if turgor_pressure_fruit < Y_0:
        turgor_pressure_fruit = 0

    ## -- water potential in the fruit (eq.5) :
    water_potential_fruit = turgor_pressure_fruit - osmotic_pressure_fruit

    # ========================================================================================================================
    # WATER AND DRY MATTER CHANGES IN FRUIT COMPARTMENTS
    # ========================================================================================================================

    ## -- rate of water inflow in the fruit from xylem and phloem (eq.4) :
    flux_xylem_phloem = ALf * (water_potential_stem - water_potential_fruit)

    ## -- changes in dry mass, fresh mass and water mass of fruit compartments :
    DM_fleshpeel = DM_fleshpeel_previous + DM_fleshpeel_delta
    W_fleshpeel = W_fleshpeel_previous + flux_xylem_phloem - transpiration_fruit
    FM_stone = e_flesh2stoneFM * (DM_fleshpeel + W_fleshpeel)
    FM_fruit = DM_fleshpeel + W_fleshpeel + FM_stone
    DM_flesh = DM_fleshpeel * e_fleshpeel2fleshDM
    W_flesh = W_fleshpeel * e_fleshpeel2fleshW

    FM_minus_stone = W_flesh_previous + DM_flesh_previous
    sucrose = mass_suc / FM_minus_stone
    glucose = mass_glc / FM_minus_stone
    fructose = mass_frc / FM_minus_stone
    soluble_sugars = (mass_suc + mass_glc + mass_frc) / FM_minus_stone
    starch = mass_sta / FM_minus_stone
    organic_acids = (mass_mal + mass_cit) / FM_minus_stone

    return ((
        water_potential_fruit,
        turgor_pressure_fruit,
        osmotic_pressure_fruit,
        flux_xylem_phloem,
        transpiration_fruit,
        sucrose,
        glucose,
        fructose,
        soluble_sugars,
        starch,
        organic_acids
    ), (
        date + np.timedelta64(1, 'D'),
        FM_fruit,
        DM_fruit,
        W_fleshpeel,
        DM_fleshpeel,
        W_flesh,
        DM_flesh
    ))
