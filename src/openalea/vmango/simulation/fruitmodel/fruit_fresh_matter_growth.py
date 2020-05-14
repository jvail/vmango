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

from .constants import R, d_W, MM_water, MM_mal, MM_cit, MM_pyr, MM_oxa, MM_K, MM_Mg, MM_Ca, MM_NH4, MM_Na, MM_glc, MM_frc, MM_suc

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

    h = params.h                                        # coeffcient of "cell wall hardening" [MPa cm-3]
    phi_max = params.phi_max                            # maximal cell wall extensibility [MPa-1 day-1]
    tau = params.tau                                    # rate of decrease in cell wall extensibility [°C-1 day-1]
    aLf = params.aLf                                    # produit between the ratio of the area of the vascular network to the fruit area (a) and hydraulic conductivity between the stem and the fruit (Lf)  [g cm-2 MPa-1 day-1]
    osmotic_pressure_aa = params.osmotic_pressure_aa    # osmotic pressure in the flesh due to amino acids [Mpa]
    ro = params.ro                                      # fruit surface conductance [cm day-1]
    RH_fruit = params.RH_fruit                          # relative humidity of air space in fruit [dimensionless]
    Y_0 = params.Y_0                                    # threshold pressure at full bloom [MPa-1 day-1]
    V_0 = params.V_0                                    # flesh volume at full bloom [cm3]
    s1 = params.s1                                      # specific parameter for saturation vapor pressure calculation
    s2 = params.s2                                      # specific parameter for saturation vapor pressure calculation
    d_DM = params.d_DM                                  # density of dry matter [g cm3]
    a5 = params.a5                                      # specific parameter for peel dry mass calculation [g DM 1-0.7641]
    a6 = params.a6                                      # specific parameter for peel dry mass calculation [dimensionless]
    a7 = params.a7                                      # specific parameter for flesh dry mass calculation [g DM 1-1.0584]
    a8 = params.a8                                      # specific parameter for flesh dry mass calculation [dimensionless]
    a9 = params.a9                                      # proportion of flesh in flesh and peel dry mass [g DM gDM-1]
    a10 = params.a10                                    # proportion of flesh in flesh and peel water mass [g DM gDM-1]
    a11 = params.a11                                    # specific parameter for fruit surface calculation [cm2 g-0.73]
    a12 = params.a12                                    # specific parameter for fruit surface calculation [dimensionless]
    a13 = params.a13                                    # proportion of stone in flesh and peel fresh mass [g FM gFM-1]
    a14 = params.a14                                    # specific parameter for stem water potential calculation
    a15 = params.a15                                    # specific parameter for stem water potential calculation
    a16 = params.a16                                    # specific parameter for stem water potential calculation
    a17 = params.a17                                    # specific parameter for stem water potential calculation
    a18 = params.a18                                    # specific paramater for cell wall degree-day threshold calculation [dd g DM-1]
    a19 = params.a19                                    # specific paramater for cell wall degree-day threshold calculation [dd]
    delta_mal = params.mass_prop_delta.mal              # malic acid
    delta_cit = params.mass_prop_delta.cit              # citric acid
    delta_pyr = params.mass_prop_delta.pyr              # pyruvic acid
    delta_oxa = params.mass_prop_delta.oxa              # oxalic acid
    delta_K = params.mass_prop_delta.K                  # K+
    delta_Mg = params.mass_prop_delta.Mg                # Mg2+
    delta_Ca = params.mass_prop_delta.Ca                # Ca2+
    delta_NH4 = params.mass_prop_delta.NH4              # NH4+
    delta_Na = params.mass_prop_delta.Na                # Na+
    delta_glc = params.mass_prop_delta.glc              # glucose
    delta_frc = params.mass_prop_delta.frc              # fructose
    delta_suc = params.mass_prop_delta.suc              # sucrose
    delta_sta = params.mass_prop_delta.sta              # starch

    # ========================================================================================================================
    # DRY MASS AND GROWTH RATE OF FRUIT FLESH
    # ========================================================================================================================
    ## from empirical relationships in Léchaudel (2004)

    DM_fleshpeel_previous = (a7 * (DM_fruit_previous) ** a8) + (a5 * (DM_fruit_previous) ** a6)
    DM_fleshpeel_growth = (a7 * a8 * (DM_fruit) ** (a8 - 1) + a5 * a6 * (DM_fruit) ** (a6 - 1)) * (DM_fruit - DM_fruit_previous)
    DM_fleshpeel_growth = max(DM_fleshpeel_growth, 0)
    DM_flesh_previous = a9 * DM_fleshpeel_previous
    W_flesh_previous = a10 * W_fleshpeel_previous

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
    m_mal = prop_mal * DM_flesh_previous
    n_mal = m_mal / MM_mal
    m_cit = prop_cit * DM_flesh_previous
    n_cit = m_cit / MM_cit
    m_pyr = prop_pyr * DM_flesh_previous
    n_pyr = m_pyr / MM_pyr
    m_oxa = prop_oxa * DM_flesh_previous
    n_oxa = m_oxa / MM_oxa
    m_K = prop_K * DM_flesh_previous
    n_K = m_K / MM_K
    m_Mg = prop_Mg * DM_flesh_previous
    n_Mg = m_Mg / MM_Mg
    m_Ca = prop_Ca * DM_flesh_previous
    n_Ca = m_Ca / MM_Ca
    m_NH4 = prop_NH4 * DM_flesh_previous
    n_NH4 = m_NH4 / MM_NH4
    m_Na = prop_Na * DM_flesh_previous
    n_Na = m_Na / MM_Na
    m_glc = prop_glc * DM_flesh_previous
    n_glc = m_glc / MM_glc
    m_frc = prop_frc * DM_flesh_previous
    n_frc = m_frc / MM_frc
    m_suc = prop_suc * DM_flesh_previous
    n_suc = m_suc / MM_suc
    m_sta = prop_sta * DM_flesh_previous

    ## -- osmotic pressure in fruit flesh (eq.6-7) :
    n_solutes = n_mal + n_cit + n_pyr + n_oxa + n_K + n_Mg + n_Ca + n_NH4 + n_Na + n_glc + n_frc + n_suc
    osmotic_pressure = (R * (TM_air + 273.15) * n_solutes) / (W_flesh_previous / d_W) + osmotic_pressure_aa

    # ========================================================================================================================
    # FRUIT TRANSPIRATION
    # ========================================================================================================================

    ## -- fruit surface (eq.3) :
    A_fruit = a11 * FM_fruit_previous ** a12

    ## -- saturation vapor pressure (eq.3 in Fishman and Génard 1998) :
    ##    converted from bar to MPa
    P_sat = s1 * exp(s2 * TM_air) / 10

    ## -- fruit transpiration (eq.2) :
    alpha = MM_water * P_sat / (R * (TM_air + 273.15))
    transpiration = A_fruit * alpha * ro * (RH_fruit - RH / 100)

    # ========================================================================================================================
    # CELL WALL PROPERTIES OF THE FRUIT
    # ========================================================================================================================

    ## -- cell wall extensibility (eq.18) :
    if np.isnan(dd_thresh):                                                                                                       # _MODIF 2017-05_1
        ## if not fixed as input, set from an empirical relationship
        dd_thresh = a18 * DM_fruit_0 + a19

    Phi = phi_max * tau ** max(0, dd_cum - dd_thresh)

    # -- threshold pressure (eq.15-16) :
    V = W_fleshpeel_previous / d_W + DM_fleshpeel_previous / d_DM
    Y = Y_0 + h * (V - V_0)

    # ========================================================================================================================
    # TURGOR PRESSURE & WATER POTENTIAL IN THE FRUIT
    # ========================================================================================================================

    ## -- water potential of the stem :
    water_potential_stem = a14 + a15 * TM_air + a16 * RH + a17 * GR

    ## -- turgor pressure in the fruit (defined by combining eq.11 and eq.13) :
    ALf = A_fruit * aLf
    numerator = Phi * V * Y  +  ALf * (water_potential_stem + osmotic_pressure) / d_W - transpiration / d_W + DM_fleshpeel_growth / d_DM
    denominator = Phi * V + ALf / d_W
    turgor_pressure = numerator / denominator

    if turgor_pressure < Y:
        turgor_pressure = water_potential_stem + osmotic_pressure - (transpiration - DM_fleshpeel_growth * d_W / d_DM) / ALf

    if turgor_pressure < Y_0:
        turgor_pressure = 0

    ## -- water potential in the fruit (eq.5) :
    water_potential = turgor_pressure - osmotic_pressure

    # ========================================================================================================================
    # WATER AND DRY MATTER CHANGES IN FRUIT COMPARTMENTS
    # ========================================================================================================================

    ## -- rate of water inflow in the fruit from xylem and phloem (eq.4) :
    flux_xylem_phloem = ALf * (water_potential_stem - water_potential)

    ## -- changes in dry mass, fresh mass and water mass of fruit compartments :
    DM_fleshpeel = DM_fleshpeel_previous + DM_fleshpeel_growth
    W_fleshpeel = W_fleshpeel_previous + flux_xylem_phloem - transpiration
    FM_stone = a13 * (DM_fleshpeel + W_fleshpeel)
    FM_fruit = DM_fleshpeel + W_fleshpeel + FM_stone
    DM_flesh = DM_fleshpeel * a9
    W_flesh = W_fleshpeel * a10

    FM_minus_stone = W_flesh_previous + DM_flesh_previous
    sucrose = m_suc / FM_minus_stone
    glucose = m_glc / FM_minus_stone
    fructose = m_frc / FM_minus_stone
    soluble_sugars = (m_suc + m_glc + m_frc) / FM_minus_stone
    starch = m_sta / FM_minus_stone
    organic_acids = (m_mal + m_cit) / FM_minus_stone

    return ((
        water_potential,
        turgor_pressure,
        osmotic_pressure,
        flux_xylem_phloem,
        transpiration,
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
