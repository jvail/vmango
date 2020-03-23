import numpy as np

from openalea.vmango.constants import R, MM_water

def growth_FM(params):

    h = params.h                                        # coeffcient of "cell wall hardening" [MPa cm-3]
    phi_max = params.phi_max                            # maximal cell wall extensibility [MPa-1 day-1]
    dd_thresh = params.dd_thresh                        # dregree-days after which the cell wall extensibility decreased [dd]
    tau = params.tau                                    # rate of decrease in cell wall extensibility [°C-1 day-1]
    aLf = params.aLf                                    # produit between the ratio of the area of the vascular network to the fruit area (a) and hydraulic conductivity between the stem and the fruit (Lf)  [g cm-2 MPa-1 day-1]
    ro = params.ro                                      # fruit surface conductance [cm day-1]
    Y_0 = params.Y_0                                    # threshold pressure at full bloom [MPa-1 day-1]
    V_0 = params.V_0                                    # flesh volume at full bloom [cm3]
    s1 = params.s1                                      # specific parameter for saturation vapor pressure calculation
    s2 = params.s2                                      # specific parameter for saturation vapor pressure calculation
    delta_solutes = params.delta_solutes                # specific parameters for calculation of flesh composition in solutes
    osmotic_pressure_aa = params.osmotic_pressure_aa    # osmotic pressure in the flesh due to amino acids [Mpa]
    d_DM = params.d_DM                                  # density of dry matter [g cm3]
    d_W = params.d_W                                    # density of water [g cm3]
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

    def fn(date, T_air, GR, RH, dd_cum, T_air_daily, DM_fruit, FM_fruit_ini, W_fleshpeel_ini, DM_fruit_0):
            
        DM_fleshpeel = a5 * DM_fruit[0] ** a6 + a7 * DM_fruit[0] ** a8
        W_fleshpeel = W_fleshpeel_ini

        DM_fleshpeel_growth = a5 * a6 * DM_fruit[1] ** (a6 - 1) + a7 * a8 * DM_fruit[1] ** (a8 - 1) * (DM_fruit[1] - DM_fruit[0])
        DM_fleshpeel_growth = max(0, DM_fleshpeel_growth)

        FM_fruit = FM_fruit_ini
        FM_stone = FM_fruit - (DM_fleshpeel + W_fleshpeel)

        dd_cum = np.mean(dd_cum)

        MSpu = a9 * DM_fleshpeel
        W_flesh = a10 * W_fleshpeel

        MSpu_x_dd_cum = MSpu * dd_cum
        prop_mal = max(0, 0.06620651 + (-0.0000538797) * dd_cum + (-0.002464413) * MSpu + 2.406565e-006 * MSpu_x_dd_cum)
        prop_pyr = max(0, 0.0006896104 + 1.613387e-006 * dd_cum + 0.00005063595 * MSpu + (-6.912509e-008) * MSpu_x_dd_cum)
        prop_oxa = max(0, 0.004750718 + (-2.113094e-006) * dd_cum + (-0.00002965687) * MSpu + 0.0 * MSpu_x_dd_cum)
        prop_K = max(0, 0.01394964 + (-5.234608e-006) * dd_cum + (-0.000288464) * MSpu + 2.682089e-007 * MSpu_x_dd_cum)
        prop_Mg = max(0, 0.00115595 + (-7.937479e-007) * dd_cum + (-0.00002320017) * MSpu + (2.344528e-008) * MSpu_x_dd_cum)
        prop_Ca = max(0, 0.001588606 + (-6.625787e-007) * dd_cum + (-0.0000228527) * MSpu + (1.514343e-008) * MSpu_x_dd_cum)
        prop_NH4 = max(0, 0.000246011 + 3.741743e-007 * dd_cum + 0.00002495255 * MSpu + (-3.010081e-008) * MSpu_x_dd_cum)
        prop_Na = max(0, 0.0001279568 + 8.15203e-008 * dd_cum + (-1.468235e-006) * MSpu + 0.0 * MSpu_x_dd_cum)
        prop_glc = max(0, 0.08074145 + (-0.00006325543) * dd_cum + (-0.001161846) * MSpu + 1.161344e-006 * MSpu_x_dd_cum)
        prop_frc = max(0, 0.04972199 + 0.0000966001 * dd_cum + (-0.001078579) * MSpu + 0.0 * MSpu_x_dd_cum)
        prop_ami = max(0, -0.1708815 + 0.0004380411 * dd_cum + 0.01923022 * MSpu + (-0.00002059459) * MSpu_x_dd_cum)
        prop_cit = max(0, 0.1625024 + (-0.0000640754) * dd_cum + 0.003906348 * MSpu + (-4.784292e-006) * MSpu_x_dd_cum)
        prop_sac = max(0, 0.0 + (0.00017695) * dd_cum + (-0.007249) * MSpu + 9.03e-006 * MSpu_x_dd_cum)

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

        osmotic_pressure = R * (T_air_daily + 273.15) * c_solutes  + osmotic_pressure_aa

        A_fruit = a11 * FM_fruit ** a12
        P_sat = s1 * np.exp(s2 * T_air_daily)
        alpha = MM_water * P_sat / (83 * (T_air_daily + 273.15))
        transpiration = A_fruit * alpha * ro * (0.996 - (np.mean(RH) / 100))

        water_potential_stem = 1.0 * np.mean(a14 + (a15 * T_air) + (a16 * RH) + (a17 * GR))
        dd_thresh = a18 * DM_fruit_0 + a19

        Phi = phi_max
        if dd_cum > dd_thresh:
            Phi = phi_max * tau ** (dd_cum - dd_thresh)

        ALf = A_fruit * aLf

        Y_0 = V_0 = 0
        Y = Y_0 + h * (W_fleshpeel - V_0)

        numerator = Phi * W_fleshpeel * Y + ALf * (water_potential_stem + osmotic_pressure) - transpiration + DM_fleshpeel_growth / 1.60
        denominator = Phi * W_fleshpeel + ALf
        turgor_pressure = numerator / denominator

        if turgor_pressure < Y:
            turgor_pressure = water_potential_stem + osmotic_pressure - (transpiration - (DM_fleshpeel_growth / 1.60)) / ALf

        if turgor_pressure < Y_0:
            turgor_pressure = 0

        water_potential = turgor_pressure - osmotic_pressure
        flux_xyleme  = ALf * (water_potential_stem - water_potential)
        flux_phloeme = DM_fleshpeel_growth / 1.60

        DM_fleshpeel = DM_fleshpeel + DM_fleshpeel_growth
        W_fleshpeel = W_fleshpeel + flux_xyleme + flux_phloeme - transpiration
        FM_stone  = a13 * (DM_fleshpeel + W_fleshpeel)
        FM_fruit  = DM_fleshpeel + W_fleshpeel + FM_stone

        sucrose =  m_sa / W_flesh
        soluble_sugars = (m_sa + m_g + m_f) / W_flesh
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

    return fn
