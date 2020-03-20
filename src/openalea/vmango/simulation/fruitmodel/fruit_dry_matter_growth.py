import numpy as np

from openalea.vmango.simulation.fruitmodel.fruitmodel_error import FruitModelValueError

def growth_DM(
    GR,
    T_air,
    T_fruit,
    sunlit_bs,
    DM_fruit_0,
    DM_fruit_previous,
    reserve_stem,
    reserve_leaf,
    LF,
    params
):

    k = params.k                            # conversion factor of leaf photosynthesis (from µmol CO2 s-1 to g C h-1) [g C s h-1 µmol-1 CO2]
    k1 = params.k1                          # fraction of global radiation photosynthetically active [dimensionless]
    k2 = params.k2                          # conversion factor of global radiation (from W m-2 to µmol photon m-2 s-1) [µmol photon W-1 s-1]
    k3 = params.k3                          # fraction of radiation received by shaded leaves [dimensionless]
    Pmax_MAX = params.Pmax_MAX              # maximal threshold of light-saturated leaf photosynthesis [µmol CO2 m-2 s-1]
    Pmax_MIN = params.Pmax_MIN              # minimal threshold of light-saturated leaf photosynthesis [µmol CO2 m-2 s-1]
    p1 = params.p1                          # specific parameter for photosynthesis calculation [µmol CO2 day s-1 g-1 C]
    p2 = params.p2                          # specific parameter for photosynthesis calculation [µmol CO2 m-2 s-1]
    p3 = params.p3                          # specific parameter for photosynthesis calculation [µmol CO2 m-2 s-1]
    p4 = params.p4                          # specific parameter for photosynthesis calculation [µmol CO2 µmol-1 photon]
    MRR_stem = params.MRR_stem              # maintenance respiration rate of stem [g C g-1 day-1]
    MRR_leaf = params.MRR_leaf              # maintenance respiration rate of leaf [g C g-1 h-1]
    MRR_fruit = params.MRR_fruit            # maintenance respiration rate of fruit [g C g-1 day-1]
    Q10_stem = params.Q10_stem              # Q10 value for stem [dimensionless]
    Q10_leaf = params.Q10_leaf              # Q10 value for leaf [dimensionless]
    Q10_fruit = params.Q10_fruit            # Q10 value for fruit [dimensionless]
    Tref = params.Tref                      # reference temperature for maintenance respiration calculation based on Q10 concept [°C]
    cc_stem = params.cc_stem                # carbon content of stem dry matter [g C g-1 DM]
    cc_leaf = params.cc_leaf                # carbon content of leaf dry matter [g C g-1 DM]
    cc_fruit = params.cc_fruit              # carbon content of fruit dry matter [g C g-1 DM]
    GRC_fruit = params.GRC_fruit            # growth respiration coefficient of fruit [g C g-1 DM]
    RGR_fruit_ini = params.RGR_fruit_ini    # initial relative growth rate of fruit [dd-1]
    r_mobile_stem = params.r_mobile_stem    # mobile fraction of carbon reserves in stem [dimensionless]
    r_mobile_leaf = params.r_mobile_leaf    # mobile fraction of carbon reserves in leaf [dimensionless]
    r_DM_stem_ini = params.r_DM_stem_ini    # initial dry mass fraction of reserves in stem [dimensionless]
    r_DM_leaf_ini = params.r_DM_leaf_ini    # initial dry mass fraction of reserves in leaf [dimensionless]
    r_storage = params.r_storage            # specific parameter for leaf reserve storage calculation [dimensionless]
    a1 = params.a1                          # specific parameter for maximum fruit dry mass calculation [g1-0.624]
    a2 = params.a2                          # specific parameter for maximum fruit dry mass calculation [dimensionless]
    a3 = params.a3                          # specific parameter for leaf area calculation [m2-0.937]
    a4 = params.a4                          # specific parameter for leaf area calculation [dimensionless]
    DM_stem = params.DM_stem                # stem dry mass [g DM]
    DM_leaf_unit = params.DM_leaf_unit      # leaf dry mass (for a single leaf) [g DM]


    dd_delta = np.sum((T_fruit - 16) / 24)
    GR = GR / 3600 * 10000
    PAR = GR * k1 * k2

    sunlit_ws = np.array([0.88] * 24)

    DM_fruit_max = a1 * (DM_fruit_0 ** a2)

    DM_structure_stem = DM_stem * (1 - r_DM_stem_ini)
    DM_structure_leaf = DM_leaf_unit * (1 - r_DM_leaf_ini) * LF

    LA = a3 * LF ** a4 

    D_fruit =  DM_fruit_previous * RGR_fruit_ini * dd_delta * (1 - (DM_fruit_previous / DM_fruit_max)) * (cc_fruit + GRC_fruit)

    Pmax = (p1 * (D_fruit / LA) * p2) / (p1 * (D_fruit / LA) + p2)
    Pmax = min(Pmax.min(), Pmax_MAX)

    LA_sunlit = sunlit_bs[PAR>0] * sunlit_ws[PAR>0] * LA
    LA_shaded = LA - LA_sunlit

    PAR_shaded = k3 * PAR
    P_shaded = ((Pmax + p3) * (1 - np.exp(- p4 * PAR_shaded[PAR_shaded > 0] / (Pmax + p3)))) - p3
    photo_shaded = sum(P_shaded[P_shaded > 0] *  LA_shaded[P_shaded > 0]) * k

    P_sunlit = ((Pmax + p3) * (1 - np.exp(-p4 * PAR[PAR > 0] / (Pmax + p3)))) - p3
    photo_sunlit = sum(P_sunlit[P_sunlit > 0] * LA_sunlit[P_sunlit > 0]) * k

    photo = photo_shaded + photo_sunlit

    reserve_m = (reserve_leaf * r_mobile_leaf) + (reserve_stem * r_mobile_stem)

    assimilats = photo + reserve_m

    reserve_nm_leaf = reserve_leaf * (1 - r_mobile_leaf)
    reserve_nm_stem = reserve_stem * (1- r_mobile_stem)

    MR_stem = MRR_stem / 24 * (Q10_stem ** ((T_air - Tref) / 10)) * (DM_structure_stem + (reserve_stem / cc_stem))

    MR_fruit = MRR_fruit / 24 * (Q10_fruit ** ((T_fruit - Tref) / 10)) * DM_fruit_previous

    MR_leaf = MRR_leaf * (Q10_leaf ** ((T_air - Tref) / 10)) * (DM_structure_leaf + reserve_leaf / cc_leaf)
    MR_leaf = sum(MR_leaf[PAR == 0])
    MR_fruit = sum(MR_fruit)
    MR_stem = sum(MR_stem)

    MR_repo = MR_fruit
    MR_veget = MR_stem + MR_leaf

    if assimilats >= MR_veget:
        remains_1 = assimilats - MR_veget
    else:
        if assimilats + reserve_nm_leaf >= MR_veget:
            remains_1 = 0
            reserve_nm_leaf = assimilats + reserve_nm_leaf - MR_veget
        else:
            if assimilats + reserve_nm_leaf + reserve_nm_stem >= MR_veget:
                remains_1 = 0
                reserve_nm_stem = assimilats + reserve_nm_leaf + reserve_nm_stem - MR_veget
                reserve_nm_leaf = 0
            else:
                remains_1 = 0
                raise FruitModelValueError(f'Les parties vegetatives s\'etouffent: le systeme meurt ...')

    if remains_1 < MR_repo:
        besoin_fruit = (MR_fruit - remains_1) / cc_fruit
        if besoin_fruit >= DM_fruit_previous:
            raise FruitModelValueError(f'Les parties reproductrices s\'etouffent: le systeme meurt ...')
        else:
            DM_fruit_previous = DM_fruit_previous - besoin_fruit

    remains_2 = max(0, remains_1 - MR_repo)

    DM_fruit  = DM_fruit_previous + (min(D_fruit, remains_2) / (cc_fruit + GRC_fruit))

    remains_3 = remains_2 - min(D_fruit, remains_2)

    reserve_stem_provi = reserve_nm_stem + min(remains_3, reserve_stem * r_mobile_stem)
    reserve_leaf_provi = reserve_nm_leaf + max(0, remains_3 - reserve_stem * r_mobile_stem)

    reserve_MAX = (r_storage / (1 - r_storage)) * DM_structure_leaf * cc_leaf

    if reserve_leaf_provi > reserve_MAX:
        reserve_leaf = reserve_MAX
        reserve_stem  = reserve_leaf_provi - reserve_MAX + reserve_stem_provi
    else:
        reserve_leaf = reserve_leaf_provi
        reserve_stem = reserve_stem_provi

    return (DM_fruit, (reserve_leaf, reserve_stem))
