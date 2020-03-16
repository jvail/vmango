import numpy as np
import pandas as pd
from os.path import abspath, dirname, join

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
    idsimu=np.nan
):

    dd_delta = np.sum((T_fruit - 16) / 24)
    GR = GR / 3600 * 10000
    PAR = GR * 0.5 * 4.6

    sunlit_ws = np.array([0.88] * 24)

    p1 = 3.85
    p2 = 33.23
    p3 = 0.483
    p4 = 0.034
    k3 = 0.0529

    MRR_stem = 0.000858
    MRR_leaf = 0.000156
    MRR_fruit = 0.00115
    Q10_stem = 1.96
    Q10_leaf = 2.11
    Q10_fruit = 1.9

    r_mobile_leaf = 0.0162
    r_mobile_stem = 0.0164
    cc_stem = 0.4387
    cc_leaf = 0.4051
    cc_fruit = 0.4239
    GRC_fruit = 0.04

    RGR_fruit_ini = 0.0105
    a1 = 16.736
    a2 = 0.624
    r_storage = 0.3
    DM_fruit_max = a1 * (DM_fruit_0 ** a2)

    DM_stem = (41.83 + 77.41) / 2
    DM_leaf_unit = 0.8
    r_DM_stem_ini = 0.1
    r_DM_leaf_ini = 0.074

    DM_structure_stem = DM_stem * (1 - r_DM_stem_ini)
    DM_structure_leaf = DM_leaf_unit * (1 - r_DM_leaf_ini) * LF

    LA = 0.0051 * LF**0.937

    D_fruit =  DM_fruit_previous * RGR_fruit_ini * dd_delta * (1 - (DM_fruit_previous / DM_fruit_max)) * (cc_fruit + GRC_fruit)

    Pmax = (p1 * (D_fruit / LA) * p2) / (p1 * (D_fruit / LA) + p2)
    Pmax = min(Pmax.min(), 15)

    LA_sunlit = sunlit_bs[PAR>0] * sunlit_ws[PAR>0] * LA
    LA_shaded = LA - LA_sunlit

    PAR_shaded = k3 * PAR
    P_shaded = ((Pmax + p3) * (1 - np.exp(- p4 * PAR_shaded[PAR_shaded > 0] / (Pmax + p3)))) - p3
    photo_shaded = 3600 * sum (P_shaded[P_shaded > 0] *  LA_shaded[P_shaded > 0]) * 12 / 10 ** 6

    P_sunlit = ((Pmax + p3) * (1 - np.exp(-p4 * PAR[PAR > 0] / (Pmax + p3)))) - p3
    photo_sunlit = 3600 * sum(P_sunlit[P_sunlit > 0] * LA_sunlit[P_sunlit > 0]) * 12 / 10 ** 6

    photo = photo_shaded + photo_sunlit

    reserve_m = (reserve_leaf * r_mobile_leaf) + (reserve_stem * r_mobile_stem)

    assimilats = photo + reserve_m

    reserve_nm_leaf = reserve_leaf * (1 - r_mobile_leaf)
    reserve_nm_stem = reserve_stem * (1- r_mobile_stem)

    MR_stem = MRR_stem / 24 * (Q10_stem ** ((T_air - 20) / 10)) * (DM_structure_stem + (reserve_stem / cc_stem))

    MR_fruit = MRR_fruit / 24 * (Q10_fruit ** ((T_fruit -20)/10)) * DM_fruit_previous

    MR_leaf = MRR_leaf * (Q10_leaf ** ((T_air -20)/10)) * (DM_structure_leaf + reserve_leaf / cc_leaf)
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
