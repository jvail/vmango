from vplants.mangosim.state import *

vegetative_proba = ['vegetative_burst','has_apical_gu_child','has_lateral_gu_children','nb_lateral_gu_children']
vegetative_proba_family = [eBinomial, eBinomial, eBinomial, ePoisson]
vegetative_proba_within = ['burst_date_gu_children','burst_delta_date_gu_children','burst_delta_date_gu_children_poisson']
vegetative_proba_within_family = [eMultinomial, eMultinomial, ePoisson]
vegetative_proba_between = ['burst_date_gu_children']
vegetative_proba_between_family = [eMultinomial]

flowering_proba  = ['flowering','nb_inflorescences','flowering_week']
flowering_proba_family  = [eBinomial, ePoisson, eMultinomial ]

fruiting_proba  = ['fruiting','nb_fruits','fruit_weight']
fruiting_proba_family  = [eBinomial, ePoisson, eGaussian ]

mixedinflo_proba = ['mixedinflo_burst','burst_date_mi_children','burst_delta_date_mi_children','burst_delta_date_mi_children_poisson']
mixedinflo_proba_family = [eBinomial, eMultinomial, eMultinomial, ePoisson]
# Always one apical mixed inflo.

def appendprefix(prefix, names): return [prefix+name for name in names]

gu_vegetative_proba = appendprefix('gu_',vegetative_proba)
gu_vegetative_proba_family = vegetative_proba_family
gu_vegetative_proba_within = appendprefix('gu_',vegetative_proba_within)
gu_vegetative_proba_within_family = vegetative_proba_within_family
gu_vegetative_proba_between = appendprefix('gu_',vegetative_proba_between)
gu_vegetative_proba_between_family = vegetative_proba_between_family

gu_flowering_proba  = appendprefix('gu_',flowering_proba)
gu_flowering_proba_family  = flowering_proba_family
gu_fruiting_proba  =  appendprefix('gu_',fruiting_proba)
gu_fruiting_proba_family  = fruiting_proba_family

gu_mixedinflo_proba = appendprefix('gu_',mixedinflo_proba)
gu_mixedinflo_proba_family = mixedinflo_proba_family

mi_vegetative_proba = appendprefix('mi_',vegetative_proba)
mi_vegetative_proba_family = vegetative_proba_family
mi_vegetative_proba_within = appendprefix('mi_',vegetative_proba_between)
mi_vegetative_proba_within_family = vegetative_proba_between_family



within_extension = {3 : None, 4 : 'within_04', 5 : 'within_05'}
between_extension = {3 : 'between_03to0405', 4 : 'between_04to05', 5 : None }

allfactors = ['Burst_Month', 'Position_A', 'Position_Ancestor_A', 'Nature_Ancestor_F', 'Nature_F', 'Cycle', 'Flowering_Week', 'Has_Apical_GU_Child']
factorsvalues = [range(6,13)+range(6), [eApical,eLateral], [eApical,eLateral], [eVegetative, eFlowering, eFruiting], [eVegetative, eFlowering], [3,4,5], range(0,13), [0,1]]
factorsvalues = dict(zip(allfactors,factorsvalues))