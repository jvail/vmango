from vplants.mangosim.state import *

vegetative_proba = ['vegetative_burst','has_apical_gu_child','has_lateral_gu_children','nb_lateral_gu_children']
vegetative_proba_family = [eBinomial, eBinomial, eBinomial, ePoisson]
vegetative_proba_within = ['burst_date_children','burst_delta_date_children','burst_delta_date_children_poisson']
vegetative_proba_within_family = [eMultiVariate, eMultiVariate, ePoisson]
vegetative_proba_between = ['burst_date_children']
vegetative_proba_between_family = [eMultiVariate]

flowering_proba  = ['flowering','nb_inflorescences','flowering_week']
flowering_proba_family  = [eBinomial, ePoisson, eMultiVariate ]
fruiting_proba  = ['fruiting','nb_fruits','fruit_weight']
fruiting_proba_family  = [eBinomial, ePoisson, eGaussian ]


within_extension = {3 : None, 4 : 'within_04', 5 : 'within_05'}
between_extension = {3 : 'between_03to0405', 4 : 'between_04to05', 5 : None }

allfactors = ['Tree_Fruit_Load', 'Burst_Month', 'Burst_Semester', 'Position_A', 'Position_Ancestor_A', 'Nature_Ancestor_F', 'Nature_F', 'Reiteration', 'Mixed_Inflo']
factorsvalues = [range(0,2), range(6,13)+range(6), [1,2], [eApical,eLateral], [eApical,eLateral], [eVegetative, eFlowering, eFruiting], [eVegetative, eFlowering], [0,1], [0,1]]
factorsvalues = dict(zip(allfactors,factorsvalues))