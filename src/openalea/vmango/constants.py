

# Type of units of the mango
from builtins import range
eGU, eInflorescence, eMixedInflorescence, eFruit, eLeaf, eInternode = list(range(6))
# Type of positions of units
eLateral, eApical = 0,1
# Type of GU
eVegetative, eFlowering, eFruiting = list(range(3))
# Load state of a tree
eLoaded, eNotLoaded = 1, 0

eBinomial, ePoisson, eMultinomial, eGaussian = list(range(1,5))

eCompleteGlm, eSelectedGlm, eInteractionGlm = 1,2,3
GlmTypeName = {eCompleteGlm : 'complete_glm', eSelectedGlm : 'selected_glm', eInteractionGlm :'interaction_glm'}


eWithinCycle, eLaterCycle = list(range(2))

#eTreeBased, eManagementTypeBased, eVarietyBased = range(3)
#EstimationBaseName = {eTreeBased : 'TreeBasedEstimation', eManagementTypeBased : 'ManagementTypeBasedEstimation', eVarietyBased : 'VarietyBasedEstimation' }

eMeasuredMtg, eSimulatedMtg = 1,2

eMonthMultinomialForWithin, eDeltaMultinomialForWithin, eDeltaPoissonForWithin = list(range(3))
WithinDelayMethodName = {eMonthMultinomialForWithin : 'MonthMultinomial', eDeltaMultinomialForWithin : 'DeltaMultinomial', eDeltaPoissonForWithin : 'DeltaPoisson'}

eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction = 1,2,4,8
eBurstDateOnlyRestriction, ePositionAOnlyRestriction, ePositionAncestorAOnlyRestriction, eNatureFOnlyRestriction = 15 - eBurstDateRestriction, 15 -  ePositionARestriction, 15 - ePositionAncestorARestriction, 15 -  eNatureFRestriction
eAllRestriction, eNoRestriction = 15,0

RestrictionName = { eBurstDateRestriction         : 'without_burst_month_and_flowering_week',
                    ePositionARestriction         : 'without_position_a',
                    ePositionAncestorARestriction : 'without_position_ancestor_a',
                    eNatureFRestriction           : 'without_nature_f_and_nature_ancestor_f',
                    eBurstDateOnlyRestriction         : 'with_only_burst_month_and_flowering_week',
                    ePositionAOnlyRestriction         : 'with_only_position_a',
                    ePositionAncestorAOnlyRestriction : 'with_only_position_ancestor_a',
                    eNatureFOnlyRestriction           : 'with_only_nature_f_and_nature_ancestor_f',
                    eAllRestriction               : 'without_all',
                    eNoRestriction                : 'allfactors',
                    None                          : 'allfactors'
                    }
