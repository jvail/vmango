

# Type of units of the mango
eGU, eInflorescence, eMixedInflorescence, eFruit, eLeaf, eInternode = range(6)
# Type of positions of units
eLateral, eApical = 0,1
# Type of GU
eVegetative, eFlowering, eFruiting = range(3)
# Load state of a tree
eLoaded, eNotLoaded = 1, 0

eBinomial, ePoisson, eMultinomial, eGaussian = range(1,5)

eCompleteGlm, eSelectedGlm, eInteractionGlm = 1,2,3
GlmTypeName = {eCompleteGlm : 'complete_glm', eSelectedGlm : 'selected_glm', eInteractionGlm :'interaction_glm'} 


eWithinCycle, eLaterCycle = range(2)

#eTreeBased, eManagementTypeBased, eVarietyBased = range(3)
#EstimationBaseName = {eTreeBased : 'TreeBasedEstimation', eManagementTypeBased : 'ManagementTypeBasedEstimation', eVarietyBased : 'VarietyBasedEstimation' }

eMeasuredMtg, eSimulatedMtg = 1,2

eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin = range(3)
WithinDelayMethodName = {eMonthMultiVariateForWithin : 'MonthMultiVariate', eDeltaMultiVariateForWithin : 'DeltaMultiVariate', eDeltaPoissonForWithin : 'DeltaPoisson'}

eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction = 1,2,4,8,15
RestrictionName = { eBurstDateRestriction         : 'without_burst_month', 
                    ePositionARestriction         : 'without_position_a', 
                    ePositionAncestorARestriction : 'without_position_ancestor_a',                    
                    eNatureFRestriction           : 'without_nature_f_and_nature_ancestor_f',
                    eAllRestriction               : 'without_all',
                    None                          : 'allfactors'
                    }
