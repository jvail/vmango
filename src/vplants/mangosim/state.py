

# Type of units of the mango
eGU, eInflorescence, eFruit, eLeaf, eInternode = range(5)
# Type of positions of units
eLateral, eApical = 0,1
# Type of GU
eVegetative, eFlowering, eFruiting = range(3)
# Load state of a tree
eLoaded, eNotLoaded = 1, 0

eBinomial, ePoisson, eMultiVariate = range(1,4)

eCompleteGlm, eSelectedGlm, eNullGlm = 1,2,3
GlmTypeName = {eCompleteGlm : 'complete_glm', eSelectedGlm : 'selected_glm', eNullGlm : 'null_glm'} 


eWithinCycle, eLaterCycle = range(2)

eTreeBased, eManagementTypeBased, eVarietyBased = range(3)
EstimationBaseName = {eTreeBased : 'TreeBasedEstimation', eManagementTypeBased : 'ManagementTypeBasedEstimation', eVarietyBased : 'VarietyBasedEstimation' }

eMeasuredMtg, eSimulatedMtg = 1,2

eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin = range(3)
WithinDelayMethodName = {eMonthMultiVariateForWithin : 'MonthMultiVariate', eDeltaMultiVariateForWithin : 'DeltaMultiVariate', eDeltaPoissonForWithin : 'DeltaPoisson'}

eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction = 1,2,4,8
RestrictionName = { eBurstDateRestriction         : 'without_burst_date', 
                    ePositionARestriction         : 'without_position_a', 
                    ePositionAncestorARestriction : 'without_ancestor_position_a',                    
                    eNatureFRestriction           : 'without_nature_f'
                    }
