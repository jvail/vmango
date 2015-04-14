

eLateral, eApical = 0,1
eVegetative, eInflorescence, eFruit = 1,0,-1
eLoaded, eNotLoaded = 1, 0

eCompleteGlm, eSelectedGlm, eNullGlm = 1,2,3
GlmTypeName= {eCompleteGlm : 'complete_glm', eSelectedGlm : 'selected_glm', eNullGlm : 'null_glm'} 

eBinomial, ePoisson, eVglm = range(1,4)

eWithinCycle, eLaterCycle = range(2)

eTreeBased, eManagementTypeBased, eVarietyBased = range(3)
EstimationBaseName = {eTreeBased : 'TreeBasedEstimation', eManagementTypeBased : 'ManagementTypeBasedEstimation', eVarietyBased : 'VarietyBasedEstimation' }

eMeasuredMtg, eSimulatedMtg = 1,2

eMonthMultiVariateForWithin, eDeltaMultiVariateForWithin, eDeltaPoissonForWithin = range(3)
WithinDelayMethodName = {eMonthMultiVariateForWithin : 'MonthMultiVariate', eDeltaMultiVariateForWithin : 'DeltaMultiVariate', eDeltaPoissonForWithin : 'DeltaPoisson'}