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


growth_FM <- function( date,
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
                       dd_thresh
)

{

  ## -- physical constants -------------------------------------------------------------------------------------------------

  R <- 8.3                             # gaz constant [cm3 MPa mol-1 K-1]
  density_W <- 1                       # water density [g cm-3]
  MM_water <- 18                       # molecular mass of water [g mol-1]
  MM_mal <- 134                        # molecular mass of malic acid [g mol-1]
  MM_cit <- 192                        # molecular mass of citric acid [g mol-1]
  MM_pyr <- 88                         # molecular mass of pyruvic acid [g mol-1]
  MM_oxa <- 90                         # molecular mass of oxalic acid [g mol-1]
  MM_K <- 39                           # molecular mass of K+ [g mol-1]
  MM_Mg <- 24                          # molecular mass of Mg2+ [g mol-1]
  MM_Ca <- 40                          # molecular mass of Ca2+ [g mol-1]
  MM_NH4 <- 18                         # molecular mass of NH4+ [g mol-1]
  MM_Na <- 23                          # molecular mass of Na+ [g mol-1]
  MM_glc <- 180                        # molecular mass of glucose [g mol-1]
  MM_frc <- 180                        # molecular mass of fructose [g mol-1]
  MM_suc <- 342                        # molecular mass of sucrose [g mol-1]


  # ========================================================================================================================
  # DRY MASS AND GROWTH RATE OF FRUIT FLESH
  # ========================================================================================================================
  ## from empirical relationships in Léchaudel (2004)

  DM_fleshpeel_previous <- (e_fruit2fleshDM_1 * (DM_fruit_previous) ^ e_fruit2fleshDM_2) + 
                           (e_fruit2peelDM_1  * (DM_fruit_previous) ^ e_fruit2peelDM_2)
  DM_fleshpeel_delta <- ( e_fruit2fleshDM_1 * e_fruit2fleshDM_2 * (DM_fruit) ^ (e_fruit2fleshDM_2 - 1) + 
                          e_fruit2peelDM_1  * e_fruit2peelDM_2  * (DM_fruit) ^ (e_fruit2peelDM_2 - 1) ) * (DM_fruit - DM_fruit_previous)
  DM_fleshpeel_delta <- max(DM_fleshpeel_delta, 0)
  DM_flesh_previous <- e_fleshpeel2fleshDM * DM_fleshpeel_previous
  W_flesh_previous <- e_fleshpeel2fleshW * W_fleshpeel_previous

  # ========================================================================================================================
  # OSMOTIC PRESSURE IN THE FRUIT
  # ========================================================================================================================

  ## -- mass proportion of osmotically active solutes & starch in the dry mass of fruit flesh (eq.9) :
  prop_mal <- delta_mal[1] + delta_mal[2] * dd_cum + delta_mal[3] * DM_flesh_previous + delta_mal[4] * DM_flesh_previous * dd_cum
  prop_cit <- delta_cit[1] + delta_cit[2] * dd_cum + delta_cit[3] * DM_flesh_previous + delta_cit[4] * DM_flesh_previous * dd_cum
  prop_pyr <- delta_pyr[1] + delta_pyr[2] * dd_cum + delta_pyr[3] * DM_flesh_previous + delta_pyr[4] * DM_flesh_previous * dd_cum
  prop_oxa <- delta_oxa[1] + delta_oxa[2] * dd_cum + delta_oxa[3] * DM_flesh_previous + delta_oxa[4] * DM_flesh_previous * dd_cum
  prop_K   <- delta_K[1]   + delta_K[2]   * dd_cum + delta_K[3]   * DM_flesh_previous + delta_K[4]   * DM_flesh_previous * dd_cum
  prop_Mg  <- delta_Mg[1]  + delta_Mg[2]  * dd_cum + delta_Mg[3]  * DM_flesh_previous + delta_Mg[4]  * DM_flesh_previous * dd_cum
  prop_Ca  <- delta_Ca[1]  + delta_Ca[2]  * dd_cum + delta_Ca[3]  * DM_flesh_previous + delta_Ca[4]  * DM_flesh_previous * dd_cum
  prop_NH4 <- delta_NH4[1] + delta_NH4[2] * dd_cum + delta_NH4[3] * DM_flesh_previous + delta_NH4[4] * DM_flesh_previous * dd_cum
  prop_Na  <- delta_Na[1]  + delta_Na[2]  * dd_cum + delta_Na[3]  * DM_flesh_previous + delta_Na[4]  * DM_flesh_previous * dd_cum
  prop_glc <- delta_glc[1] + delta_glc[2] * dd_cum + delta_glc[3] * DM_flesh_previous + delta_glc[4] * DM_flesh_previous * dd_cum
  prop_frc <- delta_frc[1] + delta_frc[2] * dd_cum + delta_frc[3] * DM_flesh_previous + delta_frc[4] * DM_flesh_previous * dd_cum
  prop_suc <- delta_suc[1] + delta_suc[2] * dd_cum + delta_suc[3] * DM_flesh_previous + delta_suc[4] * DM_flesh_previous * dd_cum
  prop_sta <- delta_sta[1] + delta_sta[2] * dd_cum + delta_sta[3] * DM_flesh_previous + delta_sta[4] * DM_flesh_previous * dd_cum
  if (prop_mal < 0) { prop_mal <- 0 }
  if (prop_cit < 0) { prop_cit <- 0 }
  if (prop_pyr < 0) { prop_pyr <- 0 }
  if (prop_oxa < 0) { prop_oxa <- 0 }
  if (prop_K < 0)   { prop_K <- 0 }
  if (prop_Mg < 0)  { prop_Mg <- 0 }
  if (prop_Ca < 0)  { prop_Ca <- 0 }
  if (prop_NH4 < 0) { prop_NH4 <- 0 }
  if (prop_Na < 0)  { prop_Na <- 0 }
  if (prop_glc < 0) { prop_glc <- 0 }
  if (prop_frc < 0) { prop_frc <- 0 }
  if (prop_suc < 0) { prop_suc <- 0 }
  if (prop_sta < 0) { prop_sta <- 0 }

  ## -- mass and number of moles of osmotically active solutes & starch in fruit flesh (eq.8) :
  mass_mal <- prop_mal * DM_flesh_previous ;      nmol_mal <- mass_mal / MM_mal
  mass_cit <- prop_cit * DM_flesh_previous ;      nmol_cit <- mass_cit / MM_cit
  mass_pyr <- prop_pyr * DM_flesh_previous ;      nmol_pyr <- mass_pyr / MM_pyr
  mass_oxa <- prop_oxa * DM_flesh_previous ;      nmol_oxa <- mass_oxa / MM_oxa
  mass_K   <- prop_K   * DM_flesh_previous ;      nmol_K   <- mass_K   / MM_K
  mass_Mg  <- prop_Mg  * DM_flesh_previous ;      nmol_Mg  <- mass_Mg  / MM_Mg
  mass_Ca  <- prop_Ca  * DM_flesh_previous ;      nmol_Ca  <- mass_Ca  / MM_Ca
  mass_NH4 <- prop_NH4 * DM_flesh_previous ;      nmol_NH4 <- mass_NH4 / MM_NH4
  mass_Na  <- prop_Na  * DM_flesh_previous ;      nmol_Na  <- mass_Na  / MM_Na
  mass_glc <- prop_glc * DM_flesh_previous ;      nmol_glc <- mass_glc / MM_glc
  mass_frc <- prop_frc * DM_flesh_previous ;      nmol_frc <- mass_frc / MM_frc
  mass_suc <- prop_suc * DM_flesh_previous ;      nmol_suc <- mass_suc / MM_suc
  mass_sta <- prop_sta * DM_flesh_previous

  ## -- osmotic pressure in fruit flesh (eq.6-7) :
  nmol_solutes <- nmol_mal + nmol_cit + nmol_pyr + nmol_oxa + nmol_K + nmol_Mg + nmol_Ca + nmol_NH4 + nmol_Na + nmol_glc + nmol_frc + nmol_suc
  osmotic_pressure_fruit <- (R * (TM_air + 273.15) * nmol_solutes) / (W_flesh_previous / density_W) + osmotic_pressure_aa


  # ========================================================================================================================
  # FRUIT TRANSPIRATION
  # ========================================================================================================================

  ## -- fruit surface (eq.3) :
  A_fruit <- e_fruitFM2surface_1 * FM_fruit_previous ^ e_fruitFM2surface_2

  ## -- saturation vapor pressure (eq.3 in Fishman and Génard 1998) :
  ##    converted from bar to MPa
  P_sat <- psat_1 * exp(psat_2 * TM_air) / 10

  ## -- fruit transpiration_fruit (eq.2) :
  alpha <- MM_water * P_sat / (R * (TM_air + 273.15))
  transpiration_fruit <- A_fruit * alpha * ro * (RH_fruit - mean(RH)/100)


  # ========================================================================================================================
  # CELL WALL PROPERTIES OF THE FRUIT
  # ========================================================================================================================

  ## -- cell wall extensibility (eq.18) :
  if (is.nan(dd_thresh)) {                                                                                                          # _MODIF 2017-05_1
    ## if not fixed as input, set from an empirical relationship
    dd_thresh <- ddthres_1 * DM_fruit_0 + ddthres_2
  }

  if (dd_cum > dd_thresh) {
    Phi <- phi_max * tau ^ (dd_cum - dd_thresh)
  }
  else {
    Phi <- phi_max
  }

  # -- threshold pressure (eq.15-16) :
  V <- W_fleshpeel_previous / density_W + DM_fleshpeel_previous / density_DM
  Y <- Y_0 + h * (V - V_0)


  # ========================================================================================================================
  # TURGOR PRESSURE & WATER POTENTIAL IN THE FRUIT
  # ========================================================================================================================

  ## -- water potential of the stem :
  water_potential_stem <- swp_1 + swp_2 * TM_air + swp_3 * mean(RH) + swp_4 * mean(GR)

  ## -- turgor pressure in the fruit (defined by combining eq.11 and eq.13) :
  ALf <- A_fruit * aLf
  numerator <- Phi * V * Y  +  ALf * (water_potential_stem + osmotic_pressure_fruit) / density_W - transpiration_fruit / density_W + 
               DM_fleshpeel_delta / density_DM
  denominator <- Phi * V + ALf / density_W

  turgor_pressure_fruit <- numerator / denominator
  if (turgor_pressure_fruit < Y) {
    turgor_pressure_fruit <- water_potential_stem + osmotic_pressure_fruit - (transpiration_fruit - DM_fleshpeel_delta * density_W / density_DM) / ALf
  }
  turgor_pressure_fruit <- max(turgor_pressure_fruit, Y_0)

  ## -- water potential in the fruit (eq.5) :
  water_potential_fruit <- turgor_pressure_fruit - osmotic_pressure_fruit


  # ========================================================================================================================
  # WATER AND DRY MATTER CHANGES IN FRUIT COMPARTMENTS
  # ========================================================================================================================

  ## -- rate of water inflow in the fruit from xylem and phloem (eq.4) :
  flux_xylem_phloem <- ALf * (water_potential_stem - water_potential_fruit)

  ## -- changes in dry mass, fresh mass and water mass of fruit compartments :
  DM_fleshpeel <- DM_fleshpeel_previous + DM_fleshpeel_delta
  W_fleshpeel <- W_fleshpeel_previous + flux_xylem_phloem - transpiration_fruit
  FM_stone <- e_flesh2stoneFM * (DM_fleshpeel + W_fleshpeel)
  FM_fruit <- (DM_fleshpeel + W_fleshpeel) + FM_stone
  DM_flesh <- DM_fleshpeel * e_fleshpeel2fleshDM
  W_flesh <- W_fleshpeel * e_fleshpeel2fleshW


  # ========================================================================================================================
  # OUTPUTS
  # ========================================================================================================================

  Results_day <- data.frame( water_potential_fruit = water_potential_fruit,
                             turgor_pressure_fruit = turgor_pressure_fruit,
                             osmotic_pressure_fruit =  osmotic_pressure_fruit,
                             flux_xylem_phloem = flux_xylem_phloem,
                             transpiration_fruit = transpiration_fruit,
                             sucrose = mass_suc / (W_flesh_previous + DM_flesh_previous),
                             glucose = mass_glc / (W_flesh_previous + DM_flesh_previous),
                             fructose = mass_frc / (W_flesh_previous + DM_flesh_previous),
                             soluble_sugars = (mass_suc + mass_glc + mass_frc) / (W_flesh_previous + DM_flesh_previous),
                             starch = mass_sta /(W_flesh_previous + DM_flesh_previous),
                             organic_acids = (mass_mal + mass_cit) / (W_flesh_previous + DM_flesh_previous)
  )

  Results_day_next <- data.frame( date = date + 1,
                                  FM_fruit = FM_fruit,
                                  DM_Fruit = DM_fruit,
                                  W_fleshpeel = W_fleshpeel,
                                  DM_fleshpeel = DM_fleshpeel,
                                  W_flesh = W_flesh,
                                  DM_flesh = DM_flesh
  )

  Results <- list( Results_day = Results_day,
                   Results_day_next = Results_day_next)

  return(Results)

}
