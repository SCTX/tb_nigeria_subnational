
# # formulae lists

formulae_list1 <- list(
    
    # # top-ranking model with fixed effects
    # # and basic models with spatial smoothing
    
    # # country fixed int
    f_int = response ~ 1
    ,
    
    # # country fixed int with smoothing
    f_icar = response ~ 1 +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE)
    ,
    
    # # country fixed int with smoothing, LGA random int
    f_bym  = response ~ 1 +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # country fixed int with smoothing, zone fixed int, LGA random int
    f_zone_bym = response ~ 1 +
        zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC, all fixed
    # # raw_prev_rate ~ age_1524_pop * zone + mv149_lessThanSecondaryEduc_2013 * zone + assets_fn12 * zone
    f_covar_zone = response ~ 1 +
        zone +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone
    ,
    
    # # 1st best by CC with smoothing and LGA random int
    f_covar_zone_bym = response ~ 1 +
        zone +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing and zone random int
    f_covar_rzone_icar = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) # +
    # f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing and LGA and zone random int
    f_covar_rzone_bym = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best model by RSE, all fixed
    # # raw_prev_rate ~ assets_fn13 + assets_fn12 * zone + assets_fn1 * high_hiv
    f_covar2_zone = response ~ 1 +
        zone +
        high_hiv +
        assets_fn13 +
        assets_fn12 + assets_fn12:zone +
        assets_fn1 + assets_fn1:high_hiv
    ,
    
    # # 11th best model by RSE, all fixed
    # # raw_prev_rate ~ assets_fn8 + v445_underweight_2013 * zone + assets_fn12 * zone
    f_covar3_zone = response ~ 1 +
        zone +
        assets_fn8 +
        v445_underweight_2013 + v445_underweight_2013:zone +
        assets_fn12 + assets_fn12:zone
    
)

formulae_list2 <- list(
    
    # # other top-ranking models with fixed effects
    
    # # country fixed with zone fixed int
    f_zone = response ~ 1 +
        zone
    ,
    
    # # 2nd best by CC, all fixed
    # # raw_prev_rate ~ v394_notVisitedHF_2013 * zone + assets_fn12 * zone + assets_fn8 * urban
    f_covar4_zone = response ~ 1 +
        zone +
        urban +
        v394_notVisitedHF_2013 + v394_notVisitedHF_2013:zone +
        assets_fn12 + assets_fn12:zone +
        assets_fn8 + assets_fn8:urban
    ,
    
    # # 3rd best by CC, all fixed
    # # raw_prev_rate ~ age_1524_pop:zone + assets_fn12:zone + assets_fn8:zone
    f_covar5_zone = response ~ 1 +
        age_1524_pop:zone +
        assets_fn12:zone +
        assets_fn8:zone
    ,
    
    # # 2nd best by RSE, all fixed
    # # raw_prev_rate ~ assets_fn12 * zone + age_0004_pop * urban + age_1524_pop * urban
    f_covar6_zone = response ~ 1 +
        zone +
        urban +
        assets_fn12 + assets_fn12:zone +
        age_0004_pop + age_0004_pop:urban +
        age_1524_pop + age_1524_pop:urban
    ,
    
    # # 3rd best by RSE, all fixed
    # # raw_prev_rate ~ assets_fn12 * zone + age_1524_pop * urban + assets_fn8 * urban
    f_covar7_zone = response ~ 1 +
        zone +
        urban +
        assets_fn12 + assets_fn12:zone +
        age_1524_pop + age_1524_pop:urban +
        assets_fn8 + assets_fn8:urban
)

formulae_list3 <- list(
    
    # # with or without zonal random intercepts with random slopes
        
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone1a_bym = response ~ 1 +
        zone +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone1b_bym = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone2a_bym = response ~ 1 +
        zone +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + f(zone3, mv149_lessThanSecondaryEduc_2013, model = "iid") +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,    
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone2b_bym = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + f(zone3, mv149_lessThanSecondaryEduc_2013, model = "iid") +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone3a_bym = response ~ 1 +
        zone +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + f(zone3, mv149_lessThanSecondaryEduc_2013, model = "iid") +
        assets_fn12 + f(zone4, assets_fn12, model = "iid") +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,    
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone3b_bym = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + f(zone3, mv149_lessThanSecondaryEduc_2013, model = "iid") +
        assets_fn12 + f(zone4, assets_fn12, model = "iid") +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone4a_bym = response ~ 1 +
        zone +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + f(zone4, assets_fn12, model = "iid") +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,    
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone4b_bym = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + f(zone4, assets_fn12, model = "iid") +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    
)

formulae_list4 <- list(
    
    # # with or without zonal random intercepts with random slopes
    # # without the lowest level cluster random intercepts
    
    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone5a_icar = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + f(zone2, age_1524_pop, model = "iid") +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE)
    ,

    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone5b_icar = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + f(zone2, mv149_lessThanSecondaryEduc_2013, model = "iid") +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE)
    ,

    # # 1st best by CC with smoothing, LGA random int, zone random slope
    f_covar_rSlopeZone5c_icar = response ~ 1 +
        f(zone, model = "iid") +
        age_1524_pop + age_1524_pop:zone2 +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + f(zone2, assets_fn12, model = "iid") +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE)

)

formulae_list5 <- list(
    
    # # other top-ranking models with fixed effects
    
    # # 4th best by CC
    # raw_prev_rate ~ assets_fn13 + assets_fn12 * zone + assets_fn1 * high_hiv
    f_covar8_zone = response ~ 1 +
        zone +
        high_hiv +
        assets_fn13 +
        assets_fn12 + assets_fn12:zone +
        assets_fn1 + assets_fn1:high_hiv
    ,
    
    # # 5th best by CC
    # raw_prev_rate ~ age_0004_pop:zone + age_1524_pop:zone + assets_fn12:zone
    f_covar9_zone = response ~ 1 +
        age_0004_pop:zone +
        age_1524_pop:zone +
        assets_fn12:zone
    ,
    
    # # 4th best by RSE
    # raw_prev_rate ~ age_1524_pop:zone + assets_fn12:zone + assets_fn8:zone
    f_covar10_zone = response ~ 1 +
        age_1524_pop:zone +
        assets_fn12:zone +
        assets_fn8:zone
    ,
    
    # # 5th best by RSE
    # raw_prev_rate ~ assets_fn12 * zone + assets_fn1 * high_hiv + assets_fn13 * high_hiv
    f_covar11_zone = response ~ 1 +
        zone +
        high_hiv +
        assets_fn12 + assets_fn12:zone +
        assets_fn1 + assets_fn1:high_hiv +
        assets_fn13 + assets_fn13:high_hiv

)


formulae_list6 <- list(
    
    # # simpler models with fixed effects for comparison
    
    # # urban
    f_urban = response ~ 1 +
        urban
    ,
    
    # # high_hiv
    f_high_hiv = response ~ 1 +
        high_hiv
    ,
    
    # # 45-54
    f_4554_pop = response ~ 1 +
        age_4554_pop
    ,
    
    # # male_pop
    f_male_pop = response ~ 1 +
        male_pop
    
)

formulae_list7 <- list(
    
    # # 2nd best by CC, all fixed
    # # raw_prev_rate ~ v394_notVisitedHF_2013 * zone + assets_fn12 * zone + assets_fn8 * urban
    f_covar4_zone_bym = response ~ 1 +
        zone +
        urban +
        v394_notVisitedHF_2013 + v394_notVisitedHF_2013:zone +
        assets_fn12 + assets_fn12:zone +
        assets_fn8 + assets_fn8:urban +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid")
    ,
    
    # # 1st best by CC with smoothing and LGA random int
    f_covar_zone_bym_alt = response ~ 1 +
        zone +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "besag", graph = NGA.adj, scale.model = TRUE) +
        f(CLUSTER2, model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 0.0001))))
    ,
    
    # # 1st best by CC with smoothing and LGA random int
    f_covar_zone_bym_alt2 = response ~ 1 +
        zone +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "bym", graph = NGA.adj, scale.model = TRUE,
          hyper = list(prec.unstruct = list(prior = "loggamma", param = c(1, 0.00001)),
                       prec.spatial  = list(prior = "loggamma", param = c(1, 0.00001))))
    ,
    
    # # 1st best by CC with smoothing and LGA random int
    f_covar_zone_bym_alt3 = response ~ 1 +
        zone +
        age_1524_pop + age_1524_pop:zone +
        mv149_lessThanSecondaryEduc_2013 + mv149_lessThanSecondaryEduc_2013:zone +
        assets_fn12 + assets_fn12:zone +
        f(CLUSTER, model = "bym", graph = NGA.adj, scale.model = TRUE,
          hyper = list(prec.unstruct = list(prior = "loggamma", param = c(1, 0.1)),
                       prec.spatial  = list(prior = "loggamma", param = c(1, 0.1))))
    
)
