################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
# 1.2.compute_dm_produced.R
#
################################################################################


# to compute the daily need of an individual (Kleiber equation) of a given species
kleiber_sea_land <- function(beta_land, beta_sea,
                             time_land, 
                             mass, n_sim, 
                             assimil_mean = NULL,  assimil_sd = 0.05,
                             assimil_a, assimil_b,
                             dietQuality
) {
  # should the daily ration be computed?
  if(!is.null(assimil_mean) && !is.null(dietQuality)) {
    a <- truncnorm::rtruncnorm(n = n_sim, 
                               mean = assimil_mean, 
                               sd = assimil_sd, 
                               a = assimil_a, 
                               b = assimil_b) # assimilation 
    
    # energetic needs/expenses on land are different than needs at sea, and 
    # as animals spend between 75 and 85% of their time at sea it should be 
    # taken into account to estimate "daily" ration 
    # "daily" ration because this is not consumed daily, it is only consumed 
    # at sea but it covers the energetic needs for time spent on land also
    return(tibble::tibble(ADMR_land = beta_land * (293.1*mass^(3/4)),
                          ADMR_sea = beta_sea * (293.1*mass^(3/4)),
                          A_rate = a) |>
             dplyr::mutate(Ration = (ADMR_land*time_land + ADMR_sea*(1 - time_land))/(a*dietQuality),
                           PercentBM = Ration/mass)
    )
  }
  else { return(list(ADMR_land = beta_land * (293.1*mass^(3/4)), 
                     ADMR_sea = beta_sea * (293.1*mass^(3/4)))) }
}



############################# COMPUTATION ######################################

run_dm_estimate <- function(input_tib) {
  

  output_tib <- input_tib |> 
    ###### SIMULATE UNCERTAINTY IN MASS, BETA, ABUNDANCE DATA, EXCRETION
    dplyr::mutate(
      ############################ COMPUTE INDIVIDUAL NRJTIC DATA, NEEDS AND CONSUMPTION OF POP ######  
      Indi_data = seq_along(BM) |>
        purrr::map(~ kleiber_sea_land(beta_land = purrr::pluck(Beta_land, ., 1), 
                                      beta_sea = purrr::pluck(Beta_sea, ., 1), 
                                      time_land = purrr::pluck(time_on_land, ., 1),
                                      mass = purrr::pluck(BM, ., 1), 
                                      n_sim = purrr::pluck(simu_count, .), 
                                      assimil_mean = 0.85, assimil_sd = 0.05, assimil_a = 0.8, assimil_b = 0.9,
                                      dietQuality = purrr::pluck(NRJ_diet, ., 1))), 
      
      ############################ COMPUTE CONSUMPTION OF FOOD ######
      
      # Individual consumption of dry matter in kg by ration
      conso_food_dm_ind_daily = seq_along(Indi_data) |> 
        purrr::map(~ purrr::pluck(Indi_data, ., "Ration") * purrr::pluck(dm_ration, .)), 
      # release_dm_ind_daily = seq_along(conso_food_dm_ind_daily) |> 
      #   purrr::map(~ purrr::pluck(conso_food_dm_ind_daily, .) * 
      #                purrr::pluck(dm_release, .)),
      # Individual release of dry matter during the 4 mths period of stay
      release_dm_ind_period_sea = seq_along(conso_food_dm_ind_daily) |> 
        purrr::map(~ purrr::pluck(conso_food_dm_ind_daily, .) * 
                     purrr::pluck(dm_release, .) * 
                     purrr::pluck(duration_of_stay, .) *
                     (1 - purrr::pluck(time_on_land, . ))),
      release_dm_ind_period_land = seq_along(conso_food_dm_ind_daily) |> 
        purrr::map(~ purrr::pluck(conso_food_dm_ind_daily, .) * 
                     purrr::pluck(dm_release, .) * 
                     purrr::pluck(duration_of_stay, .) *
                     purrr::pluck(time_on_land, . )),
      release_dm_ind_period_tot = seq_along(conso_food_dm_ind_daily) |> 
        purrr::map(~ purrr::pluck(release_dm_ind_period_sea, ., 1) +
                     purrr::pluck(release_dm_ind_period_land, ., 1) ),
      # total population production of dry matter on land during the 4 mths period of stay
      release_dm_pop_on_land_period = seq_along(release_dm_ind_period_land) |>
        purrr::map(~ sum(purrr::pluck(release_dm_ind_period_land, .))),
      # total population production of dry matter at sea during the 4 mths period of stay
      release_dm_pop_at_sea_period = seq_along(release_dm_ind_period_sea) |>
        purrr::map(~ sum(purrr::pluck(release_dm_ind_period_sea, .))),
      # total population production of dry matter during the 4 mths period of stay
      release_dm_pop_tot_period = seq_along(release_dm_pop_on_land_period) |>
        purrr::map(~ purrr::pluck(release_dm_pop_on_land_period, ., 1) +
                     purrr::pluck(release_dm_pop_at_sea_period, ., 1))
    )  |> 
    dplyr::mutate(release_dm_pop_tot_period = unlist(release_dm_pop_tot_period),
                  release_dm_pop_on_land_period = unlist(release_dm_pop_on_land_period),
                  release_dm_pop_at_sea_period = unlist(release_dm_pop_at_sea_period))
  
  

}
