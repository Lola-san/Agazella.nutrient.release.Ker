################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
# 1.1.prepare_pop_input_data.R
#
################################################################################


#'
#'
#'
#'
# Colony counts for Cap Noir and Pointe Suzanne 
Ker_summarise_count_data <- function(count_raw_tib) {
  
  # counts are different in the two sites: 
  
  # in Pointe Suzanne, the site was not counted entirely in one day: 
  # two first sectors were counted by the 3 operators on one day (adults + pups)
  # on a another day, other count in sector 3 but only one for adults
  
  # in Cap Noir: one count by a single operator on one day for the entire site
  # and second count by 3 operators on another day
  
  rbind(
    # Cap Noir
    count_raw_tib |>
      dplyr::filter(Site == "Cap Noir",
                    species == "fur seal",
                    status == "pups") |>
      dplyr::group_by(Site, operator, date, land_beach) |>
      dplyr::summarise(int_sum_count = sum(count)) |>
      # first count by LG was very low and is excluded
      # TJDD count started with separation land and land + beach, but then only 
      # land + beach, and only TJDD counted the river part, so for TJDD counts
      # we exclude the sum for land, and we sum the land_beach and 
      # the river part 
      dplyr::filter(date != lubridate::ymd("2023-01-14"), 
                    !(operator == "TJDD" & land_beach == "land")) |>
      dplyr::group_by(Site, operator) |>
      dplyr::summarise(sum_count = sum(int_sum_count)) |>
      dplyr::select(Site, sum_count),
    
    # Pointe Suzanne
    count_raw_tib |>
      dplyr::filter(Site == "Pointe Suzanne", 
                    species == "fur seal", 
                    status == "pups") |>
      dplyr::group_by(Site, operator, date, land_beach) |>
      dplyr::summarise(int_sum_count = sum(count)) |>
      # count of 2023/12/31 did not cover the entire colony and was excluded
      # and only one pup count the 2023/01/10 (TJDD sector 4), so was excluded too
      # and LG did only count on sector 3 on the 2023-01-04, so was excluded too 
      dplyr::filter(date != lubridate::ymd("2022-12-31"), 
                    date != lubridate::ymd("2023-01-10"), 
                    operator != "LG") |>
      # MC and TJDD did separated counts for the beach and land parts on the 
      # two remaining dates, so the total are the sum of their two counts
      dplyr::group_by(Site, date) |>
      dplyr::summarise(sum_count = sum(int_sum_count)) |>
      dplyr::select(Site, sum_count)
  ) 
  
   

}

#'
#'
#'
#'
# Colony counts for Cap Noir and Pointe Suzanne 
simulate_count_data <- function(counts_summmarise_pups_tib) {
  
  counts_summmarise_pups_tib |>
    tidyr::nest(initial_counts = sum_count)
  
}

#'
#'
#'
#'
# final pop data with colony count variability, body size variability, 
# metabolic index variability and energy in diet variability
pop_data_for_simulations <- function(pop_count_tib, 
                                     nsim) {
  
  tib_both_sites <- pop_count_tib |>
    dplyr::mutate(pop_count = seq_along(initial_counts) |>
                    purrr::map(~ tibble::tibble(simu_count = round(runif(n = nsim,  
                                                                         min = min(purrr::pluck(initial_counts, ., "sum_count"), 
                                                                                   na.rm = TRUE), 
                                                                         max = max(purrr::pluck(initial_counts, ., "sum_count"), 
                                                                                   na.rm = TRUE) +
                                                                           0.2*max(purrr::pluck(initial_counts, ., "sum_count"), 
                                                                                   na.rm = TRUE)), 
                                                                   0)))) |>
    tidyr::unnest(pop_count) |>
    dplyr::mutate(BM = seq_along(initial_counts) |>
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 27, 
                                                                             sd = 0.2*25, 
                                                                             a = 20, 
                                                                             b = 38))),  
                  Beta_sea = seq_along(initial_counts) |> # parameter values taken from JDD 2017, Ecol. and Evol. 
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 4.7, 
                                                                             sd = 0.3, 
                                                                             a = 3.5, 
                                                                             b = 6.5))),  
                  Beta_land = seq_along(initial_counts) |> # parameter values taken from JDD 2017, Ecol. and Evol. 
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 3.3, 
                                                                             sd = 0.1, 
                                                                             a = 2, 
                                                                             b = 4.5))), 
                  NRJ_diet = seq_along(initial_counts) |> # parameter values taken from TJDD2017, PloS ONE
                    purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = purrr::pluck(simu_count, .), 
                                                                             mean = 7.75*1e3, # kJ per g to kJ per kg
                                                                             sd = 2.47*1e3, 
                                                                             a = 4*1e3, 
                                                                             b = 13*1e3))), 
                  dm_ration = seq_along(initial_counts) |> # % dry matter in ration varying between 15 and 25 % 
                    # ie water content in prey between 75 and 85% 
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                       min = 0.15, 
                                       max = 0.25))), 
                  dm_release = seq_along(initial_counts) |> # % dry matter from ration that is released as feces
                    # parameter values taken from Keiver 1994 and Ronald 1984
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                       min = 0.09, 
                                       max = 0.17))),
                  duration_of_stay = seq_along(initial_counts) |> # nb of days spent "on land" (approx four months)
                    # ie duration of the breeding and moulting season
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                                             min = 100, 
                                                             max = 140))),
                  time_on_land = seq_along(initial_counts) |> # % of time spent on land
                    # estimated to be between 15 and 35%
                    purrr::map(~ tibble::as_tibble_col(runif(n = purrr::pluck(simu_count, .),  
                                                             min = 0.15, 
                                                             max = 0.35))))
  
  list(CN = tib_both_sites |> 
         dplyr:: filter(Site == "Cap Noir"), 
       PS = tib_both_sites |> 
         dplyr:: filter(Site == "Pointe Suzanne"))
  
}



