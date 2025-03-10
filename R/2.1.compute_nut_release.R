################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
#1.4.compute_nut_release.R
#
################################################################################

#'
#'
#'
#'
#
add_bootstrap_scat_data <- function(output_dm,
                                    scat_compo_tib) {

  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "CN")) |>
    dplyr::select(-Code_sample)

  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "PS")) |>
    dplyr::select(-Code_sample)
  
  scat_compo_tib <- scat_compo_tib |>
    dplyr::select(-Code_sample)

  output_dm |>
    dplyr::mutate(scat_data_sampled_all = seq_along(release_dm_ind_period_sea) |>
                    purrr::map(~ scat_compo_tib |>
                                 dplyr::slice_sample(n = purrr::pluck(simu_count, .),
                                                     replace = TRUE)),
                  scat_data_sampled_sites = dplyr::case_when(Site == "Cap Noir" ~ seq_along(release_dm_ind_period_sea) |>
                                                               purrr::map(~  scat_compo_tib_CN |>
                                                                            dplyr::slice_sample(n = purrr::pluck(simu_count, .),
                                                                                                replace = TRUE)),
                                                             Site == "Pointe Suzanne" ~ seq_along(release_dm_ind_period_sea) |>
                                                               purrr::map(~ scat_compo_tib_PS |>
                                                                            dplyr::slice_sample(n = purrr::pluck(simu_count, .),
                                                                                                replace = TRUE)))) |>
    # keep only variables needed for the rest of computations
    dplyr::select(c(Site,
                    simu_count,
                    release_dm_ind_period_sea,
                    release_dm_ind_period_land,
                    release_dm_ind_period_tot,
                    scat_data_sampled_all,
                    scat_data_sampled_sites))

}


#'
#'
#'
#'
#
compute_nut_release <- function(output_dm_with_scat_compo) {

  output_dm_with_scat_compo |>
         dplyr::mutate(
           # with the full scat composition dataset
           # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
           release_nut_ind_period_sea_all_scats = seq_along(release_dm_ind_period_sea) |>
             purrr::map(~ purrr::pluck(release_dm_ind_period_sea, ., 1) * purrr::pluck(scat_data_sampled_all, .)*1e-6),
           release_nut_ind_period_land_all_scats = seq_along(release_dm_ind_period_land) |>
             purrr::map(~ purrr::pluck(release_dm_ind_period_land, ., 1) * purrr::pluck(scat_data_sampled_all, .)*1e-6),
           
           release_nut_pop_tot_period_sea_all_scats = seq_along(release_nut_ind_period_sea_all_scats) |>
             purrr::map(~ purrr::pluck(release_nut_ind_period_sea_all_scats, .) |>
                          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                                       Fe, Zn, Sr, Cu, Mn, Se,
                                                       Ni, Cd, V, Cr, As, Co, 
                                                       Ag, Pb, Mo),
                                              names_to = "Nutrient",
                                              values_to = "release_ind_tot_period_sea") |>
                          dplyr::group_by(Nutrient) |>
                          dplyr::summarise(pop_tot_release_sea_period = sum(release_ind_tot_period_sea)) |>
                          tidyr::pivot_wider(names_from = Nutrient,
                                             values_from = pop_tot_release_sea_period) |>
                          dplyr::select(c(Ca, P, Mg, Na, K, 
                                          Fe, Zn, Sr, Cu, Mn, Se,
                                          Ni, Cd, V, Cr, As, Co, 
                                          Ag, Pb, Mo))),
           release_nut_pop_tot_period_land_all_scats = seq_along(release_nut_ind_period_land_all_scats) |>
             purrr::map(~ purrr::pluck(release_nut_ind_period_land_all_scats, .) |>
                          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                                       Fe, Zn, Sr, Cu, Mn, Se,
                                                       Ni, Cd, V, Cr, As, Co, 
                                                       Ag, Pb, Mo),
                                              names_to = "Nutrient",
                                              values_to = "release_ind_tot_period_land") |>
                          dplyr::group_by(Nutrient) |>
                          dplyr::summarise(pop_tot_release_land_period = sum(release_ind_tot_period_land)) |>
                          tidyr::pivot_wider(names_from = Nutrient,
                                             values_from = pop_tot_release_land_period) |>
                          dplyr::select(c(Ca, P, Mg, Na, K, 
                                          Fe, Zn, Sr, Cu, Mn, Se,
                                          Ni, Cd, V, Cr, As, Co, 
                                          Ag, Pb, Mo))),
           release_nut_pop_tot_period_all_scats = seq_along(release_nut_ind_period_sea_all_scats) |>
             purrr::map(~ purrr::pluck(release_nut_pop_tot_period_sea_all_scats, .) +
                          purrr::pluck(release_nut_pop_tot_period_land_all_scats, .)),

           ################### with scats separated per site
           # concentrations from mg.kg-1 dry weight to kg.kg-1 because daily ration is in kg
           release_nut_ind_period_sea_sites = seq_along(release_dm_ind_period_sea) |>
             purrr::map(~ purrr::pluck(release_dm_ind_period_sea, ., 1) * purrr::pluck(scat_data_sampled_sites, .)*1e-6),
           release_nut_ind_period_land_sites = seq_along(release_dm_ind_period_land) |>
             purrr::map(~ purrr::pluck(release_dm_ind_period_land, ., 1) * purrr::pluck(scat_data_sampled_sites, .)*1e-6),
           
           release_nut_pop_tot_period_sea_sites = seq_along(release_nut_ind_period_sea_sites) |>
             purrr::map(~ purrr::pluck(release_nut_ind_period_sea_sites, .) |>
                          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                                       Fe, Zn, Sr, Cu, Mn, Se,
                                                       Ni, Cd, V, Cr, As, Co, 
                                                       Ag, Pb, Mo),
                                              names_to = "Nutrient",
                                              values_to = "release_ind_tot_period_sea") |>
                          dplyr::group_by(Nutrient) |>
                          dplyr::summarise(pop_tot_release_sea_period = sum(release_ind_tot_period_sea)) |>
                          tidyr::pivot_wider(names_from = Nutrient,
                                             values_from = pop_tot_release_sea_period) |>
                          dplyr::select(c(Ca, P, Mg, Na, K, 
                                          Fe, Zn, Sr, Cu, Mn, Se,
                                          Ni, Cd, V, Cr, As, Co, 
                                          Ag, Pb, Mo))),
           release_nut_pop_tot_period_land_sites = seq_along(release_nut_ind_period_land_sites) |>
             purrr::map(~ purrr::pluck(release_nut_ind_period_land_sites, .) |>
                          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                                       Fe, Zn, Sr, Cu, Mn, Se,
                                                       Ni, Cd, V, Cr, As, Co, 
                                                       Ag, Pb, Mo),
                                              names_to = "Nutrient",
                                              values_to = "release_ind_tot_period_land") |>
                          dplyr::group_by(Nutrient) |>
                          dplyr::summarise(pop_tot_release_land_period = sum(release_ind_tot_period_land)) |>
                          tidyr::pivot_wider(names_from = Nutrient,
                                             values_from = pop_tot_release_land_period) |>
                          dplyr::select(c(Ca, P, Mg, Na, K, 
                                          Fe, Zn, Sr, Cu, Mn, Se,
                                          Ni, Cd, V, Cr, As, Co, 
                                          Ag, Pb, Mo))),
           release_nut_pop_tot_period_sites = seq_along(release_nut_ind_period_sea_sites) |>
             purrr::map(~ purrr::pluck(release_nut_pop_tot_period_sea_sites, .) +
                          purrr::pluck(release_nut_pop_tot_period_land_sites, .))
         )
}
