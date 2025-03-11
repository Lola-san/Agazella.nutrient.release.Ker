################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
# 1.3.compute_dm_produced.R
#
################################################################################

#'
#'
#'
#'
#
dm_per_site_period <- function(output_dm_CN, 
                               output_dm_PS) {
  
  
  rbind(output_dm_CN |>
          dplyr::select(c(Site, 
                        release_dm_pop_tot_period,  
                        release_dm_pop_on_land_period, 
                        release_dm_pop_at_sea_period)), 
        output_dm_PS |>
          dplyr::select(c(Site, 
                        release_dm_pop_tot_period,  
                        release_dm_pop_on_land_period, 
                        release_dm_pop_at_sea_period))) |>
    dplyr::group_by(Site) |>
    tidyr::pivot_longer(cols = c(release_dm_pop_tot_period, 
                                 release_dm_pop_on_land_period, 
                                 release_dm_pop_at_sea_period), 
                        names_to = "Location", 
                        values_to = "dm_released_kg") |>
    dplyr::mutate(Location = factor(
      dplyr::case_when(Location == "release_dm_pop_tot_period" ~ "Total (at sea & on land)", 
                       Location == "release_dm_pop_on_land_period" ~ "On land",
                       Location == "release_dm_pop_at_sea_period" ~ "At sea"), 
      levels = c("On land", "At sea", "Total (at sea & on land)")),
      Site = factor(
        dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                         Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
        levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site, Location) |>
    dplyr::summarise(min = min(dm_released_kg), 
                     mean = mean(dm_released_kg),
                     `10_quant` = quantile(dm_released_kg, 
                                           probs = c(0.1)),
                     `80_quant` = quantile(dm_released_kg, 
                                           probs = c(0.8)), 
                     max = max(dm_released_kg)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::facet_wrap(~ Location) +
    ggplot2::ylab("Fecal dry matter released during\nbreeding and moulting period (kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/exploration and intermediary results/barplot_dm_release_tot_pop_sites.jpg",
                  scale = 1,
                  height = 4, width = 9
  )
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different habitats 
test_diff_dm_sites_tot_period <- function(output_dm_CN,
                                          output_dm_PS) {
  
  table_test <- rbind(output_dm_CN |>
                         dplyr::select(c(Site, 
                                         release_dm_pop_tot_period)), 
                      output_dm_PS |>
                        dplyr::select(c(Site, 
                                        release_dm_pop_tot_period))) |>
    dplyr::select(Site, release_dm_pop_tot_period) |>
    dplyr::group_by(Site) |>
    tidyr::unnest(release_dm_pop_tot_period) |>
    dplyr::select(Site, release_dm_pop_tot_period) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = release_dm_pop_tot_period,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
    dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                                 TRUE ~ 0)
    ) |>
    dplyr::summarise(test_dm_sites = mean(t_PS_CN))
  
  openxlsx::write.xlsx(table_test, 
                       file = paste0("output/exploration and intermediary results/test_differences_sites_dm.xlsx"))
  
}



#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
table_model_param <- function(output_dm_CN,
                              output_dm_PS) {
  
  options(scipen = 999)
  
  # get rid of heavy outputs that are not needed here 
  # to lighten the data
  output_dm_CN <- output_dm_CN |>
      dplyr::select(-c(conso_food_dm_ind_daily,
                       # keep release_dm_ind_period_tot
                       release_dm_ind_period_sea, 
                       release_dm_ind_period_land,
                       release_dm_pop_tot_period,
                       release_dm_pop_on_land_period,
                       release_dm_pop_at_sea_period,
                       release_dm_pop_tot_period
      )) 
  output_dm_PS <- output_dm_PS |>
      dplyr::select(-c(conso_food_dm_ind_daily,
                       # keep release_dm_ind_period_tot
                       release_dm_ind_period_sea, 
                       release_dm_ind_period_land,
                       release_dm_pop_tot_period,
                       release_dm_pop_on_land_period,
                       release_dm_pop_at_sea_period,
                       release_dm_pop_tot_period
      ))
  
  
  table_summary_model_param <- output_dm_CN |>
    dplyr::select(c(Site, simu_count)) |>
    dplyr::group_by(Site) |>
    dplyr::summarize(min = round(min(simu_count), 0),
                     `2.5_quant` = round(quantile(simu_count, probs = c(0.025)), 0),
                     mean = round(mean(simu_count), 0),
                     median = round(median(simu_count), 0),
                     `97.5_quant` = round(quantile(simu_count, probs = c(0.975)), 0),
                     max = round(max(simu_count), 0)) |>
    dplyr::mutate(Parameter = "Abundance on colony") |>
    # next parameters
    dplyr::bind_rows(output_dm_PS |>
                       dplyr::select(c(Site, simu_count)) |>
                       dplyr::group_by(Site) |>
                       dplyr::summarize(min = round(min(simu_count), 0),
                                        `2.5_quant` = round(quantile(simu_count, probs = c(0.025)), 0),
                                        mean = round(mean(simu_count), 0),
                                        median = round(median(simu_count), 0),
                                        `97.5_quant` = round(quantile(simu_count, probs = c(0.975)), 0),
                                        max = round(max(simu_count), 0)) |>
                       dplyr::mutate(Parameter = "Abundance on colony"),
                     rbind(output_dm_CN |>
                             dplyr::select(BM), 
                           output_dm_PS |>
                             dplyr::select(BM)) |>
                       tidyr::unnest(BM) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Body mass (kg)"),
                     rbind(output_dm_CN |>
                             dplyr::select(Beta_land), 
                           output_dm_PS |>
                             dplyr::select(Beta_land)) |>
                       tidyr::unnest(Beta_land) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Beta_land"),
                     rbind(output_dm_CN |>
                             dplyr::select(Beta_sea), 
                           output_dm_PS |>
                             dplyr::select(Beta_sea)) |>
                       tidyr::unnest(Beta_sea) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Beta_sea"),
                     rbind(output_dm_CN |>
                             dplyr::select(NRJ_diet), 
                           output_dm_PS |>
                             dplyr::select(NRJ_diet)) |>
                       tidyr::unnest(NRJ_diet) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "mean energy content of diet (kJ/kg)"),
                     rbind(output_dm_CN |>
                             dplyr::select(dm_ration), 
                           output_dm_PS |>
                             dplyr::select(dm_ration)) |>
                       tidyr::unnest(dm_ration) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "% of dry matter in ration"),
                     rbind(output_dm_CN |>
                             dplyr::select(dm_release), 
                           output_dm_PS |>
                             dplyr::select(dm_release)) |>
                       tidyr::unnest(dm_release) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "dry matter release rate"),
                     rbind(output_dm_CN |>
                             dplyr::select(duration_of_stay), 
                           output_dm_PS |>
                             dplyr::select(duration_of_stay)) |>
                       tidyr::unnest(duration_of_stay) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "Duration of stay (days)"),
                     rbind(output_dm_CN |>
                             dplyr::select(time_on_land), 
                           output_dm_PS |>
                             dplyr::select(time_on_land)) |>
                       tidyr::unnest(time_on_land) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2)) |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = "% of time spent on land"),
                     rbind(output_dm_CN |>
                             dplyr::select(Indi_data), 
                           output_dm_PS |>
                             dplyr::select(Indi_data)) |>
                       tidyr::unnest(Indi_data) |>
                       tidyr::pivot_longer(cols = c(ADMR_land:PercentBM),
                                           names_to = "Parameter",
                                           values_to = "value") |>
                       dplyr::mutate(Site = NA, 
                                     Parameter = dplyr::case_when(Parameter == "A_rate" ~ "Assimilation rate",
                                                                  Parameter == "PercentBM" ~ "% of body mass (daily ration)",
                                                                  Parameter == "Ration" ~ "Daily ration (kg)",
                                                                  Parameter == "ADMR_land" ~ "Average Daily Metabolic Rate (land) (kJ)",
                                                                  Parameter == "ADMR_sea" ~ "Average Daily Metabolic Rate (sea) (kJ)")) |>
                       dplyr::group_by(Site, Parameter) |>
                       dplyr::summarize(min = round(min(value), 2),
                                        `2.5_quant` = round(quantile(value, probs = c(0.025)), 2),
                                        mean = round(mean(value), 2),
                                        median = round(median(value), 2),
                                        `97.5_quant` = round(quantile(value, probs = c(0.975)), 2),
                                        max = round(max(value), 2))
    )
  
  openxlsx::write.xlsx(table_summary_model_param,
                       file = "output/exploration and intermediary results/table_summary_model_parameters.xlsx")
  
  
}