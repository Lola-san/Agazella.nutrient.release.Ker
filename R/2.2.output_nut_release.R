################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
# 2.2._output_nut_release.R
#
################################################################################

#'
#'
#'
#'
#
nut_per_site_tot_period <- function(output_nut_release_CN, 
                                    output_nut_release_PS) {
  
  # first with all scats mixed 
  rbind(output_nut_release_CN |>
          dplyr::select(c(Site, 
                        release_nut_pop_tot_period_all_scats)), 
        output_nut_release_PS |>
          dplyr::select(c(Site, 
                        release_nut_pop_tot_period_all_scats))) |>
    tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb")), 
                  scat_compo = "All scat\nsamples mixed", 
                  Site = factor(dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                                                 Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                                levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site, scat_compo, Nutrient) |>
    dplyr::summarise(min = min(tot_pop_release_period_kg), 
                     mean = mean(tot_pop_release_period_kg), 
                     `2.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.025)),
                     `97.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.975)), 
                     max = max(tot_pop_release_period_kg)) |>
    dplyr::mutate(y_lim = dplyr::case_when(Nutrient == "P" ~ 500,
                                           Nutrient == "Fe" ~ 35, 
                                           Nutrient == "Zn" ~ 3.9, 
                                           Nutrient == "Cu" ~ 1.9, 
                                           Nutrient == "Mn" ~ 0.9, 
                                           Nutrient == "Se" ~ 0.3, 
                                           Nutrient == "Co" ~ 0.03)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, 
                                   y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            color = "slategray",
                            linewidth = 1.5,
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#353839",
                                          "#CDB5CD")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::ggtitle("All scats combined") +
    ggplot2::ylab("Total nutrient released\nin scats during breeding\nand moulting period (in kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/barplot_nut_release_tot_pop_sites_all_scats_mixed.jpg",
                  scale = 1,
                  height = 7, width = 8.5
  )
  
  # then with scat samples separated per site 
  rbind(output_nut_release_CN |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites)), 
        output_nut_release_PS |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites))) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb")), 
                  scat_compo = "Scat samples\nseparated\nper site", 
                  Site = factor(dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                                                 Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                                levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site, scat_compo, Nutrient) |>
    dplyr::summarise(min = min(tot_pop_release_period_kg), 
                     mean = mean(tot_pop_release_period_kg), 
                     `2.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.025)),
                     `97.5_quant` = quantile(tot_pop_release_period_kg, 
                                           probs = c(0.975)), 
                     max = max(tot_pop_release_period_kg)) |>
    dplyr::mutate(y_lim = dplyr::case_when(Nutrient == "P" ~ 500,
                                           Nutrient == "Fe" ~ 35, 
                                           Nutrient == "Zn" ~ 3.9, 
                                           Nutrient == "Cu" ~ 1.9, 
                                           Nutrient == "Mn" ~ 0.9, 
                                           Nutrient == "Se" ~ 0.3, 
                                           Nutrient == "Co" ~ 0.03)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, 
                                   y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            color = "slategray",
                            linewidth = 1.5,
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#353839",
                                          "#CDB5CD")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    #ggplot2::ggtitle("Scat separated per colony site") +
    ggplot2::ylab("Total nutrient released\nin scats during breeding\nand moulting period (in kg)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                        face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15, 
                                                        colour = "white"),
                   strip.background = ggplot2::element_rect(fill = "gray30"),
                   legend.position = "none")
  ggplot2::ggsave("output/barplot_nut_release_tot_pop_sites_scats_per_site.jpg",
                  scale = 1,
                  height = 6.5, width = 11
  )
  ggplot2::ggsave("output/barplot_nut_release_tot_pop_sites_scats_per_site.svg",
                  scale = 1,
                  height = 6.5, width = 11
  )
}



#'
#'
#'
#'
#
table_nut_per_site_sea_land_period <- function(output_nut_release_CN, 
                                               output_nut_release_PS) {
  
  table_summary_per_site <- rbind(
    # first with all scat samples mixed
    rbind(output_nut_release_CN |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_all_scats), 
          output_nut_release_PS |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_all_scats)) |>
      tidyr::unnest(release_nut_pop_tot_period_land_all_scats) |>
      tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                               
                                   Fe, Zn, Sr, Cu, Mn, Se,                                         
                                   Ni, Cd, V, Cr, As, Co,                                         
                                   Ag, Mo, Pb), 
                          names_to = "Nutrient", 
                          values_to = "on_land_pop_release_period_kg") |> 
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(mean_on_land = round(mean(on_land_pop_release_period_kg),4), 
                       low2_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.025), 4), 
                       high97_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.975), 4), 
                       min_on_land = round(min(on_land_pop_release_period_kg), 4),
                       max_on_land = round(max(on_land_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:max_on_land), 
                          names_to = "variable", 
                          values_to = "estimates") |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_all_scats), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_all_scats)) |>
          tidyr::unnest(release_nut_pop_tot_period_sea_all_scats) |>
          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                       
                                       Fe, Zn, Sr, Cu, Mn, Se,                                  
                                       Ni, Cd, V, Cr, As, Co,                                  
                                       Ag, Mo, Pb), 
                              names_to = "Nutrient", 
                              values_to = "at_sea_pop_release_period_kg") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_at_sea = round(mean(at_sea_pop_release_period_kg),4),
                           low2_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.025), 4), 
                           high97_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.975), 4),  
                           min_at_sea = round(min(at_sea_pop_release_period_kg), 4),
                           max_at_sea = round(max(at_sea_pop_release_period_kg), 4)) |>
          tidyr::pivot_longer(cols = c(mean_at_sea:max_at_sea), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_all_scats), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_all_scats)) |>
          tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                           
                                       Fe, Zn, Sr, Cu, Mn, Se,                                   
                                       Ni, Cd, V, Cr, As, Co,                                    
                                       Ag, Mo, Pb), 
                              names_to = "Nutrient", 
                              values_to = "release_nut_pop_tot_period_sites") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_tot = round(mean(release_nut_pop_tot_period_sites),4), 
                           low2_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.025), 4), 
                           high97_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.975), 4),  
                           min_tot = round(min(release_nut_pop_tot_period_sites), 4),
                           max_tot = round(max(release_nut_pop_tot_period_sites), 4)) |>
          tidyr::pivot_longer(cols = c(mean_tot:max_tot), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      tidyr::pivot_wider(names_from = variable, 
                         values_from = estimates) |>
      dplyr::mutate(on_land_percent = round(100*mean_on_land/mean_tot, 0), 
                    at_sea_percent = round(100*mean_at_sea/mean_tot, 0)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:at_sea_percent), 
                          names_to = "level_variable", 
                          values_to = "estimate_kg") |>
      dplyr::mutate(level = dplyr::case_when(stringr::str_detect(level_variable, "tot") ~ "total", 
                                             stringr::str_detect(level_variable, "on_land") ~ "on land",
                                             stringr::str_detect(level_variable, "at_sea") ~ "at sea"), 
                    variable = dplyr::case_when(stringr::str_detect(level_variable, "mean") ~ "mean", 
                                                stringr::str_detect(level_variable, "low2_5") ~ "2.5% quantile",
                                                stringr::str_detect(level_variable, "high97_5") ~ "97.5% quantile",
                                                stringr::str_detect(level_variable, "min") ~ "min",
                                                stringr::str_detect(level_variable, "max") ~ "max",
                                                stringr::str_detect(level_variable, "percent") ~ "percent"), 
                    scat_compo = "All scat samples mixed", 
                    Nutrient = factor(Nutrient, 
                                      levels = c("Ca", "P", "Mg", "Na", "K", 
                                                 "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                 "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                 "Ag", "Mo", "Pb"))) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = estimate_kg) |>
      dplyr::select(c(scat_compo, Site, level, variable,  
                      Ca, P, Mg, Na, K,                                            
                      Fe, Zn, Sr, Cu, Mn, Se,                                    
                      Ni, Cd, V, Cr, As, Co,                                     
                      Ag, Mo, Pb)),
    # then with scat samples separated per site 
    rbind(output_nut_release_CN |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_sites), 
          output_nut_release_PS |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_land_sites)) |>
      tidyr::unnest(release_nut_pop_tot_period_land_sites) |>
      tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                              
                                   Fe, Zn, Sr, Cu, Mn, Se,                                       
                                   Ni, Cd, V, Cr, As, Co,                                       
                                   Ag, Mo, Pb), 
                          names_to = "Nutrient", 
                          values_to = "on_land_pop_release_period_kg") |> 
      dplyr::group_by(Site, Nutrient) |>
      dplyr::summarise(mean_on_land = round(mean(on_land_pop_release_period_kg),4), 
                       low2_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.025), 4), 
                       high97_5quant_on_land = round(quantile(on_land_pop_release_period_kg, 0.975), 4),  
                       min_on_land = round(min(on_land_pop_release_period_kg), 4),
                       max_on_land = round(max(on_land_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:max_on_land), 
                          names_to = "variable", 
                          values_to = "estimates") |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_sites), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sea_sites)) |>
          tidyr::unnest(release_nut_pop_tot_period_sea_sites) |>
          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                           
                                       Fe, Zn, Sr, Cu, Mn, Se,                                    
                                       Ni, Cd, V, Cr, As, Co,                                    
                                       Ag, Mo, Pb), 
                              names_to = "Nutrient", 
                              values_to = "at_sea_pop_release_period_kg") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_at_sea = round(mean(at_sea_pop_release_period_kg),4), 
                           low2_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.025), 4), 
                           high97_5quant_at_sea = round(quantile(at_sea_pop_release_period_kg, 0.975), 4),  
                           min_at_sea = round(min(at_sea_pop_release_period_kg), 4),
                           max_at_sea = round(max(at_sea_pop_release_period_kg), 4)) |>
          tidyr::pivot_longer(cols = c(mean_at_sea:max_at_sea), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      dplyr::bind_rows(
        rbind(output_nut_release_CN |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sites), 
              output_nut_release_PS |>
                dplyr::select(Site, 
                              release_nut_pop_tot_period_sites)) |>
          tidyr::unnest(release_nut_pop_tot_period_sites) |>
          tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                            
                                       Fe, Zn, Sr, Cu, Mn, Se,                                    
                                       Ni, Cd, V, Cr, As, Co,                                     
                                       Ag, Mo, Pb), 
                              names_to = "Nutrient", 
                              values_to = "release_nut_pop_tot_period_sites") |> 
          dplyr::group_by(Site, Nutrient) |>
          dplyr::summarise(mean_tot = round(mean(release_nut_pop_tot_period_sites),4), 
                           low2_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.025), 4), 
                           high97_5quant_tot = round(quantile(release_nut_pop_tot_period_sites, 0.975), 4), 
                           min_tot = round(min(release_nut_pop_tot_period_sites), 4),
                           max_tot = round(max(release_nut_pop_tot_period_sites), 4)) |>
          tidyr::pivot_longer(cols = c(mean_tot:max_tot), 
                              names_to = "variable", 
                              values_to = "estimates")) |>
      # compute statistics
      tidyr::pivot_wider(names_from = variable, 
                         values_from = estimates) |>
      dplyr::mutate(on_land_percent = round(100*mean_on_land/mean_tot, 0), 
                    at_sea_percent = round(100*mean_at_sea/mean_tot, 0)) |>
      tidyr::pivot_longer(cols = c(mean_on_land:at_sea_percent), 
                          names_to = "level_variable", 
                          values_to = "estimate_kg") |>
      dplyr::mutate(level = dplyr::case_when(stringr::str_detect(level_variable, "tot") ~ "total", 
                                             stringr::str_detect(level_variable, "on_land") ~ "on land",
                                             stringr::str_detect(level_variable, "at_sea") ~ "at sea"), 
                    variable = dplyr::case_when(stringr::str_detect(level_variable, "mean") ~ "mean", 
                                                stringr::str_detect(level_variable, "low2_5") ~ "2.5% quantile",
                                                stringr::str_detect(level_variable, "high97_5") ~ "97.5% quantile",
                                                stringr::str_detect(level_variable, "min") ~ "min",
                                                stringr::str_detect(level_variable, "max") ~ "max",
                                                stringr::str_detect(level_variable, "percent") ~ "percent"), 
                    scat_compo = "Scats separated between colonies", 
                    Nutrient = factor(Nutrient, 
                                      levels = c("Ca", "P", "Mg", "Na", "K", 
                                                 "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                 "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                 "Ag", "Mo", "Pb"))) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = estimate_kg) |>
      dplyr::select(c(scat_compo, Site, level, variable,  
                      Ca, P, Mg, Na, K,                                            
                      Fe, Zn, Sr, Cu, Mn, Se,                                    
                      Ni, Cd, V, Cr, As, Co,                                     
                      Ag, Mo, Pb)))
  
  
  openxlsx::write.xlsx(table_summary_per_site, 
                       file = paste0("output/table_summary_sites_nut_total_estimates.xlsx"))
  
  
  
  table_summary_total <- rbind(
    # first with all scat samples mixed
    # at sea
    output_nut_release_CN |>
      dplyr::ungroup() |>
      dplyr::rename(release_nut_pop_tot_period_sea_all_scats_CN = release_nut_pop_tot_period_sea_all_scats) |>
      dplyr::select(release_nut_pop_tot_period_sea_all_scats_CN) |>
      dplyr::bind_cols(output_nut_release_PS |>
                         dplyr::ungroup() |>
                         dplyr::rename(release_nut_pop_tot_period_sea_all_scats_PS = release_nut_pop_tot_period_sea_all_scats) |>
                         dplyr::select(release_nut_pop_tot_period_sea_all_scats_PS)) |>
      dplyr::mutate(release_nut_pop_tot_period_sea_sites_all_scats = seq_along(release_nut_pop_tot_period_sea_all_scats_CN) |>
                      purrr::map(~ purrr::pluck(release_nut_pop_tot_period_sea_all_scats_CN, .) + 
                                   purrr::pluck(release_nut_pop_tot_period_sea_all_scats_PS, .))) |>
      tidyr::unnest(release_nut_pop_tot_period_sea_sites_all_scats) |>
      tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                              
                                   Fe, Zn, Sr, Cu, Mn, Se,                                       
                                   Ni, Cd, V, Cr, As, Co,                                        
                                   Ag, Mo, Pb), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_sea_kg") |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_tot_sea_sites = round(mean(tot_pop_release_period_sea_kg),4),
                       low2_5quant_tot_sea_sites = round(quantile(tot_pop_release_period_sea_kg, 0.025), 4), 
                       high97_5quant_tot_sea_sites = round(quantile(tot_pop_release_period_sea_kg, 0.975), 4),  
                       min_tot_sea_sites = round(min(tot_pop_release_period_sea_kg), 4),
                       max_tot_sea_sites = round(max(tot_pop_release_period_sea_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_tot_sea_sites:max_tot_sea_sites), 
                          names_to = "variable", 
                          values_to = "estimate_kg"),
    # on land
    output_nut_release_CN |>
      dplyr::ungroup() |>
      dplyr::rename(release_nut_pop_tot_period_land_all_scats_CN = release_nut_pop_tot_period_land_all_scats) |>
      dplyr::select(release_nut_pop_tot_period_land_all_scats_CN) |>
      dplyr::bind_cols(output_nut_release_PS |>
                         dplyr::ungroup() |>
                         dplyr::rename(release_nut_pop_tot_period_land_all_scats_PS = release_nut_pop_tot_period_land_all_scats) |>
                         dplyr::select(release_nut_pop_tot_period_land_all_scats_PS)) |>
      dplyr::mutate(release_nut_pop_tot_period_land_sites_all_scats = seq_along(release_nut_pop_tot_period_land_all_scats_CN) |>
                      purrr::map(~ purrr::pluck(release_nut_pop_tot_period_land_all_scats_CN, .) + 
                                   purrr::pluck(release_nut_pop_tot_period_land_all_scats_PS, .))) |>
      tidyr::unnest(release_nut_pop_tot_period_land_sites_all_scats) |>
      tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                         
                                   Fe, Zn, Sr, Cu, Mn, Se,                                   
                                   Ni, Cd, V, Cr, As, Co,                                   
                                   Ag, Mo, Pb), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_land_kg") |> 
      dplyr::group_by(Nutrient) |>
      dplyr::summarise(mean_tot_land_sites = round(mean(tot_pop_release_period_land_kg),4),
                       low2_5quant_tot_land_sites = round(quantile(tot_pop_release_period_land_kg, 0.025), 4), 
                       high97_5quant_tot_land_sites = round(quantile(tot_pop_release_period_land_kg, 0.975), 4),  
                       min_tot_land_sites = round(min(tot_pop_release_period_land_kg), 4),
                       max_tot_land_sites = round(max(tot_pop_release_period_land_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_tot_land_sites:max_tot_land_sites), 
                          names_to = "variable", 
                          values_to = "estimate_kg"),
    # total sea + land
    output_nut_release_CN |>
    dplyr::ungroup() |>
    dplyr::rename(release_nut_pop_tot_period_all_scats_CN = release_nut_pop_tot_period_all_scats) |>
    dplyr::select(release_nut_pop_tot_period_all_scats_CN) |>
    dplyr::bind_cols(output_nut_release_PS |>
                       dplyr::ungroup() |>
                       dplyr::rename(release_nut_pop_tot_period_all_scats_PS = release_nut_pop_tot_period_all_scats) |>
                       dplyr::select(release_nut_pop_tot_period_all_scats_PS)) |>
    dplyr::mutate(release_nut_pop_tot_period_sites_all_scats = seq_along(release_nut_pop_tot_period_all_scats_CN) |>
                    purrr::map(~ purrr::pluck(release_nut_pop_tot_period_all_scats_CN, .) + 
                                 purrr::pluck(release_nut_pop_tot_period_all_scats_PS, .))) |>
    tidyr::unnest(release_nut_pop_tot_period_sites_all_scats) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                             
                                 Fe, Zn, Sr, Cu, Mn, Se,                                        
                                 Ni, Cd, V, Cr, As, Co,                                         
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean_tot_sites = round(mean(tot_pop_release_period_kg),4),
                     low2_5quant_tot_sites = round(quantile(tot_pop_release_period_kg, 0.025), 4), 
                     high97_5quant_tot_sites = round(quantile(tot_pop_release_period_kg, 0.975), 4),  
                     min_tot_sites = round(min(tot_pop_release_period_kg), 4),
                     max_tot_sites = round(max(tot_pop_release_period_kg), 4)) |>
      tidyr::pivot_longer(cols = c(mean_tot_sites:max_tot_sites), 
                          names_to = "variable", 
                          values_to = "estimate_kg")) |>
    dplyr::mutate(level = dplyr::case_when(stringr::str_detect(variable, "tot_sites") ~ "total", 
                                           stringr::str_detect(variable, "_land") ~ "on land",
                                           stringr::str_detect(variable, "_sea") ~ "at sea"), 
                  variable = dplyr::case_when(stringr::str_detect(variable, "mean") ~ "mean", 
                                              stringr::str_detect(variable, "low2_5") ~ "2.5% quantile",
                                              stringr::str_detect(variable, "high97_5") ~ "97.5% quantile",
                                              stringr::str_detect(variable, "min") ~ "min",
                                              stringr::str_detect(variable, "max") ~ "max",
                                              stringr::str_detect(variable, "percent") ~ "percent"), 
                  scat_compo = "All scat samples mixed", 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = estimate_kg) |>
    dplyr::select(c(scat_compo, level, variable,  
                    Ca, P, Mg, Na, K,                                            
                    Fe, Zn, Sr, Cu, Mn, Se,                                    
                    Ni, Cd, V, Cr, As, Co,                                     
                    Ag, Mo, Pb)) |>
    dplyr::bind_rows(
      # second with scat samples separated per colony
      dplyr::bind_rows(
      # at sea
      output_nut_release_CN |>
        dplyr::ungroup() |>
        dplyr::rename(release_nut_pop_tot_period_sea_colony_CN = release_nut_pop_tot_period_sea_sites) |>
        dplyr::select(release_nut_pop_tot_period_sea_colony_CN) |>
        dplyr::bind_cols(output_nut_release_PS |>
                           dplyr::ungroup() |>
                           dplyr::rename(release_nut_pop_tot_period_sea_colony_PS = release_nut_pop_tot_period_sea_sites) |>
                           dplyr::select(release_nut_pop_tot_period_sea_colony_PS)) |>
        dplyr::mutate(release_nut_pop_tot_period_sea_sites_colonies = seq_along(release_nut_pop_tot_period_sea_colony_CN) |>
                        purrr::map(~ purrr::pluck(release_nut_pop_tot_period_sea_colony_CN, .) + 
                                     purrr::pluck(release_nut_pop_tot_period_sea_colony_PS, .))) |>
        tidyr::unnest(release_nut_pop_tot_period_sea_sites_colonies) |>
        tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                
                                     Fe, Zn, Sr, Cu, Mn, Se,                          
                                     Ni, Cd, V, Cr, As, Co,                             
                                     Ag, Mo, Pb), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_sea_kg") |> 
        dplyr::group_by(Nutrient) |>
        dplyr::summarise(mean_tot_sea_sites = round(mean(tot_pop_release_period_sea_kg),4),
                         low2_5quant_tot_sea_sites = round(quantile(tot_pop_release_period_sea_kg, 0.025), 4), 
                         high97_5quant_tot_sea_sites = round(quantile(tot_pop_release_period_sea_kg, 0.975), 4),  
                         min_tot_sea_sites = round(min(tot_pop_release_period_sea_kg), 4),
                         max_tot_sea_sites = round(max(tot_pop_release_period_sea_kg), 4)) |>
        tidyr::pivot_longer(cols = c(mean_tot_sea_sites:max_tot_sea_sites), 
                            names_to = "variable", 
                            values_to = "estimate_kg"),
      # on land
      output_nut_release_CN |>
        dplyr::ungroup() |>
        dplyr::rename(release_nut_pop_tot_period_land_colony_CN = release_nut_pop_tot_period_land_sites) |>
        dplyr::select(release_nut_pop_tot_period_land_colony_CN) |>
        dplyr::bind_cols(output_nut_release_PS |>
                           dplyr::ungroup() |>
                           dplyr::rename(release_nut_pop_tot_period_land_colony_PS = release_nut_pop_tot_period_land_sites) |>
                           dplyr::select(release_nut_pop_tot_period_land_colony_PS)) |>
        dplyr::mutate(release_nut_pop_tot_period_land_colonies_all_scats = seq_along(release_nut_pop_tot_period_land_colony_CN) |>
                        purrr::map(~ purrr::pluck(release_nut_pop_tot_period_land_colony_CN, .) + 
                                     purrr::pluck(release_nut_pop_tot_period_land_colony_PS, .))) |>
        tidyr::unnest(release_nut_pop_tot_period_land_colonies_all_scats) |>
        tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                               
                                     Fe, Zn, Sr, Cu, Mn, Se,                          
                                     Ni, Cd, V, Cr, As, Co,                           
                                     Ag, Mo, Pb), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_land_kg") |> 
        dplyr::group_by(Nutrient) |>
        dplyr::summarise(mean_tot_land_sites = round(mean(tot_pop_release_period_land_kg),4),
                         low2_5quant_tot_land_sites = round(quantile(tot_pop_release_period_land_kg, 0.025), 4), 
                         high97_5quant_tot_land_sites = round(quantile(tot_pop_release_period_land_kg, 0.975), 4),  
                         min_tot_land_sites = round(min(tot_pop_release_period_land_kg), 4),
                         max_tot_land_sites = round(max(tot_pop_release_period_land_kg), 4)) |>
        tidyr::pivot_longer(cols = c(mean_tot_land_sites:max_tot_land_sites), 
                            names_to = "variable", 
                            values_to = "estimate_kg"),
      # total sea + land
      output_nut_release_CN |>
        dplyr::ungroup() |>
        dplyr::rename(release_nut_pop_tot_period_colony_CN = release_nut_pop_tot_period_sites) |>
        dplyr::select(release_nut_pop_tot_period_colony_CN) |>
        dplyr::bind_cols(output_nut_release_PS |>
                           dplyr::ungroup() |>
                           dplyr::rename(release_nut_pop_tot_period_colony_PS = release_nut_pop_tot_period_sites) |>
                           dplyr::select(release_nut_pop_tot_period_colony_PS)) |>
        dplyr::mutate(release_nut_pop_tot_period_sites_colonies = seq_along(release_nut_pop_tot_period_colony_CN) |>
                        purrr::map(~ purrr::pluck(release_nut_pop_tot_period_colony_CN, .) + 
                                     purrr::pluck(release_nut_pop_tot_period_colony_PS, .))) |>
        tidyr::unnest(release_nut_pop_tot_period_sites_colonies) |>
        tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                
                                     Fe, Zn, Sr, Cu, Mn, Se,                      
                                     Ni, Cd, V, Cr, As, Co,                     
                                     Ag, Mo, Pb), 
                            names_to = "Nutrient", 
                            values_to = "tot_pop_release_period_kg") |> 
        dplyr::group_by(Nutrient) |>
        dplyr::summarise(mean_tot_sites = round(mean(tot_pop_release_period_kg),4),
                         low2_5quant_tot_sites = round(quantile(tot_pop_release_period_kg, 0.025), 4), 
                         high97_5quant_tot_sites = round(quantile(tot_pop_release_period_kg, 0.975), 4),  
                         min_tot_sites = round(min(tot_pop_release_period_kg), 4),
                         max_tot_sites = round(max(tot_pop_release_period_kg), 4)) |>
        tidyr::pivot_longer(cols = c(mean_tot_sites:max_tot_sites), 
                            names_to = "variable", 
                            values_to = "estimate_kg")) |>
    dplyr::mutate(level = dplyr::case_when(stringr::str_detect(variable, "tot_sites") ~ "total", 
                                           stringr::str_detect(variable, "_land") ~ "on land",
                                           stringr::str_detect(variable, "_sea") ~ "at sea"), 
                  variable = dplyr::case_when(stringr::str_detect(variable, "mean") ~ "mean", 
                                              stringr::str_detect(variable, "low2_5") ~ "2.5% quantile",
                                              stringr::str_detect(variable, "high97_5") ~ "97.5% quantile",
                                              stringr::str_detect(variable, "min") ~ "min",
                                              stringr::str_detect(variable, "max") ~ "max",
                                              stringr::str_detect(variable, "percent") ~ "percent"), 
                  scat_compo = "Scats separated between colonies", 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = estimate_kg) |>
    dplyr::select(c(scat_compo, level, variable,  
                    Ca, P, Mg, Na, K,                                            
                    Fe, Zn, Sr, Cu, Mn, Se,                                    
                    Ni, Cd, V, Cr, As, Co,                                     
                    Ag, Mo, Pb))
      
    )
  
  
  openxlsx::write.xlsx(table_summary_total, 
                       file = paste0("output/table_summary_total_sites_estimates.xlsx"))
  
  
}



#'
#'
#'
#'
#'
#
test_nut_sites_tot_period <- function(output_nut_release_CN, 
                                      output_nut_release_PS) {
  
  table_test <- rbind(
    # first with scats separated per site
    rbind(output_nut_release_CN |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_sites), 
          output_nut_release_PS |>
            dplyr::select(Site, 
                          release_nut_pop_tot_period_sites)) |>
      tidyr::unnest(release_nut_pop_tot_period_sites) |>
      tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                           
                                   Fe, Zn, Sr, Cu, Mn, Se,                          
                                   Ni, Cd, V, Cr, As, Co,                            
                                   Ag, Mo, Pb), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Ca", "P", "Mg", "Na", "K", 
                                                 "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                 "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                 "Ag", "Mo", "Pb"))) |>
      dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
      tidyr::pivot_wider(names_from = Site,
                         values_from = tot_pop_release_period_kg,
                         values_fn = list) |>
      tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
      dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                               TRUE ~ 0), 
                    scat_compo = "Scats separated per site") |>
      dplyr::group_by(scat_compo, Nutrient) |>
      dplyr::summarise(test_nut_sites = round(mean(t_PS_CN), 5)) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = test_nut_sites),
    # then with all scats mixed 
    table_test_all_scats <- rbind(output_nut_release_CN |>
                                    dplyr::select(Site, 
                                                  release_nut_pop_tot_period_all_scats), 
                                  output_nut_release_PS |>
                                    dplyr::select(Site, 
                                                  release_nut_pop_tot_period_all_scats)) |>
      tidyr::unnest(release_nut_pop_tot_period_all_scats) |>
      tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                                                         
                                   Fe, Zn, Sr, Cu, Mn, Se,       
                                   Ni, Cd, V, Cr, As, Co,                          
                                   Ag, Mo, Pb), 
                          names_to = "Nutrient", 
                          values_to = "tot_pop_release_period_kg") |> 
      dplyr::mutate(Nutrient = factor(Nutrient, 
                                      levels = c("Ca", "P", "Mg", "Na", "K", 
                                                 "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                 "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                 "Ag", "Mo", "Pb"))) |>
      dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
      tidyr::pivot_wider(names_from = Site,
                         values_from = tot_pop_release_period_kg,
                         values_fn = list) |>
      tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
      dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                               TRUE ~ 0), 
                    scat_compo = "All scats mixed") |>
      dplyr::group_by(scat_compo, Nutrient) |>
      dplyr::summarise(test_nut_sites = round(mean(t_PS_CN), 5)) |>
      tidyr::pivot_wider(names_from = Nutrient, 
                         values_from = test_nut_sites)
  )
  
  openxlsx::write.xlsx(table_test, 
                       file = paste0("output/test_differences_sites_with_and_without_all_scats.xlsx"))
  
  
  table_test_scat_per_site_only <- rbind(output_nut_release_CN |>
                                          dplyr::select(Site, 
                                                        release_nut_pop_tot_period_sites), 
                                        output_nut_release_PS |>
                                          dplyr::select(Site, 
                                                        release_nut_pop_tot_period_sites)) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K,                           
                                 Fe, Zn, Sr, Cu, Mn, Se,                          
                                 Ni, Cd, V, Cr, As, Co,                            
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    dplyr::select(Site, Nutrient, tot_pop_release_period_kg) |>
    tidyr::pivot_wider(names_from = Site,
                       values_from = tot_pop_release_period_kg,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Cap Noir`, `Pointe Suzanne`)) |>
    dplyr::mutate(t_PS_CN = dplyr::case_when(`Pointe Suzanne` > `Cap Noir` ~ 1,
                                             TRUE ~ 0)) |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(p = round(mean(t_PS_CN), 5)) |>
    dplyr::mutate(significant = dplyr::case_when(p <= 0.05 ~ "yes", 
                                                 p >= 0.95 ~ "yes",
                                                 TRUE ~ "no"))
  
  openxlsx::write.xlsx(table_test_scat_per_site_only, 
                       file = paste0("output/test_differences_sites_scats-per-site.xlsx"))
  
}



#'
#'
#'
#'
#
diff_sites_release_levels_nutrients <- function(output_nut_release_CN, 
                                                output_nut_release_PS) {
  
  rbind(output_nut_release_CN |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites)), 
        output_nut_release_PS |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites))) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::reframe(mean = mean(tot_pop_release_period_kg)) |>
    tidyr::pivot_wider(names_from = Site, 
                       values_from = mean) |>
    dplyr::mutate(p_diff_change_CNo_to_PSuz = 100 * (`Pointe Suzanne` - `Cap Noir`)/`Cap Noir`) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = p_diff_change_CNo_to_PSuz, 
                                   fill = Nutrient), 
                      stat = "identity") +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#D98594FF", "#26432FFF",
                                          "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                          "#2e276a", "#5c4d73", "#D8AF39FF",
                                          "#583B2BFF", "#14191FFF")) +
    # add horizontal line with % change of colony counts
    ggplot2::geom_hline(ggplot2::aes(yintercept = 57), 
                        linetype = "dashed",
                        color = "darkred") +
    ggplot2::ylab("% of deviation from expected difference\nin mean nutrient release level from Cap Noir\nto Pointe Suzanne fur seal colony") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/barplot_diff_nut_release_tot_pop_from_CNo_to_PSuz_with57line.jpg",
                  scale = 1,
                  height = 4, width = 7.5
  )
  
  
  rbind(output_nut_release_CN |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites)), 
        output_nut_release_PS |>
          dplyr::select(c(Site, 
                          release_nut_pop_tot_period_sites))) |>
    tidyr::unnest(release_nut_pop_tot_period_sites) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "tot_pop_release_period_kg") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::reframe(mean = mean(tot_pop_release_period_kg)) |>
    tidyr::pivot_wider(names_from = Site, 
                       values_from = mean) |>
    # 57% is the difference in colony abundance estimate from Cap Noir to Pointe Suzanne
    # will serve as the zero here
    dplyr::mutate(p_diff_change_CNo_to_PSuz = (100 * (`Pointe Suzanne` - `Cap Noir`)/`Cap Noir`)-57) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, 
                                   y = p_diff_change_CNo_to_PSuz, 
                                   fill = Nutrient), 
                      stat = "identity") +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#D98594FF", "#26432FFF",
                                          "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                          "#2e276a", "#5c4d73", "#D8AF39FF",
                                          "#583B2BFF", "#14191FFF")) +
    ggplot2::ylab("% of deviation from expected\ndifference in mean nutrient release\nlevel from Cap Noir to Pointe\nSuzanne fur seal colony") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15, 
                                                       face = "bold"), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 15, 
                                                        face = "bold"),
                   #strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/barplot_diff_nut_release_tot_pop_from_CNo_to_PSuz_with57_asref.jpg",
                  scale = 1,
                  height = 4, width = 7.5
  )
  ggplot2::ggsave("output/barplot_diff_nut_release_tot_pop_from_CNo_to_PSuz_with57_asref.svg",
                  scale = 1,
                  height = 4, width = 7.5
  )
  
  
}



#'
#'
#'
#'
#
nb_animals_per_site_simulated <- function(output_nut_release_CN, 
                                           output_nut_release_PS) {

  rbind(output_nut_release_CN |>
          dplyr::mutate(Site = "Cap Noir") |>
          dplyr::select(c(simu_count, 
                          Site)), 
        output_nut_release_PS |>
          dplyr::mutate(Site = "Pointe Suzanne") |>
          dplyr::select(c(simu_count, 
                          Site))) |>
    dplyr::mutate(Site = factor(dplyr::case_when(Site == "Cap Noir" ~ "Cap\nNoir", 
                                                 Site == "Pointe Suzanne" ~ "Pointe\nSuzanne"), 
                                levels = c("Cap\nNoir", "Pointe\nSuzanne"))) |>
    dplyr::group_by(Site) |>
    dplyr::summarise(min = min(simu_count), 
                     mean = mean(simu_count), 
                     `2.5_quant` = quantile(simu_count, 
                                            probs = c(0.025)),
                     `97.5_quant` = quantile(simu_count, 
                                             probs = c(0.975)), 
                     max = max(simu_count)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Site, 
                                   y = mean, 
                                   fill = Site), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(width = 1)) +
    ggplot2::geom_linerange(ggplot2::aes(x = Site, 
                                         ymin = `2.5_quant`, 
                                         ymax = `97.5_quant`), 
                            color = "slategray",
                            linewidth = 1.5,
                            position = ggplot2::position_dodge2(width = 1)) +
    ggplot2::scale_fill_manual(values = c("#353839",
                                          "#CDB5CD")) +
    #ggplot2::ggtitle("All scats combined") +
    ggplot2::ylab("Nb of breeding\nfemales") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12), 
                   axis.text.y = ggplot2::element_text(size = 12), 
                   strip.text.x = ggplot2::element_text(size = 13), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 13, 
                                                        face = "bold"),
                   legend.position = "none")
  ggplot2::ggsave("output/tot_pop_counts_sites.jpg",
                  scale = 1,
                  height = 2, width = 2.5
  )
  
  
}


