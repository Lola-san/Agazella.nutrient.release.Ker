################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# # February 2024
# 0.2.scats_compo_sites.R
#
################################################################################



################# ABSOLUTE CONCENTRATIONS ######################################
#'
#'
#'
#'
#'
# function to display boxplot of elemental composition per site
boxplot_violon_compo_scats_site <- function(scat_compo_tib) {
  
  # to plot both mean and median on the final plots
  mean_median_tib <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    dplyr::group_by( Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
  
  
  scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap\nNoir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe\nSuzanne"), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    dplyr::mutate(site = factor(site, 
                                levels = c("Cap\nNoir", 
                                           "Pointe\nSuzanne"))) |>
    ggplot2::ggplot(ggplot2::aes(x = site, y = concentration_mg_kg_dw, 
                                 fill = site)) +
    ggplot2::geom_violin(ggplot2::aes(color = site),
                         width = 1.4, alpha = 0.5) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::stat_summary(fun.y = mean, geom = "errorbar", 
                          ggplot2::aes(ymax = ..y.., ymin = ..y..),
                          width = .75, linetype = "dashed") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_color_manual(values = c("#353839", "#AE93BEFF")) +
    ggplot2::scale_fill_manual(values = c("#353839", "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", 
                        ncol = 4) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/exploration and intermediary results/boxplot_violon_scat_conc_sites.jpg",
                  scale = 1,
                  height = 9, width = 10
  )
  
}


# function to display boxplot of elemental composition per site
boxplot_compo_scats_site <- function(scat_compo_tib) {
  
  # to plot both mean and median on the final plots
  mean_median_tib <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    dplyr::group_by( Nutrient) |>
    dplyr::summarise(mean = mean(concentration_mg_kg_dw), 
                     median = median(concentration_mg_kg_dw))
  
  
  scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap\nNoir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe\nSuzanne"), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    dplyr::mutate(site = factor(site, 
                                levels = c("Cap\nNoir", 
                                           "Pointe\nSuzanne"))) |>
    ggplot2::ggplot(ggplot2::aes(x = site, y = concentration_mg_kg_dw, 
                                 fill = site)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = median), 
                        linetype = "solid", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_median_tib, 
                        ggplot2::aes(yintercept = mean), 
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::stat_summary(fun.y = mean, geom = "errorbar", 
                          ggplot2::aes(ymax = ..y.., ymin = ..y..),
                          width = .75, linetype = "dashed") +
    ggplot2::ylab("Nutrient concentration (in mg/kg dry weight)") +
    ggplot2::scale_fill_manual(values = c("#353839", "#AE93BEFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free", 
                        ncol = 4) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 16),
                   axis.title.y = ggplot2::element_text(size = 17, 
                                                        face = "bold"), 
                   strip.text.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "none")
  ggplot2::ggsave("output/exploration and intermediary results/boxplot_only_scat_conc_sites.jpg",
                  scale = 1,
                  height = 9, width = 10
  )
  
}



#'
#'
#'
#'
#'
# function to produce table with summary of elemental analysis
table_compo_scats_site <- function(scat_compo_tib
) {
  
  table_summary <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_kg_dw") |>
    dplyr::mutate(Site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     mean = round(mean(concentration_mg_kg_dw), 3),
                     median = round(median(concentration_mg_kg_dw), 3),
                     sd = round(sd(concentration_mg_kg_dw), 3),
                     cv = round(sd/mean, 3),
                     min = round(min(concentration_mg_kg_dw), 3),
                     max = round(max(concentration_mg_kg_dw), 3),
                     max_divided_by_min = round(max/min, 0)) |>
    tidyr::pivot_longer(cols = c(mean, median, sd, cv, min, max, 
                                 max_divided_by_min), 
                        names_to = "variable", 
                        values_to = "conc_mg_kg_dw") |>
    tidyr::pivot_wider(names_from = Nutrient,  
                       values_from = conc_mg_kg_dw) |>
    dplyr::select(c(Site, n, variable, 
                    Ca, P, Mg, Na, K, 
                    Fe, Zn, Sr, Cu, Mn, Se,
                    Ni, Cd, V, Cr, As, Co, 
                    Ag, Mo, Pb)) |>
    dplyr::bind_rows(scat_compo_tib |>
                       tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                                    Fe, Zn, Sr, Cu, Mn, Se,
                                                    Ni, Cd, V, Cr, As, Co, 
                                                    Ag, Mo, Pb), 
                                           names_to = "Nutrient", 
                                           values_to = "concentration_mg_kg_dw") |>
                       dplyr::mutate(Site = "All scats together") |>
                       dplyr::group_by(Site, Nutrient) |>
                       dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                                        mean = round(mean(concentration_mg_kg_dw), 3),
                                        median = round(median(concentration_mg_kg_dw), 3),
                                        sd = round(sd(concentration_mg_kg_dw), 3),
                                        cv = round(sd/mean, 3),
                                        min = round(min(concentration_mg_kg_dw), 3),
                                        max = round(max(concentration_mg_kg_dw), 3),
                                        max_divided_by_min = round(max/min, 0)) |>
                       tidyr::pivot_longer(cols = c(mean, median, sd, cv, min, max, 
                                                    max_divided_by_min), 
                                           names_to = "variable", 
                                           values_to = "conc_mg_kg_dw") |>
                       tidyr::pivot_wider(names_from = Nutrient,  
                                          values_from = conc_mg_kg_dw) |>
                       dplyr::select(c(Site, n, variable, 
                                       Ca, P, Mg, Na, K, 
                                       Fe, Zn, Sr, Cu, Mn, Se,
                                       Ni, Cd, V, Cr, As, Co, 
                                       Ag, Mo, Pb)))
  
  openxlsx::write.xlsx(table_summary, 
                       file = paste0("output/figures-tables-article/table_summary_sites_scat_compo_data.xlsx"))
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between sites 
MWtest_scats_compo_sites <- function(scat_compo_tib) {
  
  scat_compo_tib <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne"),
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) 
  
  nut_vec <- unique(scat_compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- scat_compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = site, 
                         values_from = conc_mg_kg_dw)
    
    CapNo <- na.omit(table$`Cap Noir`)
    PSuz <- na.omit(table$`Pointe Suzanne`)
    
    nut_test <- data.frame(Nutrient = nut,  
                           alpha_MW = wilcox.test(CapNo, PSuz)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,]
  
  df_test <- df_test |>
    dplyr::mutate(significant = dplyr::case_when(alpha_MW <= 0.05 ~ "yes", 
                                                 TRUE ~ "no"))
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/Mann_Whitney_test_scats_sites.xlsx")
  
}


############################ RELATIVE COMPOSITION ##############################


#'
#'
#'
#'
#'
# 
mean_scat_compo_relative_sites <- function(scat_compo_tib) {
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(mean = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative concentration in one scat") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/exploration and intermediary results/mean_scat_compo_rel_sites.jpg",
                  scale = 1,
                  height = 6, width = 9
  )
  
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb")))  |>
    dplyr::group_by(Nutrient) |>
    dplyr::summarize(mean = mean(conc_relative),
                     `10_quant` = quantile(conc_relative, probs = c(0.1)),
                     `80_quant` = quantile(conc_relative, probs = c(0.8))
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean), 
                      stat = "identity") +
    ggplot2::geom_linerange(ggplot2::aes(x = Nutrient, 
                                         ymin = `10_quant`, 
                                         ymax = `80_quant`)) +
    ggplot2::ylab("Relative concentration in one scat") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  
}


#'
#'
#'
#'
#'
# 
barplot_nut_scat_compo_relative_sites <- function(scat_compo_tib) {
  
  # comparison between sites
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb")), 
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::group_by(site, Nutrient) |>
    dplyr::summarise(mean_conc_relative = mean(conc_relative)) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Nutrient, y = mean_conc_relative), 
                      stat = "identity", 
                      position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ site) + 
    ggplot2::ylab("Relative proportion\nin scats") +
    ggplot2::xlab("Nutrient") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14), 
                   axis.text.y = ggplot2::element_text(size = 14), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/exploration and intermediary results/scat_compo_rel_comp_sites_Agazella.jpg",
                  scale = 1,
                  height = 3, width = 8
  )
  

  # Site per site, stacked barplot with nutrients separated depending on 
  # relative concentration values and ordered based on Fe relative conc. 
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb")), 
                  micro_macro = factor(dplyr::case_when(
                    Nutrient %in% c("Ca", "P", "Mg", "Na", "K") ~ "Ca, P, Mg, Na, K", 
                    Nutrient %in% c("Fe", "Zn", "Sr") ~ "Fe, Zn, Sr", 
                    Nutrient %in% c("Cu", "Mn", "Se", "Ni") ~ "Cu, Mn, Se, Ni",  
                    Nutrient %in% c("V", "As", "Co", "Mo") ~ "V, As, Co, Mo",  
                    Nutrient %in% c("Cd", "Cr", "Ag", "Pb") ~ "Cd, Cr, Ag, Pb"), 
                    levels = c("Ca, P, Mg, Na, K", 
                               "Fe, Zn, Sr", 
                               "Cu, Mn, Se, Ni", 
                               "V, As, Co, Mo", 
                               "Cd, Cr, Ag, Pb")),
                  ylim = dplyr::case_when(micro_macro == "Ca, P, Mg, Na, K" ~ 1,
                                          micro_macro == "Fe, Zn, Sr" ~ 0.3,
                                          micro_macro == "Cu, Mn, Se, Ni" ~ 0.03,
                                          micro_macro == "V, As, Co, Mo" ~ 1e-3,
                                          micro_macro == "Cd, Cr, Ag, Pb" ~ 8e-4),
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    # order of samples determined in work-in-progress/set-up-the-rest.R
    dplyr::mutate(Code_sample = factor(Code_sample, 
                                       levels = c("CN25", "CN19", "CN05",
                                                  "CN04", "CN15", "CN07",
                                                  "CN09", "CN03", "CN17",
                                                  "CN08", "CN26", "CN23",
                                                  "CN06", "CN02", "CN16",
                                                  "CN27", "CN10", "CN13",
                                                  "CN22", "CN21", "CN28",
                                                  "CN29", "CN24", "CN11",
                                                  "CN20", "CN18", "CN14",
                                                  "CN01"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Code_sample, y = conc_relative, 
                                   fill = Nutrient), 
                      stat = "identity", 
                      position = ggplot2::position_stack()) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#D98594FF", "#26432FFF",
                                          "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                          "#2e276a", "#5c4d73", "#D8AF39FF",
                                          "#583B2BFF", "#14191FFF")) +
    ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
                        ncol = 1,
                        strip.position = "left") +
    ggplot2::geom_blank(ggplot2::aes(y = ylim)) +
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Scat sample") +
    #ggplot2::ylim(c(0, 0.065)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_blank(), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.y = ggplot2::element_text(size = 9, 
                                                        color = "white"),
                   strip.background = ggplot2::element_rect(fill = "gray30"),
                   strip.placement = "outside", 
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/figures-tables-article/scat_compo_rel_micro-macro_comp_CNoir_Agazella_stack_Feorder.jpg",
                  scale = 1,
                  height = 7, width = 6
  )
  ggplot2::ggsave("output/figures-tables-article/scat_compo_rel_micro-macro_comp_CNoir_Agazella_stack_Feorder.svg",
                  scale = 1,
                  height = 7, width = 6
  )
  
  scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "V", "As", "Co", "Mo", 
                                               "Cd", "Cr", "Ag", "Pb")), 
                  micro_macro = factor(dplyr::case_when(
                    Nutrient %in% c("Ca", "P", "Mg", "Na", "K") ~ "Ca, P, Mg, Na, K", 
                    Nutrient %in% c("Fe", "Zn", "Sr") ~ "Fe, Zn, Sr", 
                    Nutrient %in% c("Cu", "Mn", "Se", "Ni") ~ "Cu, Mn, Se, Ni",  
                    Nutrient %in% c("V", "As", "Co", "Mo") ~ "V, As, Co, Mo",  
                    Nutrient %in% c("Cd", "Cr", "Ag", "Pb") ~ "Cd, Cr, Ag, Pb"), 
                    levels = c("Ca, P, Mg, Na, K", 
                               "Fe, Zn, Sr", 
                               "Cu, Mn, Se, Ni", 
                               "V, As, Co, Mo", 
                               "Cd, Cr, Ag, Pb")),
                  ylim = dplyr::case_when(micro_macro == "Ca, P, Mg, Na, K" ~ 1,
                                          micro_macro == "Fe, Zn, Sr" ~ 0.3,
                                          micro_macro == "Cu, Mn, Se, Ni" ~ 0.03,
                                          micro_macro == "V, As, Co, Mo" ~ 1e-3,
                                          micro_macro == "Cd, Cr, Ag, Pb" ~ 8e-4),
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz") |>
    # order of samples determined in work-in-progress/set-up-the-rest.R
    dplyr::mutate(Code_sample = factor(Code_sample, 
                                       levels = c("PS09", "PS13", "PS23",
                                                  "PS30", "PS16", "PS15",
                                                  "PS05", "PS04", "PS06",
                                                  "PS14", "PS19", "PS29",
                                                  "PS26", "PS31", "PS02",
                                                  "PS03", "PS18", "PS27",
                                                  "PS22", "PS21", "PS20",
                                                  "PS11", "PS24", "PS28",
                                                  "PS12", "PS10", "PS01",
                                                  "PS08", "PS07", "PS17",
                                                  "PS25"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Code_sample, y = conc_relative, 
                                   fill = Nutrient), 
                      stat = "identity", 
                      position = ggplot2::position_stack()) +
    ggplot2::scale_fill_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#D98594FF", "#26432FFF",
                                          "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                          "#2e276a", "#5c4d73", "#D8AF39FF",
                                          "#583B2BFF", "#14191FFF")) +
    ggplot2::facet_wrap(~ micro_macro, scales = "free_y", 
                        ncol = 1, 
                        strip.position = "left") +
    ggplot2::geom_blank(ggplot2::aes(y = ylim)) +
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Scat sample") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_blank(), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.y = ggplot2::element_text(size = 9, 
                                                        color = "white"),
                   strip.background = ggplot2::element_rect(fill = "gray30"),
                   strip.placement = "outside", 
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/figures-tables-article/scat_compo_rel_micro-macro_comp_PSuz_Agazella_stack_Feorder.jpg",
                  scale = 1,
                  height = 7, width = 6
  )
  ggplot2::ggsave("output/figures-tables-article/scat_compo_rel_micro-macro_comp_PSuz_Agazella_stack_Feorder.svg",
                  scale = 1,
                  height = 7, width = 6
  )
  
  
  }


#'
#'
#'
#'
#'
# function to produce table with summary of elemental analysis
table_compo_relative_scats_site <- function(scat_compo_tib
) {
  
  table_summary <- scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb")),
                  Site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::group_by(Site, Nutrient) |>
    dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                     mean = round(mean(conc_relative), 7),
                     median = round(median(conc_relative), 7),
                     sd = round(sd(conc_relative), 7),
                     cv = round(sd/mean, 7),
                     min = round(min(conc_relative), 7),
                     max = round(max(conc_relative), 7)) |>
    tidyr::pivot_longer(cols = c(mean, median, sd, cv, min, max), 
                        names_to = "variable", 
                        values_to = "relative_fraction") |>
    tidyr::pivot_wider(names_from = Nutrient,  
                       values_from = relative_fraction) |>
    dplyr::select(c(Site, n, variable, 
                    Ca, P, Mg, Na, K, 
                    Fe, Zn, Sr, Cu, Mn, Se,
                    Ni, Cd, V, Cr, As, Co, 
                    Ag, Mo, Pb)) |>
    dplyr::bind_rows(scat_compo_tib |>
                       dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                                       Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                                       Ag + Pb + Mo) |>
                       tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                                    Fe, Zn, Sr, Cu, Mn, Se,
                                                    Ni, Cd, V, Cr, As, Co, 
                                                    Ag, Mo, Pb), 
                                           names_to = "Nutrient", 
                                           values_to = "conc_mg_kg_dw") |> 
                       dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                                     Nutrient = factor(Nutrient, 
                                                       levels = c("Ca", "P", "Mg", "Na", "K", 
                                                                  "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                                  "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                                  "Ag", "Mo", "Pb")),
                                     Site = "All scats together") |>
                       dplyr::group_by(Site, Nutrient) |>
                       dplyr::summarise(n = dplyr::n_distinct(Code_sample), 
                                        mean = round(mean(conc_relative), 7),
                                        median = round(median(conc_relative), 7),
                                        sd = round(sd(conc_relative), 7),
                                        cv = round(sd/mean, 7),
                                        min = round(min(conc_relative), 7),
                                        max = round(max(conc_relative), 7)) |>
                       tidyr::pivot_longer(cols = c(mean, median, sd, cv, min, max), 
                                           names_to = "variable", 
                                           values_to = "conc_relative") |>
                       tidyr::pivot_wider(names_from = Nutrient,  
                                          values_from = conc_relative) |>
                       dplyr::select(c(Site, n, variable, 
                                       Ca, P, Mg, Na, K, 
                                       Fe, Zn, Sr, Cu, Mn, Se,
                                       Ni, Cd, V, Cr, As, Co, 
                                       Ag, Mo, Pb)))
  
  openxlsx::write.xlsx(table_summary, 
                       file = paste0("output/exploration and intermediary results/table_summary_sites_scat_compo_relative_data.xlsx"))
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentrations in scats between sites 
MWtest_scats_compo_relative_sites <- function(scat_compo_tib) {
  
  scat_compo_tib <- scat_compo_tib |>
    dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Sr + Cu + Mn + Se + Ni + Cd + V + Cr + As + Co +
                    Ag + Pb + Mo) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb")),
                  conc_relative = conc_mg_kg_dw/sum_nut,
                  site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne"))
  
  nut_vec <- unique(scat_compo_tib$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table <- scat_compo_tib |>
      dplyr::filter(Nutrient == nut) |>
      tidyr::pivot_wider(names_from = site, 
                         values_from = conc_relative)
    
    CapNo <- na.omit(table$`Cap Noir`)
    PSuz <- na.omit(table$`Pointe Suzanne`)
    
    nut_test <- data.frame(Nutrient = nut,  
                           alpha_MW = wilcox.test(CapNo, PSuz)[[3]])
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Nutrient = NA, 
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,]
  
  df_test <- df_test |>
    dplyr::mutate(significant = dplyr::case_when(alpha_MW <= 0.05 ~ "yes", 
                                                 TRUE ~ "no"))
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/exploration and intermediary results/Mann_Whitney_test_scats_compo_relative_sites.xlsx")
  
}



#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of scats
corr_compo_scats <- function(scat_compo_tib) {
  
  corr_mat_CN <- robCompositions::corCoDa(
    as.data.frame(scat_compo_tib |>
                    dplyr::filter(stringr::str_detect(Code_sample, "CN")) |>
                    dplyr::select(c(Ca, P, Mg, Na, K, 
                                    Fe, Zn, Sr, Cu, Mn, Se,
                                    Ni, Cd, V, Cr, As, Co, 
                                    Ag, Mo, Pb)))) 
  
  colnames(corr_mat_CN) <- rownames(corr_mat_CN) <- c("Ca", "P", "Mg", "Na", "K", 
                                                "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                "Ag", "Mo", "Pb")
  
  corr_mat_PS <- robCompositions::corCoDa(
    as.data.frame(scat_compo_tib |>
                    dplyr::filter(stringr::str_detect(Code_sample, "PS")) |>
                    dplyr::select(c(Ca, P, Mg, Na, K, 
                                    Fe, Zn, Sr, Cu, Mn, Se,
                                    Ni, Cd, V, Cr, As, Co, 
                                    Ag, Mo, Pb)))) 
  
  colnames(corr_mat_PS) <- rownames(corr_mat_PS) <- c("Ca", "P", "Mg", "Na", "K", 
                                                      "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                      "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                      "Ag", "Mo", "Pb")
  
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat_CN <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat_CN), 
                                                    na.rm = TRUE)) 
  
  melted_cormat_PS <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat_PS), 
                                                       na.rm = TRUE))
  
  # Corrplot Cap Noir
  ggplot2::ggplot(data = melted_cormat_CN, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                  high = "#E75B64FF", 
                                  mid = "white", 
                                  midpoint = 0, limit = c(-1,1),
                                  name = "Correlation\ncoefficient") +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "right", 
                   legend.title = ggplot2::element_text(size = 13), 
                   legend.text = ggplot2::element_text(size = 11), 
                   legend.title.align = 0)
  ggplot2::ggsave("output/exploration and intermediary results/covar_mat_CapNo.jpg",
                  scale = 1,
                  height = 5, width = 7.5)
  
  
  # Corrplot Pointe Suzanne
  ggplot2::ggplot(data = melted_cormat_PS, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                  high = "#E75B64FF", 
                                  mid = "white", 
                                  midpoint = 0, limit = c(-1,1),
                                  name = "Correlation\ncoefficient") +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "right", 
                   legend.title = ggplot2::element_text(size = 13), 
                   legend.text = ggplot2::element_text(size = 11), 
                   legend.title.align = 0)
  ggplot2::ggsave("output/exploration and intermediary results/covar_mat_PointeSuz.jpg",
                  scale = 1,
                  height = 5, width = 7.5)
  
}


#'
#'
#'
#'
#'
# change in nutrient covariation between fish and scats 
covar_diff_sites <- function(scat_compo_tib) {

  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "CN")) 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "PS")) 
  
  corr_mat_CN <- robCompositions::corCoDa(
    as.data.frame(scat_compo_tib_CN |>
                    dplyr::select(c(Ca, P, Mg, Na, K, 
                                    Fe, Zn, Sr, Cu, Mn, Se,
                                    Ni, Cd, V, Cr, As, Co, 
                                    Ag, Mo, Pb)))) 
  
  colnames(corr_mat_CN) <- rownames(corr_mat_CN) <- c("Ca", "P", "Mg", "Na", "K", 
                                                      "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                      "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                      "Ag", "Mo", "Pb")
  
  corr_mat_CN_012 <- tibble::as_tibble(corr_mat_CN) |>
    dplyr::mutate(Ca = dplyr::case_when(Ca > 0.25 ~ 2, 
                                        Ca <= 0.25 & Ca >= -0.25 ~ 1, 
                                        Ca < -0.25 ~ 0), 
                  P = dplyr::case_when(P > 0.25 ~ 2, 
                                       P <= 0.25 & P >= -0.25 ~ 1, 
                                       P < -0.25 ~ 0), 
                  Mg = dplyr::case_when(Mg > 0.25 ~ 2, 
                                        Mg <= 0.25 & Mg >= -0.25 ~ 1, 
                                        Mg < -0.25 ~ 0), 
                  Na = dplyr::case_when(Na > 0.25 ~ 2, 
                                        Na <= 0.25 & Na >= -0.25 ~ 1, 
                                        Na < -0.25 ~ 0), 
                  K = dplyr::case_when(K > 0.25 ~ 2, 
                                       K <= 0.25 & K >= -0.25 ~ 1, 
                                       K < -0.25 ~ 0), 
                  Fe = dplyr::case_when(Fe > 0.25 ~ 2, 
                                        Fe <= 0.25 & Fe >= -0.25 ~ 1, 
                                        Fe < -0.25 ~ 0), 
                  Zn = dplyr::case_when(Zn > 0.25 ~ 2, 
                                        Zn <= 0.25 & Zn >= -0.25 ~ 1, 
                                        Zn < -0.25 ~ 0), 
                  Sr = dplyr::case_when(Sr > 0.25 ~ 2, 
                                        Sr <= 0.25 & Sr >= -0.25 ~ 1, 
                                        Sr < -0.25 ~ 0), 
                  Cu = dplyr::case_when(Cu > 0.25 ~ 2, 
                                        Cu <= 0.25 & Cu >= -0.25 ~ 1, 
                                        Cu < -0.25 ~ 0), 
                  Mn = dplyr::case_when(Mn > 0.25 ~ 2, 
                                        Mn <= 0.25 & Mn >= -0.25 ~ 1, 
                                        Mn < -0.25 ~ 0), 
                  Se = dplyr::case_when(Se > 0.25 ~ 2, 
                                        Se <= 0.25 & Se >= -0.25 ~ 1, 
                                        Se < -0.25 ~ 0), 
                  Ni = dplyr::case_when(Ni > 0.25 ~ 2, 
                                        Ni <= 0.25 & Ni >= -0.25 ~ 1, 
                                        Ni < -0.25 ~ 0), 
                  Cd = dplyr::case_when(Cd > 0.25 ~ 2, 
                                        Cd <= 0.25 & Cd >= -0.25 ~ 1, 
                                        Cd < -0.25 ~ 0),
                  V = dplyr::case_when(V > 0.25 ~ 2, 
                                       V <= 0.25 & V >= -0.25 ~ 1, 
                                       V < -0.25 ~ 0),
                  Cr = dplyr::case_when(Cr > 0.25 ~ 2, 
                                        Cr <= 0.25 & Cr >= -0.25 ~ 1, 
                                        Cr < -0.25 ~ 0), 
                  As = dplyr::case_when(As > 0.25 ~ 2, 
                                        As <= 0.25 & As >= -0.25 ~ 1, 
                                        As < -0.25 ~ 0), 
                  Co = dplyr::case_when(Co > 0.25 ~ 2, 
                                        Co <= 0.25 & Co >= -0.25 ~ 1, 
                                        Co < -0.25 ~ 0), 
                  Ag = dplyr::case_when(Ag > 0.25 ~ 2, 
                                        Ag <= 0.25 & Ag >= -0.25 ~ 1, 
                                        Ag < -0.25 ~ 0), 
                  Pb = dplyr::case_when(Pb > 0.25 ~ 2, 
                                        Pb <= 0.25 & Pb >= -0.25 ~ 1, 
                                        Pb < -0.25 ~ 0), 
                  Mo = dplyr::case_when(Mo > 0.25 ~ 2, 
                                        Mo <= 0.25 & Mo >= -0.25 ~ 1, 
                                        Mo < -0.25 ~ 0), 
                  Nutrient2 = c("Ca", "P", "Mg", "Na", "K", 
                                "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                "Ni", "Cd", "V", "Cr", "As", "Co", 
                                "Ag", "Mo", "Pb"))
  
  corr_mat_PS <- robCompositions::corCoDa(
    as.data.frame(scat_compo_tib_PS |>
                    dplyr::select(c(Ca, P, Mg, Na, K, 
                                    Fe, Zn, Sr, Cu, Mn, Se,
                                    Ni, Cd, V, Cr, As, Co, 
                                    Ag, Mo, Pb)))) 
  
  colnames(corr_mat_PS) <- rownames(corr_mat_PS) <- c("Ca", "P", "Mg", "Na", "K", 
                                                      "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                      "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                      "Ag", "Mo", "Pb")
  
  corr_mat_PS_012 <- tibble::as_tibble(corr_mat_PS) |>
    dplyr::mutate(Ca = dplyr::case_when(Ca > 0.25 ~ 2, 
                                        Ca <= 0.25 & Ca >= -0.25 ~ 1, 
                                        Ca < -0.25 ~ 0), 
                  P = dplyr::case_when(P > 0.25 ~ 2, 
                                       P <= 0.25 & P >= -0.25 ~ 1, 
                                       P < -0.25 ~ 0), 
                  Mg = dplyr::case_when(Mg > 0.25 ~ 2, 
                                        Mg <= 0.25 & Mg >= -0.25 ~ 1, 
                                        Mg < -0.25 ~ 0), 
                  Na = dplyr::case_when(Na > 0.25 ~ 2, 
                                        Na <= 0.25 & Na >= -0.25 ~ 1, 
                                        Na < -0.25 ~ 0), 
                  K = dplyr::case_when(K > 0.25 ~ 2, 
                                       K <= 0.25 & K >= -0.25 ~ 1, 
                                       K < -0.25 ~ 0), 
                  Fe = dplyr::case_when(Fe > 0.25 ~ 2, 
                                        Fe <= 0.25 & Fe >= -0.25 ~ 1, 
                                        Fe < -0.25 ~ 0), 
                  Zn = dplyr::case_when(Zn > 0.25 ~ 2, 
                                        Zn <= 0.25 & Zn >= -0.25 ~ 1, 
                                        Zn < -0.25 ~ 0), 
                  Sr = dplyr::case_when(Sr > 0.25 ~ 2, 
                                        Sr <= 0.25 & Sr >= -0.25 ~ 1, 
                                        Sr < -0.25 ~ 0), 
                  Cu = dplyr::case_when(Cu > 0.25 ~ 2, 
                                        Cu <= 0.25 & Cu >= -0.25 ~ 1, 
                                        Cu < -0.25 ~ 0), 
                  Mn = dplyr::case_when(Mn > 0.25 ~ 2, 
                                        Mn <= 0.25 & Mn >= -0.25 ~ 1, 
                                        Mn < -0.25 ~ 0), 
                  Se = dplyr::case_when(Se > 0.25 ~ 2, 
                                        Se <= 0.25 & Se >= -0.25 ~ 1, 
                                        Se < -0.25 ~ 0), 
                  Ni = dplyr::case_when(Ni > 0.25 ~ 2, 
                                        Ni <= 0.25 & Ni >= -0.25 ~ 1, 
                                        Ni < -0.25 ~ 0), 
                  Cd = dplyr::case_when(Cd > 0.25 ~ 2, 
                                        Cd <= 0.25 & Cd >= -0.25 ~ 1, 
                                        Cd < -0.25 ~ 0),
                  V = dplyr::case_when(V > 0.25 ~ 2, 
                                       V <= 0.25 & V >= -0.25 ~ 1, 
                                       V < -0.25 ~ 0),
                  Cr = dplyr::case_when(Cr > 0.25 ~ 2, 
                                        Cr <= 0.25 & Cr >= -0.25 ~ 1, 
                                        Cr < -0.25 ~ 0), 
                  As = dplyr::case_when(As > 0.25 ~ 2, 
                                        As <= 0.25 & As >= -0.25 ~ 1, 
                                        As < -0.25 ~ 0), 
                  Co = dplyr::case_when(Co > 0.25 ~ 2, 
                                        Co <= 0.25 & Co >= -0.25 ~ 1, 
                                        Co < -0.25 ~ 0), 
                  Ag = dplyr::case_when(Ag > 0.25 ~ 2, 
                                        Ag <= 0.25 & Ag >= -0.25 ~ 1, 
                                        Ag < -0.25 ~ 0), 
                  Pb = dplyr::case_when(Pb > 0.25 ~ 2, 
                                        Pb <= 0.25 & Pb >= -0.25 ~ 1, 
                                        Pb < -0.25 ~ 0), 
                  Mo = dplyr::case_when(Mo > 0.25 ~ 2, 
                                        Mo <= 0.25 & Mo >= -0.25 ~ 1, 
                                        Mo < -0.25 ~ 0), 
                  Nutrient2 = c("Ca", "P", "Mg", "Na", "K", 
                                "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                "Ni", "Cd", "V", "Cr", "As", "Co", 
                                "Ag", "Mo", "Pb"))
  
  
  # compare the two correlation matrix
  corr_mat_CN_012_long <- corr_mat_PS_012 |>
    tidyr::pivot_longer(cols = c(Ca:Pb), 
                        names_to = "Nutrient1", 
                        values_to = "corr_factor") |>
    dplyr::mutate(site = "CN")
  
  corr_mat_PS_012_long <- corr_mat_CN_012 |>
    tidyr::pivot_longer(cols = c(Ca:Pb), 
                        names_to = "Nutrient1", 
                        values_to = "corr_factor") |>
    dplyr::mutate(site = "PS") 
  
  corr_mat_012_pooled <- corr_mat_CN_012_long |>
    dplyr::bind_rows(corr_mat_PS_012_long) |>
    tidyr::pivot_wider(names_from = site, 
                       values_from = corr_factor) |>
    dplyr::mutate(change = dplyr::case_when(CN == PS ~ 0, 
                                            TRUE ~ 1))
  
  corr_mat_change <- corr_mat_012_pooled |>
    dplyr::select(-c(CN, PS)) |>
    tidyr::pivot_wider(names_from = Nutrient1, 
                       values_from = change) |>
    as.data.frame() 
  
  rownames(corr_mat_change) <- colnames(corr_mat_change)[2:21]
  
  corr_mat_change <- corr_mat_change |>
    dplyr::select(-Nutrient2)
  
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  melted_cormat <- tibble::as_tibble(reshape2::melt(as.matrix(get_lower_tri(corr_mat_change)), 
                                                    na.rm = TRUE)) |>
    dplyr::mutate(value = factor(value))
  
  total_changes_p <- tibble::as_tibble(reshape2::melt(as.matrix(get_lower_tri(corr_mat_change)), 
                                                      na.rm = TRUE)) |>
    dplyr::summarise(percent_change = round(100*(sum(value)/210), 1)) |>
    purrr::pluck("percent_change")
  
  ggplot2::ggplot(data = melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_manual(values = c("lightgrey",
                                          "lightblue"), 
                               name = "Binary\nindicator") +
    ggplot2::theme_bw() + 
    ggplot2::ggtitle("Change in nutrient concentration correlation coefficients\nbetween scats from Cap Noir and Pointe Suzanne") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 16.5, 
                                                      face = "bold", 
                                                      hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 15.5), 
                   axis.text.y = ggplot2::element_text(size = 15.5), 
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_blank(), 
                   legend.position = "right", 
                   legend.title = ggplot2::element_text(size = 13.5), 
                   legend.text = ggplot2::element_text(size = 11.5), 
                   legend.title.align = 0)
  ggplot2::ggsave("output/exploration and intermediary results/corrplot_diff_sites.jpg",
                  scale = 1,
                  height = 5, width = 7.5
  )
  
}


#'
#'
#'
#'
# is it likely than some nutrients have leached out of scats before analysis
# because they were old (and therefore, dehydrated) ?
plot_concentration_vs_water_content <- function(scat_data, 
                                                scat_compo_tib) {
  
  scat_compo_tib |> 
    dplyr::left_join(scat_data |>
                       dplyr::filter(Code_sample != "CN12") |>
                       dplyr::select(Code_sample, water_percent), by = ("Code_sample")) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Mo", "Pb"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = water_percent, y = conc_mg_kg_dw, 
                                     color = Nutrient)) +
    ggplot2::scale_color_manual(values = c("#F0D77BFF", "#5A6F80FF","#278B9AFF", 
                                          "#B4DAE5FF", "#6FB382FF", "#AE93BEFF",
                                          "#CEC917FF", "#3A160AFF", "#E75B64FF",
                                          "#92BBD9FF", "#D98594FF", "#26432FFF",
                                          "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                          "#2e276a", "#5c4d73", "#D8AF39FF",
                                          "#583B2BFF", "#14191FFF")) +
    ggplot2::facet_wrap(~ Nutrient, scales = "free_y") +
    ggplot2::ylab("Relative fraction") +
    ggplot2::xlab("Scat sample") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12), 
                   axis.text.x = ggplot2::element_blank(), 
                   title = ggplot2::element_text(size = 15, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_blank(),
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/exploration and intermediary results/concentration_vs_waterp.jpg",
                  scale = 1,
                  height = 4, width = 10
  )
  
  
}

