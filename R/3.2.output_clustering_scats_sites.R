################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# August 2023
# 03.2_output_clustering_scats_sites.R
#
################################################################################

################################################################################
################ STARTING WITH A PCA TO REDUCE DIMENSIONS ##################
################################################################################


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
boxplot_compo_clust_PCs <- function(res_clust_PCs,
                                    scat_compo_tib
) {
  
  ############################### CAP NOIR #####################################
  # assign each sample to its cluster
  clust_vec_CN <- res_clust_PCs$CN$cluster
  
  mean_conc_table_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |> 
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                     median_conc = median(conc_mg_kg_dw))
  
  scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb")), 
                  y_lim = dplyr::case_when(Nutrient == "Ca" ~ 260000,
                                           Nutrient == "P" ~ 145000,
                                           Nutrient == "Na" ~ 18000,
                                           Nutrient == "K" ~ 9000,
                                           Nutrient == "Mg" ~ 24000,
                                           Nutrient == "Fe" ~ 18000, 
                                           Nutrient == "Zn" ~ 1250, 
                                           Nutrient == "Cu" ~ 1550, 
                                           Nutrient == "Mn" ~ 550, 
                                           Nutrient == "Se" ~ 145, 
                                           Nutrient == "Ni" ~ 85,
                                           Nutrient == "As" ~ 14,
                                           Nutrient == "Co" ~ 15)) |> 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                       fill = factor(cluster)), 
                          position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient,
                        ncol = 4, scales = "free_y") + 
    ggplot2::geom_hline(data = mean_conc_table_CN, 
                        ggplot2::aes(yintercept = mean_conc), 
                        linetype = "dashed",
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_conc_table_CN,
                        ggplot2::aes(yintercept = median_conc), 
                        color = "darkred") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#7EBAC2FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF",
                                          "5" = "#1D2645FF")) +
    ggplot2::ggtitle("Cap Noir") +
    ggplot2::ylab("Absolute concentration in scats\n(mg per kg dry weight)") +
    ggplot2::xlab("Cluster") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/figures-tables-article/clust_scat_compo_PCs_CapNoir.jpg",
                  scale = 1,
                  height = 7.5, width = 9
  )
  
  
  ######################### POINTE SUZANNE #####################################
  
  clust_vec_PS <- res_clust_PCs$PS$cluster
  
  mean_conc_table_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
    dplyr::filter(site == "Pointe Suz") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |> 
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                     median_conc = median(conc_mg_kg_dw))
  
  
  scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
    dplyr::filter(site == "Pointe Suz") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb")), 
                  y_lim = dplyr::case_when(Nutrient == "Ca" ~ 260000,
                                           Nutrient == "P" ~ 145000,
                                           Nutrient == "Na" ~ 18000,
                                           Nutrient == "K" ~ 9000,
                                           Nutrient == "Mg" ~ 24000,
                                           Nutrient == "Fe" ~ 18000, 
                                           Nutrient == "Zn" ~ 1250, 
                                           Nutrient == "Cu" ~ 1550, 
                                           Nutrient == "Mn" ~ 550, 
                                           Nutrient == "Se" ~ 145,
                                           Nutrient == "Ni" ~ 85, 
                                           Nutrient == "As" ~ 14,
                                           Nutrient == "Co" ~ 15)) |> 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                       fill = factor(cluster)), 
                          position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, 
                        ncol = 4, scales = "free_y") + 
    ggplot2::geom_hline(data = mean_conc_table_PS, 
                        ggplot2::aes(yintercept = mean_conc),
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_conc_table_PS,
                        ggplot2::aes(yintercept = median_conc), 
                        color = "darkred") +
    ggplot2::geom_blank(ggplot2::aes(y = y_lim)) +
    ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#7EBAC2FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF",
                                          "5" = "#1D2645FF")) +
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::ylab("Absolute concentration in scats\n(mg per kg dry weight)") +
    ggplot2::xlab("Cluster") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/figures-tables-article/clust_scat_compo_PCs_PSuzanne.jpg",
                  scale = 1,
                  height = 7.5, width = 9
  )
  
  ############################ both sites together ###########################
  # assign each sample to its cluster
  clust_vec <- res_clust_PCs$both$cluster
  
  mean_conc_table <- scat_compo_tib |>
    dplyr::mutate(cluster = clust_vec) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |> 
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                     median_conc = median(conc_mg_kg_dw))
  
  scat_compo_tib |>
    dplyr::mutate(cluster = clust_vec) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                       fill = factor(cluster)), 
                          position = ggplot2::position_dodge(1)) +
    ggplot2::facet_wrap(~ Nutrient, 
                        ncol = 4, scales = "free_y") + 
    ggplot2::geom_hline(data = mean_conc_table, 
                        ggplot2::aes(yintercept = mean_conc), 
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_conc_table,
                        ggplot2::aes(yintercept = median_conc),
                        linetype = "dashed", 
                        color = "darkred") +
    ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#7EBAC2FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF",
                                          "5" = "#1D2645FF")) +
    ggplot2::ggtitle("All scats") +
    ggplot2::ylab("Absolute concentration in scats (mg per kg dry weight)") +
    ggplot2::xlab("Cluster") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/clustering with PCs/clust_scat_compo_PCs_all_scats.jpg",
                  scale = 1,
                  height = 7.5, width = 12
  )
  
}


#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
boxplot_grouped_compo_clust_PCs <- function(res_clust_PCs,
                                            scat_compo_tib
) {
  
  ############################### CAP NOIR #####################################
  # assign each sample to its cluster
  clust_vec_CN <- res_clust_PCs$CN$cluster
  clust_vec_PS <- res_clust_PCs$PS$cluster
  
  mean_conc_table_all <- scat_compo_tib |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |> 
    dplyr::group_by(Nutrient) |>
    dplyr::summarise(mean_conc = mean(conc_mg_kg_dw), 
                     median_conc = median(conc_mg_kg_dw))
  
  scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::mutate(cluster = c(clust_vec_CN, clust_vec_PS)) |> # samples from Cap Noir are first
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb"))) |> 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = factor(cluster), y = conc_mg_kg_dw, 
                                       fill = site), 
                          position = ggplot2::position_dodge(0.7)) +
    ggplot2::facet_wrap(~ Nutrient,
                        ncol = 4, scales = "free_y") + 
    ggplot2::geom_hline(data = mean_conc_table_all, 
                        ggplot2::aes(yintercept = mean_conc), 
                        linetype = "dashed",
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_conc_table_all,
                        ggplot2::aes(yintercept = median_conc), 
                        color = "darkred") +
    ggplot2::scale_fill_manual(values = c("#605F5F",
                                          "#CDB5CD"),
                               name = "Site") +
    ggplot2::ylab("Absolute concentration in scats\n(mg per kg dry weight)") +
    ggplot2::xlab("Cluster") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   legend.text = ggplot2::element_text(size = 13))
  ggplot2::ggsave("output/clustering with PCs/boxplot_clustgrouped_clust_scat_compo_PCs.jpg",
                  scale = 1,
                  height = 7, width = 10
  )
  
  
  scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap\nNoir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe\nSuzanne")) |>
    dplyr::mutate(cluster = c(clust_vec_CN, clust_vec_PS)) |> # samples from Cap Noir are first
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") |> 
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", 
                                               "Cu", "Mn", "Se", "Ni", 
                                               "V", "As", "Co", "Mo",
                                               "Cd", "Cr", "Ag", "Pb")), 
                  y_lim = dplyr::case_when(Nutrient == "Ca" ~ 260000,
                                           Nutrient == "P" ~ 150000,
                                           Nutrient == "Na" ~ 18000,
                                           Nutrient == "K" ~ 9000,
                                           Nutrient == "Mg" ~ 24000,
                                           Nutrient == "Fe" ~ 18000, 
                                           Nutrient == "Zn" ~ 1250, 
                                           Nutrient == "Cu" ~ 1550, 
                                           Nutrient == "Mn" ~ 550, 
                                           Nutrient == "Se" ~ 145,
                                           Nutrient == "Ni" ~ 85, 
                                           Nutrient == "As" ~ 14,
                                           Nutrient == "Co" ~ 15)) |> 
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = site, y = conc_mg_kg_dw, 
                                       fill = factor(cluster)), 
                          position = ggplot2::position_dodge(0.9)) +
    ggplot2::facet_wrap(~ Nutrient,
                        ncol = 4, scales = "free_y") + 
    ggplot2::geom_hline(data = mean_conc_table_all, 
                        ggplot2::aes(yintercept = mean_conc), 
                        linetype = "dashed",
                        color = "darkred") +
    ggplot2::geom_hline(data = mean_conc_table_all,
                        ggplot2::aes(yintercept = median_conc), 
                        color = "darkred") +
    ggplot2::geom_vline(data = mean_conc_table_all,
                        ggplot2::aes(xintercept = 1.5), 
                        color = "black", 
                        linewidth = 0.25) +
    ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#7EBAC2FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF",
                                          "5" = "#1D2645FF"), 
                               name = "Cluster") +
    ggplot2::ylab("Absolute concentration in scats\n(mg per kg dry weight)") +
    ggplot2::xlab("Site") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15, 
                                                        colour = "white"),
                   strip.background = ggplot2::element_rect(fill = "gray30"),
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_text(size = 14, 
                                                        face = "bold"), 
                   legend.text = ggplot2::element_text(size = 13))
  ggplot2::ggsave("output/clustering with PCs/boxplot_sitegrouped_clust_scat_compo_PCs.jpg",
                  scale = 1,
                  height = 8, width = 10
  )
  ggplot2::ggsave("output/clustering with PCs/boxplot_sitegrouped_clust_scat_compo_PCs.svg",
                  scale = 1,
                  height = 8, width = 10
  )
  
 }




#'
#'
#'
#'
#'
# 
barplot_nut_scat_compo_relative_clusters <- function(clust_PCs_output,
                                                     scat_compo_tib) {
  
  #Cap Noir
  clust_vec_CN <- clust_PCs_output$CN$cluster
  
  scat_compo_tib |> 
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir") |>
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec_CN),
                  sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Cu + Mn + Se + Ni + As + Co) |>
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
                                               "Ag", "Mo", "Pb"))) |> 
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
    ggplot2::facet_wrap(~ cluster, ncol = 2) +
    ggplot2::ggtitle("Cap Noir") +
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
                   strip.text.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/clustering with PCs/scat_compo_rel_comp_CapNoir_Agazella_stack_Feorder_clusters.jpg",
                  scale = 1,
                  height = 4, width = 6
  )
  
  #PSuz
  clust_vec_PS <- clust_PCs_output$PS$cluster
  
  scat_compo_tib |> 
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz") |>
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec_PS),
                  sum_nut = P + Ca + Mg + Na + K + Fe + 
                    Zn + Cu + Mn + Se + Ni + As + Co) |>
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
                                               "Ag", "Mo", "Pb"))) |> 
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
    ggplot2::facet_wrap(~ cluster, ncol = 2) +
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
                   strip.text.x = ggplot2::element_text(size = 14, 
                                                        face = "bold"),
                   legend.position = "bottom", 
                   legend.text = ggplot2::element_text(size = 12))
  ggplot2::ggsave("output/clustering with PCs/scat_compo_rel_comp_PSuz_Agazella_stack_Feorder_clusters.jpg",
                  scale = 1,
                  height = 4, width = 6
  )
  
}

#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
table_stats_clust_PCs_per_site <- function(list_res_clust_sites,
                                           scat_compo_tib
) {
  
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  table <- rbind(scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Cap Noir") |>
                   dplyr::mutate(cluster = clust_vec_CN) |>
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
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::reframe(median = round(median(conc_mg_kg_dw), 3),
                                  mean = round(mean(conc_mg_kg_dw), 3),
                                  sd = round(sd(conc_mg_kg_dw), 3), 
                                  min = round(min(conc_mg_kg_dw), 3),
                                  max = round(max(conc_mg_kg_dw), 3), 
                                  max_divided_by_min = round(max/min, 0)) |>
                   tidyr::pivot_longer(cols = c(median, mean, sd, min, max, 
                                                max_divided_by_min), 
                                       names_to = "statistic", 
                                       values_to = "value") |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = value), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS) |>
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
                   dplyr::group_by(site, cluster, Nutrient) |>
                   dplyr::summarise(median = round(median(conc_mg_kg_dw), 3),
                                    mean = round(mean(conc_mg_kg_dw), 3), 
                                    sd = round(sd(conc_mg_kg_dw), 3), 
                                    min = round(min(conc_mg_kg_dw), 3),
                                    max = round(max(conc_mg_kg_dw), 3), 
                                    max_divided_by_min = round(max/min, 0)) |>
                   tidyr::pivot_longer(cols = c(median, mean, sd, min, max, 
                                                max_divided_by_min), 
                                       names_to = "statistic", 
                                       values_to = "value") |>
                   tidyr::pivot_wider(names_from = Nutrient, 
                                      values_from = value))
  
  openxlsx::write.xlsx(table, 
                       file = "output/figures-tables-article/clust_PCs_compo_sites.xlsx")
  
  table 
  
}

#'
#'
#'
#'
# function to show elemental composition of samples from the different clusters
table_compo_clust_PCs_percent_per_site <- function(list_res_clust_sites,
                                               scat_compo_tib
) {
  
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  table <- rbind(scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Cap Noir") |>
                   dplyr::mutate(cluster = clust_vec_CN, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   dplyr::group_by(site, cluster) |>
                   dplyr::reframe(n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = 100*(n/ntot)) |>
                   dplyr::distinct(), 
                 scat_compo_tib |>
                   dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                                         stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
                   dplyr::filter(site == "Pointe Suzanne") |>
                   dplyr::mutate(cluster = clust_vec_PS, 
                                 ntot = dplyr::n_distinct(Code_sample)) |>
                   dplyr::group_by(site, cluster) |>
                   dplyr::reframe(n = dplyr::n_distinct(Code_sample), 
                                    clust_ratio = 100*(n/ntot)) |>
                   dplyr::distinct())
  
  openxlsx::write.xlsx(table, 
                       file = "output/figures-tables-article/clust_PCs_percent_sites.xlsx")
  
  table 
  
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k34 <- function(list_res_clust_sites,
                             scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Pointe Suzanne") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  
  nut_vec <- unique(scat_compo_tib_CN$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table_CN <- scat_compo_tib_CN |>
      dplyr::filter(Nutrient == nut)
    
    table_PS <- scat_compo_tib_PS |>
      dplyr::filter(Nutrient == nut)
    
    table_CN$cluster <- factor(table_CN$cluster)
    table_PS$cluster <- factor(table_PS$cluster)
    
    table_CN <- table_CN |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_CN <- na.omit(table_CN$`1`)
    clust2_CN <- na.omit(table_CN$`2`)
    clust3_CN <- na.omit(table_CN$`3`)
    
    table_PS <- table_PS |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_PS <- na.omit(table_PS$`1`)
    clust2_PS <- na.omit(table_PS$`2`)
    clust3_PS <- na.omit(table_PS$`3`)
    clust4_PS <- na.omit(table_PS$`4`)
    
    nut_test <- rbind(data.frame(Site = "Cap Noir",
                                 Nutrient = rep(nut, 3), 
                                 Cluster_comp_1 = c("1", "1", 
                                                    "2"), 
                                 Cluster_comp_2 = c("2", "3",
                                                    "3"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust2_CN)[[3]],
                                              wilcox.test(clust1_CN, clust3_CN)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust3_CN)[[3]])), 
                      data.frame(Site = "Pointe Suzanne",
                                 Nutrient = rep(nut, 6), 
                                 Cluster_comp_1 = c("1", "1", "1", 
                                                    "2", "2", 
                                                    "3"), 
                                 Cluster_comp_2 = c("2", "3", "4",
                                                    "3", "4", 
                                                    "4"), 
                                 alpha_MW = c(wilcox.test(clust1_PS, clust2_PS)[[3]],
                                              wilcox.test(clust1_PS, clust3_PS)[[3]],
                                              wilcox.test(clust1_PS, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust2_PS, clust3_PS)[[3]],
                                              wilcox.test(clust2_PS, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust3_PS, clust4_PS)[[3]])), 
                      data.frame(Site = "both",
                                 Nutrient = rep(nut, 12), 
                                 Cluster_comp_1 = c("1_CN", "1_CN", "1_CN", "1_CN", 
                                                    "2_CN", "2_CN", "2_CN", "2_CN",  
                                                    "3_CN", "3_CN", "3_CN", "3_CN"), 
                                 Cluster_comp_2 = c("1_PS", "2_PS", "3_PS", "4_PS", 
                                                    "1_PS", "2_PS", "3_PS", "4_PS", 
                                                    "1_PS", "2_PS", "3_PS", "4_PS"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust1_PS)[[3]],
                                              wilcox.test(clust1_CN, clust2_PS)[[3]],
                                              wilcox.test(clust1_CN, clust3_PS)[[3]],
                                              wilcox.test(clust1_CN, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust1_PS)[[3]],
                                              wilcox.test(clust2_CN, clust2_PS)[[3]],
                                              wilcox.test(clust2_CN, clust3_PS)[[3]],
                                              wilcox.test(clust2_CN, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust1_PS)[[3]],
                                              wilcox.test(clust3_CN, clust2_PS)[[3]],
                                              wilcox.test(clust3_CN, clust3_PS)[[3]],
                                              wilcox.test(clust3_CN, clust4_PS)[[3]]))
    )
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Site = NA, 
                        Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,] |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = alpha_MW)
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/clustering with PCs/Mann_Whitney_test_clust_PCs_k34_compo_sites.xlsx")
  
  df_test
}



#'
#'
#'
#'
# output matching species/samples with attributed cluster
clust_PCs_samples <- function(clust_PCs_output,
                              scat_compo_tib
) {
  
  #Cap Noir
  clust_vec_CN <- clust_PCs_output$CN$cluster
  
  clust_tib_CN <- scat_compo_tib |> 
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Cap Noir", 
                  # outliers
                  !(Code_sample %in% c("CN12"))) |>
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec_CN)) 
  
  # save 
  openxlsx::write.xlsx(clust_tib_CN, 
                       file = "output/clustering with PCs/clust_attribution_CN.xlsx")
  
  # Pointe Suzanne
  clust_vec_PS <- clust_PCs_output$PS$cluster
  
  clust_tib_PS <- scat_compo_tib |> 
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
    dplyr::filter(site == "Pointe Suz", 
                  # outliers
                  !(Code_sample %in% c("CN12"))) |>
    dplyr::ungroup() |>
    dplyr::mutate(cluster = as.factor(clust_vec_PS)) 
  # save 
  openxlsx::write.xlsx(clust_tib_PS, 
                       file = "output/clustering with PCs/clust_attribution_PS.xlsx")
  }

#'
#'
#'
#'
#'
# function to make biplot with cluster grouping
biplot_after_clust <- function(list_res_pca,
                               list_res_clust_sites,
                               scat_compo_tib,
                               pcomp = c(1:2), # choices of the PC to plot (2 in total), 
                               # 1 and 2 by default
                               circle.prob = 0.69, # not sure yet why this value by default
                               var.add.scaling = 2, # constant to multiply coordinates
                               # of variables by so that they show on a similar scale as 
                               # that of observations # 2 seems to fit ok but could be changed 
                               ellipse.prob = 0.68 # size of the ellipse in Normal probability
                               # not sure yet why this value by default
) {
  
  # function constructed based on ggbiplot function on github 
  # https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r
  # and was adapted to work for an pca CoDa object
  
  res_pca_CN <- list_res_pca$CN
  res_pca_PS <- list_res_pca$PS
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  ###### biplot settings
  # common practice as explained in ?biplot() : 
  # variables are scaled by lambda^scale and observations are scaled by
  # lambda^(1-scale) where lambda are singular values as computed by PCA
  # i.e d below
  scale <- 0
  obs.scale <- 1 - scale
  var.scale <- scale
  
  ################### CAP NOIR FIRST ###########################################
  ##### recover the single value decomposition SVD
  nobs.factor.CN <- sqrt(nrow(res_pca_CN$scores) - 1) # not sure what this is 
  # and what is it for
  
  # standard deviation of the PCs #lambda in ?biplot()
  d.CN <- sqrt(res_pca_CN$eigenvalues)
  
  u.CN <- sweep(res_pca_CN$scores, 2, 1 / (d.CN * nobs.factor.CN), FUN = '*')
  v.CN <- res_pca_CN$loadings
  
  
  #####
  # compute scores 
  # ie coordinates of individuals (observations) on each principal component (PC)
  # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
  df.u.CN <- as.data.frame(sweep(u.CN[,pcomp], 2, d.CN[pcomp]^obs.scale, FUN='*'))
  # scale observations by lambda^(1-scale)
  
  # compute directions 
  # ie coordinates of the variables ie loadings * sdev of PCs
  v.CN <- sweep(v.CN, 2, d.CN^var.scale, FUN='*')
  df.v.CN <- as.data.frame(v.CN[, pcomp])
  
  names(df.u.CN) <- c('xvar', 'yvar')
  names(df.v.CN) <- names(df.u.CN)
  
  df.u.CN <- df.u.CN * nobs.factor.CN # so we are back to the original scores - res_pca$scores
  # ie the coordinates of the individuals on the PCs
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores (as done with ggbiplot)
  r.CN <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u.CN^2))^(1/4) 
  
  # scale directions
  # v^2 = cos2 = quality of representation of variables on each PC 
  v.scale.CN <- rowSums(v.CN^2)
  df.v.CN <- r.CN * df.v.CN / sqrt(max(v.scale.CN))
  # multiply then by another constant to get arrows on the same scale as observations 
  # coordinates
  # as mentioned in 
  # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
  # "it might be necessary to scale arrows by some arbitrary constant factor so 
  # that both arrows and data points appear roughly on the same scale "
  df.v.CN <- var.add.scaling * df.v.CN
  
  # scale scores 
  # as done by 
  # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
  # with r <- 1 
  # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
  # df.u=.99*df.u/r.scale
  # this version was set aside as we are more interested in comparing individuals 
  # and not structuring variables, so we went for an additional scaling 
  # of variables coordinates instead - see above
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')   
  # add explained variance
  u.axis.labs.CN <- paste(u.axis.labs, 
                          sprintf('(%0.1f%% explained var.)', 
                                  100 * res_pca_CN$eigenvalues[pcomp]/sum(res_pca_CN$eigenvalues)))
  
  
  # Score Labels (labels of the observations)
  df.u.CN$labels <- (scat_compo_tib |> 
                       dplyr::filter(stringr::str_detect(Code_sample, "CN")))$Code_sample
  
  # define groups
  # grouping per cluster
  df.u.CN$groups <- as.factor(clust_vec_CN)
  
  # Variable Names
  df.v.CN$varname <- rownames(v.CN)
  
  # Variables for text label placement
  varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
  var.axes <- TRUE # draw arrow for the variable
  df.v.CN$angle <- with(df.v.CN, (180/pi) * atan(yvar / xvar))
  df.v.CN$hjust = with(df.v.CN, (1 - varname.adjust * sign(xvar)) / 2)
  
  
  ############## draw biplot
  # Base plot
  g <- ggplot2::ggplot(data = df.u.CN, ggplot2::aes(x = xvar, y = yvar)) + 
    ggplot2::xlab(u.axis.labs.CN[1]) + 
    ggplot2::ylab(u.axis.labs.CN[2]) + 
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Cap Noir") +
    # Draw directions
    ggplot2::geom_segment(data = df.v.CN,
                          ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                        'picas')), 
                          color = 'darkred') +
    # Draw either labels or points
    ggplot2::geom_point(ggplot2::aes(color = groups), 
                        size = 1.5,
                        alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
    ) + 
    ggplot2::scale_color_manual(values = c("1" = "#44A57CFF",
                                          "2" = "#7EBAC2FF",
                                          "3" = "#D8AF39FF", 
                                          "4" = "#AE93BEFF",
                                          "5" = "#1D2645FF"), 
                               name = "Cluster") +
    ggplot2::geom_text(data = df.u.CN, 
                       ggplot2::aes(label = labels, x = xvar + 0.06, y = yvar + 0.06
                       ), 
                       color = 'black', size = 5) +
    # Label the variable axes
    ggplot2::geom_text(data = df.v.CN, 
                       ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                    angle = angle, hjust = hjust), 
                       color = 'darkred', size = 5) +
    #ggplot2::scale_color_manual(values = color_scale) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   legend.position = "bottom",
                   legend.title = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   legend.text = ggplot2::element_text(size = 15))
  
  # Overlay a concentration ellipse of clusters
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  
  ell <- plyr::ddply(df.u.CN, 'groups', function(x) {
    if(nrow(x) <= 2) {
      return(NULL)
    }
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
               groups = x$groups[1])
  })
  names(ell)[1:2] <- c('xvar', 'yvar')
  
  g <- g + ggplot2::geom_path(data = ell, ggplot2::aes(color = groups, group = groups))
  
  g
  
  # save plot 
  ggplot2::ggsave("output/clustering with PCs/PCA_biplot_sites_CN_clust_PCs.jpg",
                  scale = 1,
                  height = 8, width = 10
  )
  
  ######################### POINTE SUZANNE #####################################
  
  ##### recover the single value decomposition SVD
  nobs.factor.PS <- sqrt(nrow(res_pca_PS$scores) - 1) # not sure what this is 
  # and what is it for
  
  # standard deviation of the PCs #lambda in ?biplot()
  d.PS <- sqrt(res_pca_PS$eigenvalues)
  
  u.PS <- sweep(res_pca_PS$scores, 2, 1 / (d.PS * nobs.factor.PS), FUN = '*')
  v.PS <- res_pca_PS$loadings
  
  
  #####
  # compute scores 
  # ie coordinates of individuals (observations) on each principal component (PC)
  # pcomp <- pmin(pcomp, ncol(u)) # not sure what is the purpose of this
  df.u.PS <- as.data.frame(sweep(u.PS[,pcomp], 2, d.PS[pcomp]^obs.scale, FUN='*'))
  # scale observations by lambda^(1-scale)
  
  # compute directions 
  # ie coordinates of the variables ie loadings * sdev of PCs
  v.PS <- sweep(v.PS, 2, d.PS^var.scale, FUN='*')
  df.v.PS <- as.data.frame(v.PS[, pcomp])
  
  names(df.u.PS) <- c('xvar', 'yvar')
  names(df.v.PS) <- names(df.u.PS)
  
  df.u.PS <- df.u.PS * nobs.factor.PS # so we are back to the original scores - res_pca$scores
  # ie the coordinates of the individuals on the PCs
  
  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores (as done with ggbiplot)
  r.PS <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u.PS^2))^(1/4) 
  
  # scale directions
  # v^2 = cos2 = quality of representation of variables on each PC 
  v.scale.PS <- rowSums(v.PS^2)
  df.v.PS <- r.PS * df.v.PS / sqrt(max(v.scale.PS))
  # multiply then by another constant to get arrows on the same scale as observations 
  # coordinates
  # as mentioned in 
  # https://stats.stackexchange.com/questions/141085/positioning-the-arrows-on-a-pca-biplot
  # "it might be necessary to scale arrows by some arbitrary constant factor so 
  # that both arrows and data points appear roughly on the same scale "
  df.v.PS <- var.add.scaling * df.v.PS
  
  # scale scores 
  # as done by 
  # https://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot
  # with r <- 1 
  # r.scale=sqrt(max(df.u[,1]^2+df.u[,2]^2))
  # df.u=.99*df.u/r.scale
  # this version was set aside as we are more interested in comparing individuals 
  # and not structuring variables, so we went for an additional scaling 
  # of variables coordinates instead - see above
  
  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste('PC', pcomp,' (clr - robust)', sep='')   
  # add explained variance
  u.axis.labs.PS <- paste(u.axis.labs, 
                          sprintf('(%0.1f%% explained var.)', 
                                  100 * res_pca_PS$eigenvalues[pcomp]/sum(res_pca_PS$eigenvalues)))
  
  
  # Score Labels (labels of the observations)
  df.u.PS$labels <- (scat_compo_tib |> 
                       dplyr::filter(stringr::str_detect(Code_sample, "PS")))$Code_sample
  
  # define groups
  # grouping per cluster
  df.u.PS$groups <- as.factor(clust_vec_PS)
  
  # Variable Names
  df.v.PS$varname <- rownames(v.PS)
  
  # Variables for text label placement
  varname.adjust <- 1.2 # adjustment factor the placement of the variable names, >= 1 means farther from the arrow
  var.axes <- TRUE # draw arrow for the variable
  df.v.PS$angle <- with(df.v.PS, (180/pi) * atan(yvar / xvar))
  df.v.PS$hjust = with(df.v.PS, (1 - varname.adjust * sign(xvar)) / 2)
  
  
  ############## draw biplot
  # Base plot
  g <- ggplot2::ggplot(data = df.u.PS, ggplot2::aes(x = xvar, y = yvar)) + 
    ggplot2::xlab(u.axis.labs.PS[1]) + 
    ggplot2::ylab(u.axis.labs.PS[2]) + 
    ggplot2::coord_equal() +
    ggplot2::ggtitle("Pointe Suzanne") +
    ggplot2::theme_bw() +
    # Draw directions
    ggplot2::geom_segment(data = df.v.PS,
                          ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = ggplot2::arrow(length = ggplot2::unit(1/2, 
                                                                        'picas')), 
                          color = 'darkred') +
    # Draw either labels or points
    ggplot2::geom_point(ggplot2::aes(color = groups), 
                        size = 1.5,
                        alpha = 1 # alpha transparency value for the points (0 = transparent, 1 = opaque)
    ) + 
    ggplot2::scale_color_manual(values = c("1" = "#44A57CFF",
                                           "2" = "#7EBAC2FF",
                                           "3" = "#D8AF39FF", 
                                           "4" = "#AE93BEFF",
                                           "5" = "#1D2645FF"), 
                                name = "Cluster") +
    ggplot2::geom_text(data = df.u.PS, 
                       ggplot2::aes(label = labels, x = xvar + 0.06, y = yvar + 0.06
                       ), 
                       color = 'black', size = 5) +
    # Label the variable axes
    ggplot2::geom_text(data = df.v.PS, 
                       ggplot2::aes(label = varname, x = xvar, y = yvar, 
                                    angle = angle, hjust = hjust), 
                       color = 'darkred', size = 5) +
    #ggplot2::scale_color_manual(values = color_scale) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   axis.title.x = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   legend.text = ggplot2::element_text(size = 15))
  
  # Overlay a concentration ellipse of clusters
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  
  ell <- plyr::ddply(df.u.PS, 'groups', function(x) {
    if(nrow(x) <= 2) {
      return(NULL)
    }
    sigma <- var(cbind(x$xvar, x$yvar))
    mu <- c(mean(x$xvar), mean(x$yvar))
    ed <- sqrt(qchisq(ellipse.prob, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
               groups = x$groups[1])
  })
  names(ell)[1:2] <- c('xvar', 'yvar')
  
  g <- g + ggplot2::geom_path(data = ell, ggplot2::aes(color = groups, group = groups))
  
  g
  
  
  # save plot 
  ggplot2::ggsave("output/clustering with PCs/PCA_biplot_sites_PS_clust_PCs.jpg",
                  scale = 1,
                  height = 8, width = 10
  )
  
}


################################################################################
# functions to compute MWW comparison tests between clusters ###################
# for other nb of clusters than the one chosen in the article ##################


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k4 <- function(list_res_clust_sites,
                            scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Pointe Suzanne") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  
  nut_vec <- unique(scat_compo_tib_CN$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table_CN <- scat_compo_tib_CN |>
      dplyr::filter(Nutrient == nut)
    
    table_PS <- scat_compo_tib_PS |>
      dplyr::filter(Nutrient == nut)
    
    table_CN$cluster <- factor(table_CN$cluster)
    table_PS$cluster <- factor(table_PS$cluster)
    
    table_CN <- table_CN |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_CN <- na.omit(table_CN$`1`)
    clust2_CN <- na.omit(table_CN$`2`)
    clust3_CN <- na.omit(table_CN$`3`)
    clust4_CN <- na.omit(table_CN$`4`)
    
    table_PS <- table_PS |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_PS <- na.omit(table_PS$`1`)
    clust2_PS <- na.omit(table_PS$`2`)
    clust3_PS <- na.omit(table_PS$`3`)
    clust4_PS <- na.omit(table_PS$`4`)
    
    nut_test <- rbind(data.frame(Site = "Cap Noir",
                                 Nutrient = rep(nut, 6), 
                                 Cluster_comp_1 = c("1", "1", "1",
                                                    "2", "2", 
                                                    "3"), 
                                 Cluster_comp_2 = c("2", "3", "4",
                                                    "3", "4", 
                                                    "4"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust2_CN)[[3]],
                                              wilcox.test(clust1_CN, clust3_CN)[[3]],
                                              wilcox.test(clust1_CN, clust4_CN)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust3_CN)[[3]],
                                              wilcox.test(clust2_CN, clust4_CN)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust4_CN)[[3]])), 
                      data.frame(Site = "Pointe Suzanne",
                                 Nutrient = rep(nut, 6), 
                                 Cluster_comp_1 = c("1", "1", "1",
                                                    "2", "2", 
                                                    "3"), 
                                 Cluster_comp_2 = c("2", "3", "4",
                                                    "3", "4", 
                                                    "4"), 
                                 alpha_MW = c(wilcox.test(clust1_PS, clust2_PS)[[3]],
                                              wilcox.test(clust1_PS, clust3_PS)[[3]],
                                              wilcox.test(clust1_PS, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust2_PS, clust3_PS)[[3]],
                                              wilcox.test(clust2_PS, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust3_PS, clust4_PS)[[3]])), 
                      data.frame(Site = "both",
                                 Nutrient = rep(nut, 16), 
                                 Cluster_comp_1 = c("1_CN", "1_CN", "1_CN", "1_CN",
                                                    "2_CN", "2_CN", "2_CN", "2_CN", 
                                                    "3_CN", "3_CN", "3_CN", "3_CN", 
                                                    "4_CN", "4_CN", "4_CN", "4_CN"), 
                                 Cluster_comp_2 = c("1_PS", "2_PS", "3_PS", "4_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust1_PS)[[3]],
                                              wilcox.test(clust1_CN, clust2_PS)[[3]],
                                              wilcox.test(clust1_CN, clust3_PS)[[3]],
                                              wilcox.test(clust1_CN, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust1_PS)[[3]],
                                              wilcox.test(clust2_CN, clust2_PS)[[3]],
                                              wilcox.test(clust2_CN, clust3_PS)[[3]],
                                              wilcox.test(clust2_CN, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust1_PS)[[3]],
                                              wilcox.test(clust3_CN, clust2_PS)[[3]],
                                              wilcox.test(clust3_CN, clust3_PS)[[3]],
                                              wilcox.test(clust3_CN, clust4_PS)[[3]],
                                              
                                              wilcox.test(clust4_CN, clust1_PS)[[3]],
                                              wilcox.test(clust4_CN, clust2_PS)[[3]],
                                              wilcox.test(clust4_CN, clust3_PS)[[3]],
                                              wilcox.test(clust4_CN, clust4_PS)[[3]]))
    )
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Site = NA, 
                        Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,] |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = alpha_MW)
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/figures-tables-article/Mann_Whitney_test_clust_PCs_compo_sites.xlsx")
  
  df_test
}



#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k3 <- function(list_res_clust_sites,
                            scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Pointe Suzanne") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  
  nut_vec <- unique(scat_compo_tib_CN$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table_CN <- scat_compo_tib_CN |>
      dplyr::filter(Nutrient == nut)
    
    table_PS <- scat_compo_tib_PS |>
      dplyr::filter(Nutrient == nut)
    
    table_CN$cluster <- factor(table_CN$cluster)
    table_PS$cluster <- factor(table_PS$cluster)
    
    table_CN <- table_CN |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_CN <- na.omit(table_CN$`1`)
    clust2_CN <- na.omit(table_CN$`2`)
    clust3_CN <- na.omit(table_CN$`3`)
    
    table_PS <- table_PS |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_PS <- na.omit(table_PS$`1`)
    clust2_PS <- na.omit(table_PS$`2`)
    clust3_PS <- na.omit(table_PS$`3`)
    
    nut_test <- rbind(data.frame(Site = "Cap Noir",
                                 Nutrient = rep(nut, 3), 
                                 Cluster_comp_1 = c("1", "1", 
                                                    "2"), 
                                 Cluster_comp_2 = c("2", "3", 
                                                    "3"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust2_CN)[[3]],
                                              wilcox.test(clust1_CN, clust3_CN)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust3_CN)[[3]]
                                 )), 
                      data.frame(Site = "Pointe Suzanne",
                                 Nutrient = rep(nut, 3), 
                                 Cluster_comp_1 = c("1", "1", 
                                                    "2"), 
                                 Cluster_comp_2 = c("2", "3",
                                                    "3"), 
                                 alpha_MW = c(wilcox.test(clust1_PS, clust2_PS)[[3]],
                                              wilcox.test(clust1_PS, clust3_PS)[[3]],
                                              
                                              wilcox.test(clust2_PS, clust3_PS)[[3]])), 
                      data.frame(Site = "both",
                                 Nutrient = rep(nut, 9), 
                                 Cluster_comp_1 = c("1_CN", "1_CN", "1_CN", 
                                                    "2_CN", "2_CN", "2_CN", 
                                                    "3_CN", "3_CN", "3_CN"), 
                                 Cluster_comp_2 = c("1_PS", "2_PS", "3_PS", 
                                                    "1_PS", "2_PS", "3_PS",
                                                    "1_PS", "2_PS", "3_PS"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust1_PS)[[3]],
                                              wilcox.test(clust1_CN, clust2_PS)[[3]],
                                              wilcox.test(clust1_CN, clust3_PS)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust1_PS)[[3]],
                                              wilcox.test(clust2_CN, clust2_PS)[[3]],
                                              wilcox.test(clust2_CN, clust3_PS)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust1_PS)[[3]],
                                              wilcox.test(clust3_CN, clust2_PS)[[3]],
                                              wilcox.test(clust3_CN, clust3_PS)[[3]]
                                 ))
    )
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Site = NA, 
                        Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,] |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = alpha_MW)
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/clustering with PCs/Mann_Whitney_test_clust_PCs_k3_compo_sites.xlsx")
  
  df_test
}


#'
#'
#'
#'
#'
# function to compute Mann-Whitney U Test to assess difference between 
# concentration of fish in different clusters 
MWtest_clust_k5 <- function(list_res_clust_sites,
                            scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Cap Noir") |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                          stringr::str_detect(Code_sample, "PS") ~ "Pointe Suzanne")) |>
    dplyr::filter(site == "Pointe Suzanne") |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    tidyr::pivot_longer(cols = c(Ca, P, Mg, Na, K, 
                                 Fe, Zn, Sr, Cu, Mn, Se,
                                 Ni, Cd, V, Cr, As, Co, 
                                 Ag, Mo, Pb), 
                        names_to = "Nutrient", 
                        values_to = "conc_mg_kg_dw") 
  
  
  nut_vec <- unique(scat_compo_tib_CN$Nutrient)
  
  list_outputs <- list()
  
  for (nut in nut_vec) {
    
    table_CN <- scat_compo_tib_CN |>
      dplyr::filter(Nutrient == nut)
    
    table_PS <- scat_compo_tib_PS |>
      dplyr::filter(Nutrient == nut)
    
    table_CN$cluster <- factor(table_CN$cluster)
    table_PS$cluster <- factor(table_PS$cluster)
    
    table_CN <- table_CN |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_CN <- na.omit(table_CN$`1`)
    clust2_CN <- na.omit(table_CN$`2`)
    clust3_CN <- na.omit(table_CN$`3`)
    clust4_CN <- na.omit(table_CN$`4`)
    clust5_CN <- na.omit(table_CN$`5`)
    
    table_PS <- table_PS |>
      tidyr::pivot_wider(names_from = cluster, 
                         values_from = conc_mg_kg_dw) 
    
    clust1_PS <- na.omit(table_PS$`1`)
    clust2_PS <- na.omit(table_PS$`2`)
    clust3_PS <- na.omit(table_PS$`3`)
    clust4_PS <- na.omit(table_PS$`4`)
    clust5_PS <- na.omit(table_PS$`5`)
    
    nut_test <- rbind(data.frame(Site = "Cap Noir",
                                 Nutrient = rep(nut, 10), 
                                 Cluster_comp_1 = c("1", "1", "1", "1",
                                                    "2", "2", "2",
                                                    "3", "3",
                                                    "4"), 
                                 Cluster_comp_2 = c("2", "3", "4", "5",
                                                    "3", "4", "5",
                                                    "4", "5", 
                                                    "5"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust2_CN)[[3]],
                                              wilcox.test(clust1_CN, clust3_CN)[[3]],
                                              wilcox.test(clust1_CN, clust4_CN)[[3]],
                                              wilcox.test(clust1_CN, clust5_CN)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust3_CN)[[3]],
                                              wilcox.test(clust2_CN, clust4_CN)[[3]],
                                              wilcox.test(clust2_CN, clust5_CN)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust4_CN)[[3]],
                                              wilcox.test(clust3_CN, clust5_CN)[[3]], 
                                              
                                              wilcox.test(clust4_CN, clust5_CN)[[3]])), 
                      data.frame(Site = "Pointe Suzanne",
                                 Nutrient = rep(nut, 10), 
                                 Cluster_comp_1 = c("1", "1", "1", "1",
                                                    "2", "2", "2",
                                                    "3", "3",
                                                    "4"), 
                                 Cluster_comp_2 = c("2", "3", "4", "5",
                                                    "3", "4", "5",
                                                    "4", "5", 
                                                    "5"), 
                                 alpha_MW = c(wilcox.test(clust1_PS, clust2_PS)[[3]],
                                              wilcox.test(clust1_PS, clust3_PS)[[3]],
                                              wilcox.test(clust1_PS, clust4_PS)[[3]],
                                              wilcox.test(clust1_PS, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust2_PS, clust3_PS)[[3]],
                                              wilcox.test(clust2_PS, clust4_PS)[[3]],
                                              wilcox.test(clust2_PS, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust3_PS, clust4_PS)[[3]],
                                              wilcox.test(clust3_PS, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust4_PS, clust5_PS)[[3]])), 
                      data.frame(Site = "both",
                                 Nutrient = rep(nut, 25), 
                                 Cluster_comp_1 = c("1_CN", "1_CN", "1_CN", "1_CN", "1_CN",
                                                    "2_CN", "2_CN", "2_CN", "2_CN", "2_CN", 
                                                    "3_CN", "3_CN", "3_CN", "3_CN", "3_CN", 
                                                    "4_CN", "4_CN", "4_CN", "4_CN", "4_CN", 
                                                    "5_CN", "5_CN", "5_CN", "5_CN", "5_CN"), 
                                 Cluster_comp_2 = c("1_PS", "2_PS", "3_PS", "4_PS", "5_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS", "5_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS", "5_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS", "5_PS",
                                                    "1_PS", "2_PS", "3_PS", "4_PS", "5_PS"), 
                                 alpha_MW = c(wilcox.test(clust1_CN, clust1_PS)[[3]],
                                              wilcox.test(clust1_CN, clust2_PS)[[3]],
                                              wilcox.test(clust1_CN, clust3_PS)[[3]],
                                              wilcox.test(clust1_CN, clust4_PS)[[3]],
                                              wilcox.test(clust1_CN, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust2_CN, clust1_PS)[[3]],
                                              wilcox.test(clust2_CN, clust2_PS)[[3]],
                                              wilcox.test(clust2_CN, clust3_PS)[[3]],
                                              wilcox.test(clust2_CN, clust4_PS)[[3]],
                                              wilcox.test(clust2_CN, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust3_CN, clust1_PS)[[3]],
                                              wilcox.test(clust3_CN, clust2_PS)[[3]],
                                              wilcox.test(clust3_CN, clust3_PS)[[3]],
                                              wilcox.test(clust3_CN, clust4_PS)[[3]],
                                              wilcox.test(clust3_CN, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust4_CN, clust1_PS)[[3]],
                                              wilcox.test(clust4_CN, clust2_PS)[[3]],
                                              wilcox.test(clust4_CN, clust3_PS)[[3]],
                                              wilcox.test(clust4_CN, clust4_PS)[[3]],
                                              wilcox.test(clust4_CN, clust5_PS)[[3]],
                                              
                                              wilcox.test(clust5_CN, clust1_PS)[[3]],
                                              wilcox.test(clust5_CN, clust2_PS)[[3]],
                                              wilcox.test(clust5_CN, clust3_PS)[[3]],
                                              wilcox.test(clust5_CN, clust4_PS)[[3]],
                                              wilcox.test(clust5_CN, clust5_PS)[[3]]))
    )
    
    list_outputs <- append(list_outputs, list(nut_test))
  }
  
  
  df_test <- data.frame(Site = NA, 
                        Nutrient = NA, 
                        Cluster_comp_1 = NA,
                        Cluster_comp_2 = NA,
                        alpha_MW = NA)
  
  for (i in 1:length(nut_vec)) {
    df_test <- rbind(df_test, list_outputs[[i]])
  }
  
  # delete first line of NAs
  df_test <- df_test[-1,] |>
    tidyr::pivot_wider(names_from = Nutrient, 
                       values_from = alpha_MW)
  
  openxlsx::write.xlsx(df_test, 
                       file = "output/clustering with PCs/Mann_Whitney_test_clust_PCs_k5_compo_sites.xlsx")
  
  df_test
}



#'
#'
#'
#'
#'
# function to display correlation plot of elemental composition of scats
corr_compo_clusters <- function(list_res_clust_sites,
                                scat_compo_tib) {
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "CN")) |>
    dplyr::mutate(cluster = clust_vec_CN)
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "PS")) |>
    dplyr::mutate(cluster = clust_vec_PS) 
  
  # plot only lower triangle in the corrplot
  get_lower_tri<-function(cormat){
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
  }
  
  ####################### CAP NOIR #############################################
  vec_clust_CN <- unique(scat_compo_tib_CN$cluster)
  
  for (cl in vec_clust_CN) {
    corr_mat_CN_cli <- robCompositions::corCoDa(
      as.data.frame(scat_compo_tib_CN |>
                      dplyr::filter(cluster == cl) |>
                      dplyr::select(c(Ca, P, Mg, Na, K, 
                                      Fe, Zn, Sr, Cu, Mn, Se,
                                      Ni, Cd, V, Cr, As, Co, 
                                      Ag, Mo, Pb)))) 
    
    colnames(corr_mat_CN_cli) <- rownames(corr_mat_CN_cli) <- c("Ca", "P", "Mg", "Na", "K", 
                                                                "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                                "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                                "Ag", "Mo", "Pb")
    
    melted_cormat_CN_cli <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat_CN_cli), 
                                                        na.rm = TRUE))
    
    ggplot2::ggplot(data = melted_cormat_CN_cli, ggplot2::aes(Var2, Var1, fill = value)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                    high = "#E75B64FF", 
                                    mid = "white", 
                                    midpoint = 0, limit = c(-1,1),
                                    name = "Correlation\ncoefficient") +
      ggplot2::theme_bw() + 
      ggplot2::ggtitle(paste("Cap Noir - cluster", cl)) +
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
    ggplot2::ggsave(paste0("output/clustering with PCs/covar_mat_CapNo_clust", cl, ".jpg"),
                    scale = 1,
                    height = 5, width = 7.5)
    
  }
   
  
  ###################### POITNE SUZANNE ########################################
  vec_clust_PS <- unique(scat_compo_tib_PS$cluster)
  
  for (cl in vec_clust_PS) {
    corr_mat_PS_cli <- robCompositions::corCoDa(
      as.data.frame(scat_compo_tib_PS |>
                      dplyr::filter(stringr::str_detect(Code_sample, "PS") &
                                      cluster == cl) |>
                      dplyr::select(c(Ca, P, Mg, Na, K, 
                                      Fe, Zn, Sr, Cu, Mn, Se,
                                      Ni, Cd, V, Cr, As, Co, 
                                      Ag, Mo, Pb)))) 
    
    colnames(corr_mat_PS_cli) <- rownames(corr_mat_PS_cli) <- c("Ca", "P", "Mg", "Na", "K", 
                                                                "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                                                "Ni", "Cd", "V", "Cr", "As", "Co", 
                                                                "Ag", "Mo", "Pb")
    
    melted_cormat_PS_cli <- tibble::as_tibble(reshape2::melt(get_lower_tri(corr_mat_PS_cli), 
                                                        na.rm = TRUE))
    
    ggplot2::ggplot(data = melted_cormat_PS_cli, ggplot2::aes(Var2, Var1, fill = value)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = "#F0D77BFF", 
                                    high = "#E75B64FF", 
                                    mid = "white", 
                                    midpoint = 0, limit = c(-1,1),
                                    name = "Correlation\ncoefficient") +
      ggplot2::theme_bw() + 
      ggplot2::ggtitle(paste("Pointe Suzanne - cluster", cl)) +
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
    ggplot2::ggsave(paste0("output/clustering with PCs/covar_mat_PointeSuz_clust", cl, ".jpg"),
                    scale = 1,
                    height = 5, width = 7.5)
    
  }
  
}


#'
#'
#'
#'
#'
# change in nutrient covariation between scats from two ressembling clusters
covar_changes_close_clusters <- function(list_res_clust_sites,
                                         scat_compo_tib, 
                                         clust_CN, # nb of the cluster to compare
                                         clust_PS) {
  
  
  # assign each sample to its cluster
  clust_vec_CN <- list_res_clust_sites$CN$cluster
  clust_vec_PS <- list_res_clust_sites$PS$cluster
  
  scat_compo_tib_CN <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "CN")) |>
    dplyr::mutate(cluster = clust_vec_CN) |>
    dplyr::filter(cluster == clust_CN)
  
  scat_compo_tib_PS <- scat_compo_tib |>
    dplyr::filter(stringr::str_detect(Code_sample, "PS")) |>
    dplyr::mutate(cluster = clust_vec_PS) |>
    dplyr::filter(cluster == clust_PS)
  
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
    ggplot2::ggtitle(paste("Change in nutrient concentration correlation coefficients in\nscats from cluster", 
                           clust_CN, "in Cap Noir and cluster", clust_PS, "\nin Pointe Suzanne - total change", 
                           total_changes_p, "%")) +
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
  ggplot2::ggsave(paste0("output/clustering with PCs/corrplot_CNclust", 
                         clust_CN, "_PSclust", clust_PS, ".jpg"),
                  scale = 1,
                  height = 5, width = 7.5
  )
  
}

