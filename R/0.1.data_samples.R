################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024
# 00.data_samples.R
#
################################################################################



#' 
#' 
# load excel files  
load_xl <- function(pathxl) {
  readxl::read_xlsx(pathxl)
}

###################### 1 - DATA SAMPLES ########################################
#'
#'
#'
#'
#'
# clean file and summarise data on samples
summary_scat_samples <- function(scat_data_tab) {
  
  table <- scat_data_tab |>
    dplyr::group_by(site, date_collecte) |>
    dplyr::mutate(HPI_0 = dplyr::case_when(index_hard_parts == 0 ~ 1,
                                           TRUE ~ 0),
                  HPI_1 = dplyr::case_when(index_hard_parts == 1 ~ 1,
                                           TRUE ~ 0),
                  HPI_2 = dplyr::case_when(index_hard_parts == 2 ~ 1,
                                           TRUE ~ 0), 
                  HPI_3 = dplyr::case_when(index_hard_parts == 3 ~ 1,
                                           TRUE ~ 0)) |>
    dplyr::summarize(n = dplyr::n_distinct(Code_sample), 
                     wweight_mean = mean(ww), 
                     wweight_min = min(ww), 
                     wweight_max = max(ww), 
                     H20_mean = mean(water_percent), 
                     H20_min = min(water_percent), 
                     H20_max= max(water_percent), 
                     percent_HPI0 = 100*(sum(HPI_0)/n), 
                     percent_HPI1 = 100*(sum(HPI_1)/n), 
                     percent_HPI2 = 100*(sum(HPI_2)/n), 
                     percent_HPI3 = 100*(sum(HPI_3)/n))
  
  openxlsx::write.xlsx(table, 
                       file = "output/summary_scat_samples_sites.xlsx")
}



###################### 2 - SAMPLES COMPO #######################################

#'
#'
#'
#'
#'
# function to identify values under limit of quantification (loq), for non
# essential nutrients 
replace_under_loq_values <- function(scat_compo_tab) 
{
  
  # table to log data about values under LOQ 
  # i.e. nutrients and % of values per nutrient 
  table_LOQ <- scat_compo_tab |>
    dplyr::mutate(Cr = as.numeric(Cr),
                  Mo = as.numeric(Mo), 
                  V = as.numeric(V),
                  Pb = as.numeric(Pb), 
                  Sr = as.numeric(Sr)) |>
    tidyr::pivot_longer(cols = c(Cr, Mo, V, 
                                 Ag, Pb, Cd, Sr,
                                 Ca, P, Mg, Na, K, 
                                 Fe, Zn, Cu, Mn, Se,
                                 As, Ni, Co), 
                        names_to = 'Nutrient', 
                        values_to = "conc_mg_kg_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::reframe(n_under_LOQ = sum(is.na(conc_mg_kg_dw)), 
                   p_under_LOQ = round(100*(n_under_LOQ/length(unique(Code_sample))), 1))
  # save 
  openxlsx::write.xlsx(table_LOQ, 
                       file = "output/samples_nut_LOQ.xlsx")
  
  # handle values under LOQ
  scat_compo_tab |>
    dplyr::mutate(
      Cr = dplyr::case_when(stringr::str_detect(Cr, "<") ~ as.numeric(
        stringr::str_replace(stringr::str_sub(Cr, 
                                              start = 3), ",", "."))/2, 
        TRUE ~ as.numeric(Cr)),
      Mo = dplyr::case_when(stringr::str_detect(Mo, "<") ~ as.numeric(
        stringr::str_replace(stringr::str_sub(Mo, 
                                              start = 3), ",", "."))/2, 
        TRUE ~ as.numeric(Mo)),
      Pb = dplyr::case_when(stringr::str_detect(Pb, "<") ~ as.numeric(
        stringr::str_replace(stringr::str_sub(Pb, 
                                              start = 3), ",", "."))/2, 
        TRUE ~ as.numeric(Pb)),
      Sr = dplyr::case_when(stringr::str_detect(Sr, "<") ~ as.numeric(
        stringr::str_replace(stringr::str_sub(Sr, 
                                              start = 3), ",", "."))/2, 
        TRUE ~ as.numeric(Sr)),
      V = dplyr::case_when(stringr::str_detect(V, "<") ~ as.numeric(
        stringr::str_replace(stringr::str_sub(V, 
                                              start = 3), ",", "."))/2, 
        TRUE ~ as.numeric(V)))
  
}

############ ID statistical outliers

#'
#'
#'
#'
#'
# function to display boxplots with labeled outliers
boxplot_id_outliers_stats <- function(scat_compo_tab) 
  {
  
  # small function to find statistical outliers as an indication of 
  # potential adnormal values 
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  scat_compo_tab |>
    tidyr::pivot_longer(cols = c("As":"Ag"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw),
                                   Code_sample, NA), 
                  Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Pb", "Mo"))) |>
    ggplot2::ggplot(ggplot2::aes(x = Nutrient, y = concentration_mg_g_dw, 
                                 fill = Nutrient)) +
    ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm=TRUE, 
                       hjust=-.1, size = 3.5) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#26432FFF", "#C0DDE1FF",
                                          "#5A6F80FF", "#6FB382FF", "#278B9AFF",
                                          "#3A160AFF", "#CEC917FF", "#92BBD9FF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF",
                                          "#583B2BFF", "#D98594FF",
                                          "#E8C4A2FF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/ID outliers/boxplot_stat_outliers_scats.jpg", 
                  scale = 1,
                  height = 7, width = 8
  )
  
  
}

#'
#'
#'
#'
#'
# function to generate tables with all samples identified as 
# statistical outliers, and 
tib_id_stat_outliers <- function(scat_compo_tab
) {
  
  # small function to find outliers
  find_outlier <- function(x) {
    return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
  }
  
  options(scipen = 999)
  
  table <- scat_compo_tab |>
    tidyr::pivot_longer(cols = c("As":"Ag"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::group_by(Nutrient) |>
    dplyr::mutate(outlier = ifelse(find_outlier(concentration_mg_g_dw),
                                   Code_sample, NA), 
                  mean_all = mean(concentration_mg_g_dw), 
                  min_all = min(concentration_mg_g_dw), 
                  max_all = max(concentration_mg_g_dw)) |>
    dplyr::filter(!is.na(outlier)) |> # keep only samples with abnormal values 
    dplyr::select(Code_sample, Nutrient, concentration_mg_g_dw, 
                  mean_all, min_all, max_all) |>
    dplyr::arrange(Code_sample, Nutrient)
  
  
  openxlsx::write.xlsx(table,
                       file = "output/ID outliers/id_stat_outliers_scats.xlsx")
  
}


#'
#'
#'
#'
#'
# function to show histograms and identify extreme values 
# analytical outliers 
hist_id_outliers <- function(scat_compo_tab)
  {
  
  scat_compo_tab |>
    tidyr::pivot_longer(cols = c("As":"Ag"), 
                        names_to = "Nutrient", 
                        values_to = "concentration_mg_g_dw") |>
    dplyr::mutate(Nutrient = factor(Nutrient, 
                                    levels = c("Ca", "P", "Mg", "Na", "K", 
                                               "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                               "Ni", "Cd", "V", "Cr", "As", "Co", 
                                               "Ag", "Pb", "Mo"))) |>
    ggplot2::ggplot(ggplot2::aes(x = concentration_mg_g_dw, fill = Nutrient)) +
    ggplot2::geom_histogram() +
    ggplot2::scale_fill_manual(values = c("#4C413FFF", "#26432FFF", "#C0DDE1FF",
                                          "#5A6F80FF", "#6FB382FF", "#278B9AFF",
                                          "#3A160AFF", "#CEC917FF", "#92BBD9FF",
                                          "#E75B64FF", "#DE7862FF", "#D8AF39FF",
                                          "#583B2BFF", "#D98594FF",
                                          "#E8C4A2FF", "#1D2645FF", 
                                          "#403369FF", "#AE93BEFF", "#B4DAE5FF", 
                                          "#F0D77BFF")) +
    ggplot2::facet_wrap(~ Nutrient, scale = "free") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15), 
                   axis.text.y = ggplot2::element_text(size = 15), 
                   title = ggplot2::element_text(size = 17, 
                                                 face = "bold"),
                   axis.title.x = ggplot2::element_blank(), 
                   axis.title.y = ggplot2::element_text(size = 16, 
                                                        face = "bold"),
                   strip.text.x = ggplot2::element_text(size = 15),
                   legend.position = "none")
  ggplot2::ggsave("output/ID outliers/hist_outliers_scats.jpg",
                  scale = 1,
                  height = 8, width = 14
  )
  
  
}


#'
#'
#'
#'
# select only nutrient of interest, get rid of technical outliers/contaminated
clean_scats_compo_from_outliers <- function(scat_compo_tab) {
  
  scat_compo_tab |>
    # for technical outliers
    dplyr::filter(!(Code_sample %in% c("CN12")))
  
}


