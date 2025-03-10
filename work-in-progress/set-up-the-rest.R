################################################################################
# Agazella.nutrient.release.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2024  - development script
# set-up-the-rest.R
#
################################################################################
# this script does not necessarily contains functions, quite the opposite in fact

# some basic data exploration not to be included in the analysis, for example
# for plot settings

# function in 0.1.data_samples
######### For nutrients not included in the first place, i.e. Cr, V, Pb, Sr, Cd, 
# Mo, Ag - with some having values under quantification limites LOQ
res_compo_tib <- targets::tar_read(res_compo_scats_raw)

# identify nutrients where there are some values under LOQ (which won't be num)
summary(res_compo_tib)
# Cr, Mo, Pb, Sr, V determine % of values with no concentration

table(is.na(as.numeric(res_compo_tib$Cr))) # 13/60 22%
table(is.na(as.numeric(res_compo_tib$Mo))) # 4/60 7%
table(is.na(as.numeric(res_compo_tib$Pb))) # 5/60 8%
table(is.na(as.numeric(res_compo_tib$Sr))) # 1/60 2%
table(is.na(as.numeric(res_compo_tib$V))) # 16/60 27%

res_compo_tib |>
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

# So more than 75% of samples are quantified, so we should keep all and replace 
# values by half of sample LOQ, which means we need to log this LOQ

trial <- res_compo_tib |>
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

######### For relative composition of scats plot, 0.2.scats_compo_sites.R
scat_compo_tib <- targets::tar_read(res_compo_scats)

# to order samples so that it is prettier on plots, find order or conc with 
# the nutrient with the highest relative concentration, i.e. Ca
order_Ca_CNo <- scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Cap Noir", Nutrient == "Ca") |>
  dplyr::arrange(desc(conc_relative))

order_Ca_CNo$Code_sample

order_Ca_PSuz <- scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Pointe Suz", Nutrient == "Ca") |>
  dplyr::arrange(desc(conc_relative))

order_Ca_PSuz$Code_sample

# this does not look right in log10 scale, it's counter-intuitive
order_Ca_CNo$Code_sample
scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Cap Noir", Code_sample == "CN29")

scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Cap Noir", Code_sample == "CN01") 

scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "Ni", "As","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Pointe Suz") |>
  # order of samples determined in work-in-progress/set-up-the-rest.R
  dplyr::mutate(Code_sample = factor(Code_sample, 
                                     levels = c("PS31", "PS19", "PS21",
                                                "PS11", "PS28", "PS17",
                                                "PS14", "PS07", "PS08",
                                                "PS25", "PS27", "PS12",
                                                "PS20", "PS02", "PS18",
                                                "PS24", "PS29", "PS01",
                                                "PS03", "PS04", "PS06",
                                                "PS16", "PS22", "PS10",
                                                "PS05", "PS30", "PS26",
                                                "PS13", "PS23", "PS09",
                                                "PS15"))) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = Code_sample, y = conc_relative, 
                                 fill = Nutrient), 
                    stat = "identity", 
                    position = ggplot2::position_stack()) +
  ggplot2::scale_fill_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                        "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                        "#E8C4A2FF", "#14191FFF", "#2e276a", 
                                        "#5c4d73", "#AE93BEFF", "#B4DAE5FF", 
                                        "#F0D77BFF")) +
  ggplot2::scale_y_log10(labels = scales::label_log(digits = 2)) +
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
                 strip.text.x = ggplot2::element_blank(),
                 legend.position = "bottom", 
                 legend.text = ggplot2::element_text(size = 12))


# trying a point plot then 
scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Pointe Suz") |>
  # order of samples determined in work-in-progress/set-up-the-rest.R
  dplyr::mutate(Code_sample = factor(Code_sample, 
                                     levels = c("PS31", "PS19", "PS21",
                                                "PS11", "PS28", "PS17",
                                                "PS14", "PS07", "PS08",
                                                "PS25", "PS27", "PS12",
                                                "PS20", "PS02", "PS18",
                                                "PS24", "PS29", "PS01",
                                                "PS03", "PS04", "PS06",
                                                "PS16", "PS22", "PS10",
                                                "PS05", "PS30", "PS26",
                                                "PS13", "PS23", "PS09",
                                                "PS15"))) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = Code_sample, y = conc_relative, 
                                 colour = Nutrient), 
                      size = 3) +
  ggplot2::scale_color_manual(values = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                                        "#E75B64FF", "#DE7862FF", "#D8AF39FF", 
                                        "#E8C4A2FF", "#14191FFF", "#2e276a", 
                                        "#5c4d73", "#AE93BEFF", "#B4DAE5FF", 
                                        "#F0D77BFF")) +
  ggplot2::scale_y_log10(labels = scales::label_log(digits = 2)) +
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
                 strip.text.x = ggplot2::element_blank(),
                 legend.position = "bottom", 
                 legend.text = ggplot2::element_text(size = 12))

# with a classification based on Fe relative concentration
order_Fe_CNo <- scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Cap Noir", Nutrient == "Fe") |>
  dplyr::arrange(desc(conc_relative))

order_Fe_CNo$Code_sample

order_Fe_PSuz <- scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co")), 
                site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |> 
  dplyr::filter(site == "Pointe Suz", Nutrient == "Fe") |>
  dplyr::arrange(desc(conc_relative))

order_Fe_PSuz$Code_sample


# with all scats, i.e. both sites together

order_Fe_all <- scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "As", "Ni","Co"))) |> 
  dplyr::filter(Nutrient == "Fe") |>
  dplyr::arrange(desc(conc_relative))

order_Fe_all$Code_sample

# trying two plots: one for micronutrients, one for macronutrients 
scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Mg", "Na", "K", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "Ni", "As","Co")), 
                micro_macro = dplyr::case_when(Nutrient %in% c("Ca", "P", "Na", 
                                                               "K", "Mg", "Fe" 
                                                               ) ~ "macro", 
                                               Nutrient %in% c("Zn", "Cu",
                                                               "Mn", "Se", "Ni", 
                                                               "As","Co") ~ "micro"),
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
                                        "#B4DAE5FF",  
                                        "#AE93BEFF", "#E75B64FF", 
                                        "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                        "#D8AF39FF","#2e276a", "#5c4d73", 
                                        "#14191FFF")) +
  ggplot2::facet_wrap(~ micro_macro, scales = "free_y") +
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
                 strip.text.x = ggplot2::element_blank(),
                 legend.position = "bottom", 
                 legend.text = ggplot2::element_text(size = 12))
ggplot2::ggsave("output/scat_compo_rel_micro-macro_comp_PSuz_Agazella_stack_Feorder.jpg",
                scale = 1,
                height = 4, width = 10
)

# trying two plots: one for micronutrients, one for macronutrients 
scat_compo_tib |>
  dplyr::mutate(sum_nut = P + Ca + Mg + Na + K + Fe + 
                  Zn + Cu + Mn + Se + Ni + As + Co) |>
  tidyr::pivot_longer(cols = c(As, Ca, Co, Cu, Fe, K, 
                               Mg, Mn, Na, Ni, P, Se, Zn), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(conc_relative = conc_mg_kg_dw/sum_nut, 
                Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Mg", "Na", "K", 
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "Ni", "As","Co")), 
                micro_macro = dplyr::case_when(Nutrient %in% c("Ca", "P", "Na", 
                                                               "K", "Mg", "Fe" 
                ) ~ "macro", 
                Nutrient %in% c("Zn", "Cu",
                                "Mn", "Se", "Ni", 
                                "As","Co") ~ "micro"),
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
                                        "#B4DAE5FF",  
                                        "#AE93BEFF", "#E75B64FF", 
                                        "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                        "#D8AF39FF","#2e276a", "#5c4d73", 
                                        "#14191FFF")) +
  ggplot2::facet_wrap(~ micro_macro, scales = "free_y") +
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
                 strip.text.x = ggplot2::element_blank(),
                 legend.position = "bottom", 
                 legend.text = ggplot2::element_text(size = 12))
ggplot2::ggsave("output/scat_compo_rel_micro-macro_comp_CNoir_Agazella_stack_Feorder.jpg",
                scale = 1,
                height = 4, width = 10
)

#################################################################################
# find the number of PCs to include into the clustering analysis
targets::tar_read(list_pca)$CN 
# to have >= 95% of variance explained we should take PCs one to 5
# to have >= 99% of variance explained we should take PCs one to 9

targets::tar_read(list_pca)$PS 
# to have >= 95% of variance explained we should take PCs one to 5
# to have >= 99% of variance explained we should take PCs one to 9

targets::tar_read(list_pca)$both
# to have >= 95% of variance explained we should take PCs one to 6
# to have > 99% of variance explained we should take PCs one to 10


#################################################################################
# I should get the exact same results as in work with fish/scats together when 
# we do the clusters for both sites together
dat.pca <- targets::tar_read(res_compo_scats_clustering)

data.act_both <- dat.pca |> 
  dplyr::filter(Code_sample != "CN02") |>
  # was considered as an analytical outlier
  dplyr::mutate(K = dplyr::case_when(Code_sample == "PS06" ~ 8100.02013774216, 
                                     TRUE ~ K)) |> 
  # this value was not considered as outlier in previous analysis
  dplyr::select(c(As, Ca, Co, Cu, Fe, K, 
                  Mg, Mn, Na, Ni, P, Se, Zn)) |>
  # same order of nutrients than in previous analysis
  as.data.frame()

head(data.act_both)
tail(data.act_both)
summary(data.act_both) # they are the same exactly

## robust estimation (default):
res.rob_both <- robCompositions::pcaCoDa(data.act_both)
  
summary(res.rob_both)  
res.rob_both


# to type in R project "03.Ker_Arctgazella-prey-poop.R"
# res_tib <- targets::tar_read(full_res_compo_scats)
# data.act <- as.data.frame(res_tib[, 2:14])
# head(data.act)
# tail(data.act)
# summary(data.act)
# res.rob. <- robCompositions::pcaCoDa(data.act)
# res.rob.
# summary(res.rob.)

################################################################################
# try plotting significance letters directly on boxplot because it's so 
# tedious to do it by hand on Powerpoint
scat_compo_tib <- targets::tar_read(res_compo_scats)
res_clust_PCs <- targets::tar_read(clust_PCs)
test_tib <- targets::tar_read(table_test_clust_PCs_sites)
renv::install("ggpubr")

# get coordinates of the tops of the box whiskers
clust_vec_CN <- res_clust_PCs$CN$cluster

boxplot_naked <- scat_compo_tib |>
  dplyr::mutate(site = dplyr::case_when(stringr::str_detect(Code_sample, "CN") ~ "Cap Noir", 
                                        stringr::str_detect(Code_sample, "PS") ~ "Pointe Suz")) |>
  dplyr::filter(site == "Cap Noir") |>
  dplyr::mutate(cluster = clust_vec_CN) |>
  tidyr::pivot_longer(cols = c(Ca, P, Na, K, Mg,
                               Fe, Zn, Cu, Mn, Se,
                               Ni, As, Co), 
                      names_to = "Nutrient", 
                      values_to = "conc_mg_kg_dw") |> 
  dplyr::mutate(Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg",
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "Ni", "As", "Co")), 
                letter_assigned = "a") |>
  dplyr::group_by(Nutrient, cluster) |>
  dplyr::mutate(ymax_whiskers = max(conc_mg_kg_dw)) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = cluster, y = conc_mg_kg_dw, 
                                     fill = factor(cluster)), 
                        position = ggplot2::position_dodge(1)) +
  ggplot2::facet_wrap(~ Nutrient,
                      ncol = 4, scales = "free_y") +
  ggplot2::scale_fill_manual(values = c("1" = "#44A57CFF",
                                        "2" = "#1D2645FF",
                                        "3" = "#D8AF39FF", 
                                        "4" = "#AE93BEFF")) 

boxplot_naked  + 
  ggplot2::geom_text(ggplot2::aes(label = letter_assigned, 
                                  y = ymax_whiskers + 0.1*ymax_whiskers, 
                                  x = cluster))

head(test_tib)

# well it seems it's taking me more time to find a solution than to do it
# so I'm leaving this for now. 


################################################################################
# nutrient release levels at the scale of the colony
# not to plot the estimates with "all scats combined" but to plot the differences
# in nutrient release levels between colonies
output_nut_release_CN <- targets::tar_read(output_nut_release_CN)
output_nut_release_PS <- targets::tar_read(output_nut_release_PS)

rbind(output_nut_release_CN |>
        dplyr::select(c(Site, 
                        release_nut_pop_tot_period_sites)), 
      output_nut_release_PS |>
        dplyr::select(c(Site, 
                        release_nut_pop_tot_period_sites))) |>
  tidyr::unnest(release_nut_pop_tot_period_sites) |>
  tidyr::pivot_longer(cols = c(Ca:Co), 
                      names_to = "Nutrient", 
                      values_to = "tot_pop_release_period_kg") |> 
  dplyr::mutate(Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Na", "K", "Mg",
                                             "Fe", "Zn", "Cu", "Mn", "Se",
                                             "Ni", "As", "Co"))) |>
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
                                        "#B4DAE5FF",  
                                        "#AE93BEFF", "#E75B64FF", 
                                        "#DE7862FF", "#4C413FFF", "#E8C4A2FF", 
                                        "#D8AF39FF","#2e276a", "#5c4d73", 
                                        "#14191FFF")) +
  # add horizontal line with % change of colony counts
  ggplot2::geom_hline(ggplot2::aes(yintercept = 57), 
                      linetype = "dashed",
                      color = "darkred") +
  ggplot2::ylab("% of change in mean nutrient\nrelease level from Cap Noir\nto Pointe Suzanne fur seal colony") +
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
ggplot2::ggsave("output/barplot_diff_nut_release_tot_pop_from_CNo_to_PSuz.jpg",
                scale = 1,
                height = 4, width = 6
)

### differences between colony counts
rbind(output_nut_release_CN |>
        dplyr::mutate(Site = "Cap Noir") |>
        dplyr::select(c(simu_count, 
                        Site)), 
      output_nut_release_PS |>
        dplyr::mutate(Site = "Pointe Suzanne") |>
        dplyr::select(c(simu_count, 
                        Site))) |>
  dplyr::group_by(Site) |>
  dplyr::summarise(mean = mean(simu_count)) |>
  tidyr::pivot_wider(names_from = Site, 
                     values_from = mean) |>
  dplyr::mutate(p_diff_change_CNo_to_PSuz = 100*(`Pointe Suzanne` - `Cap Noir`)/`Cap Noir`) 



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
                               Ag, Pb, Mo), 
                      names_to = "Nutrient", 
                      values_to = "tot_pop_release_period_kg") |> 
  dplyr::mutate(Nutrient = factor(Nutrient, 
                                  levels = c("Ca", "P", "Mg", "Na", "K", 
                                             "Fe", "Zn", "Sr", "Cu", "Mn", "Se",
                                             "Ni", "Cd", "V", "Cr", "As", "Co", 
                                             "Ag", "Pb", "Mo"))) |>
  dplyr::group_by(Site, Nutrient) |>
  dplyr::reframe(mean = mean(tot_pop_release_period_kg)) |>
  tidyr::pivot_wider(names_from = Site, 
                     values_from = mean) |>
  dplyr::mutate(p_diff_change_CNo_to_PSuz = (100 * (`Pointe Suzanne` - `Cap Noir`)/`Cap Noir`)-57)  |>
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
  # # add horizontal line with % change of colony counts
  # ggplot2::geom_hline(ggplot2::aes(yintercept = 57), 
  #                     linetype = "dashed",
  #                     color = "darkred") +
  ggplot2::ylab("% of change in mean nutrient\nrelease level from Cap Noir\nto Pointe Suzanne fur seal colony") +
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
