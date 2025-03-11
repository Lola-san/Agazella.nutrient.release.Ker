################################################################################
# Agazella.modelling.Ker project
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# July 2023
# _targets.R
#
# Script decomposing with all steps of the analysis with target
################################################################################

library("targets")


# Source all functions contained in all files in the R directory
lapply(list.files(here::here("R"),
                  recursive = TRUE, full.names = T),
       source)


list(
  
  ##############################################################################
  ############################# SCAT DATA ######################################
  ##############################################################################
  ##--------------- script 0.1.data_samples.R -----------------------##
  # define and load data on samples of scats
  tar_target(data_scat_file,
             "data/data_scats.xlsx",
             format = "file"),
  tar_target(data_scat_samples, load_xl(data_scat_file)),
  # define and load results of composition of scats
  tar_target(res_compo_scats_file,
             "data/res_compo_scats.xlsx",
             format = "file"),
  tar_target(res_compo_scats_raw, load_xl(res_compo_scats_file)),

  # summarize data on scat samples
  tar_target(data_scat_summary, summary_scat_samples(data_scat_samples)),
  
  # handle values under LOQ
  tar_target(res_compo_scats_LOQ_OK, replace_under_loq_values(res_compo_scats_raw)),
  
  # ID outliers
  # statistical outliers
  tar_target(boxplot_outliers_scats_stats, 
             boxplot_id_outliers_stats(res_compo_scats_LOQ_OK)), # boxplot
  tar_target(tib_outliers_scats_stats, 
             tib_id_stat_outliers(res_compo_scats_LOQ_OK)), # tibble
  # technical outliers 
  tar_target(hist_outliers_scats_tech, 
             hist_id_outliers(res_compo_scats_LOQ_OK)), # histogram
  ### CN12 is removed because of possible contamination (Fe-Ni high values)
  # other are kept to estimate nutrient release for colonies
  # but some high values should be handled for any stat. analysis (e.g. clustering)
  # those are PS06 for K, CN02 for Cu, CN11 for Cd and CN24 for Mo
  tar_target(res_compo_scats, 
             clean_scats_compo_from_outliers(res_compo_scats_LOQ_OK)), 
  
  ##--------------- script 0.2.scats_compo_sites.R -----------------------##
  # results' exploration and summaries
  # nutrient composition of scats as in the dataset
  tar_target(boxplot_scat_compo_in_sites,
             boxplot_violon_compo_scats_site(res_compo_scats)),
  tar_target(boxplot_only_scat_compo_in_sites,
             boxplot_compo_scats_site(res_compo_scats)),
  tar_target(table_summary_scat_compo_in_sites,
             table_compo_scats_site(res_compo_scats)),
  tar_target(table_test_scat_compo_sites,
             MWtest_scats_compo_sites(res_compo_scats)),
  
  # relative composition of scats
  tar_target(fig_scat_compo_rel,
             mean_scat_compo_relative_sites(res_compo_scats)),
  tar_target(fig_scat_compo_rel_sites,
             barplot_nut_scat_compo_relative_sites(res_compo_scats)),
  tar_target(table_summary_scat_compo_rel_in_sites,
             table_compo_relative_scats_site(res_compo_scats)),
  tar_target(table_test_scat_compo_rel_sites,
             MWtest_scats_compo_relative_sites(res_compo_scats)),
  
  # covariation between different nutrient concentrations
  tar_target(corr_mat_sites,
             corr_compo_scats(res_compo_scats)),
  tar_target(corr_mat_diff_covar_sites,
             covar_diff_sites(res_compo_scats)),
  
  # scat dessication and nutrient concentrations
  tar_target(conc_vs_water_plot,
             plot_concentration_vs_water_content(data_scat_samples, 
                                                 res_compo_scats)),
  
  ################ ANALYSIS : how much nutrients do  ###########################
  ######################## the two colonies release ? ##########################
  #################### DATA : SAMPLES ##########################################

  # script 1.1.prepare_pop_input_data.R
  # define and load data
  tar_target(data_counts_file,
            "data/counts_Ker_2022-2023_compiled.xlsx",
            format = "file"),
  tar_target(data_counts_raw, load_xl(data_counts_file)),
  tar_target(pop_counts_summary,
            Ker_summarise_count_data(data_counts_raw)),
  
  # prepare full dataset with colony counts and bioenergetic data
  tar_target(pop_initial_count_data,
             simulate_count_data(pop_counts_summary)),
  # because of memory limit, we'll split everything for each site
  tar_target(list_pop_data_with_indi_data, # now a list with data for the two sites
             pop_data_for_simulations(pop_initial_count_data,
                                      nsim = 5000)),
  tar_target(pop_data_with_indi_data_CN, list_pop_data_with_indi_data$CN),
  tar_target(pop_data_with_indi_data_PS, list_pop_data_with_indi_data$PS),

  # script 1.2.compute_dm_produced.R
  # estimate dry matter release
  tar_target(output_dm_produced_CN,
             run_dm_estimate(pop_data_with_indi_data_CN)),
  tar_target(output_dm_produced_PS,
             run_dm_estimate(pop_data_with_indi_data_PS)),

  # script 1.3.output_dm_produced.R
  tar_target(barplot_dm_produced_per_site_period,
             dm_per_site_period(output_dm_produced_CN,
                                output_dm_produced_PS)),
  tar_target(table_test_dm_release_per_site_tot_period,
             test_diff_dm_sites_tot_period(output_dm_produced_CN,
                                           output_dm_produced_PS)),
  tar_target(table_summary_model_param,
             table_model_param(output_dm_produced_CN,
                               output_dm_produced_PS)),

  # script 2.1.compute_nut_release.R
  # prepare scat composition dataset
  tar_target(output_dm_produced_with_scat_compo_data_CN,
             add_bootstrap_scat_data(output_dm_produced_CN,
                                     res_compo_scats)),
  tar_target(output_dm_produced_with_scat_compo_data_PS,
             add_bootstrap_scat_data(output_dm_produced_PS,
                                     res_compo_scats)),
  tar_target(output_nut_release_CN,
             compute_nut_release(output_dm_produced_with_scat_compo_data_CN)),
  tar_target(output_nut_release_PS,
             compute_nut_release(output_dm_produced_with_scat_compo_data_PS)),

  # script 2.2.output_nut_release.R

  tar_target(barplot_nut_release_per_site_tot_period,
             nut_per_site_tot_period(output_nut_release_CN,
                                     output_nut_release_PS)),
  tar_target(table_nut_release_per_site_period,
             table_nut_per_site_sea_land_period(output_nut_release_CN,
                                                output_nut_release_PS)),
  tar_target(table_test_nut_release_per_site_tot_period,
             test_nut_sites_tot_period(output_nut_release_CN,
                                       output_nut_release_PS)),
  tar_target(barplot_diff_nut_sites,
             diff_sites_release_levels_nutrients(output_nut_release_CN,
                                                 output_nut_release_PS)),
  tar_target(barplot_animals_per_site_tot,
             nb_animals_per_site_simulated(output_nut_release_CN,
                                           output_nut_release_PS)),
  
  ####################### THIRD ANALYSIS : SCAT CLUSTERING  ####################
  ##############################################################################
  # handle statistical outliers
  tar_target(res_compo_scats_clustering,
              set_up_scats_compo_clust(res_compo_scats)),
  # ######################## USING ALL NUTRIENTS #################################
  # # script 3.1.clustering_scats_sites.R
  # ### without using a PCA to reduce dimensions
  # tar_target(clust_all_nut_findk_table,
  #            clust_find_k_table_full_tib(res_compo_scats_clustering,
  #                                        method = "ward.D2",
  #                                        k_range = c(2:10))),
  # tar_target(clust_all_nut_findk_means_plot,
  #            means_clust_find_k_val_full_tib(clust_all_nut_findk_table)),
  # # dendrogram ##### DID NOT FIND A WAY TO MAKE A DENDROGRAM WITH GGPLOT
  # # WITH HCLUST OUTPUT OF RobComposition PACKAGE so it's not pretty
  # # but still we can identify samples in clusters...
  # tar_target(clust_all_nut_dendro,
  #            clust_compo_dendro_full_tib(clust_all_nut,
  #                                        res_compo_scats_clustering)),
  # 
  # tar_target(clust_all_nut,
  #            clust_compo_full_tib(res_compo_scats_clustering,
  #                                 k = c(3, 4, 7),
  #                                 method = "ward.D2")),
  # 
  # # script 3.2.output_clustering_scats_sites.R
  # # boxplots
  # tar_target(clust_all_nut_boxplot,
  #            boxplot_compo_clust_full_tib(clust_all_nut,
  #                                         res_compo_scats_clustering)),
  # 
  # # # barplot with relative composition per scat with cluster coloring
  # # tar_target(clust_all_nut_sites_barplot_rel,
  # #            barplot_nut_scat_compo_relative_clust(clust_all_nut_sites,
  # #                                                  res_compo_scats_clustering)),
  # # tar_target(clust_all_nut_all_scats_barplot_rel,
  # #            barplot_nut_scat_compo_relative_clust(clust_all_nut_all_scats,
  # #                                                  res_compo_scats_clustering)),
  # # tables
  # tar_target(table_stats_clusts_all_nut,
  #            table_stats_clust_per_site_full_tib(clust_all_nut,
  #                                                res_compo_scats_clustering)),
  # # tar_target(table_test_clust_all_nut_sites,
  # #            MWtest_clust_k33_full_tib(clust_all_nut_sites,
  # #                                      res_compo_scats)),
  # tar_target(tables_scat_samples_all_nut_clust_attribution,
  #            clust_full_tib_samples(clust_all_nut,
  #                                   res_compo_scats_clustering)), 
  # tar_target(table_test_clust_all_nut_sites,
  #            MWtest_clust_k34_full_tib(clust_all_nut,
  #                                      res_compo_scats_clustering)),
  
  ################ USING PCA FIRST TO REDUCE DIMENSIONS ########################
  # PCA and clustering, script 3.1.clustering_scats_sites.R

  tar_target(list_pca,
             pca_coda(res_compo_scats_clustering)),
  tar_target(biplot_pca,
             biplot_pca_coda(list_pca,
                             res_compo_scats_clustering)),
  tar_target(clust_PCs_findk_table,
             clust_find_k_table_PCs(list_pca,
                                    # pcomp_list = list(CN = c(1:7), 
                                    #                   PS = c(1:6), 
                                    #                   both = c(1:6)), # 99% of var
                                    method = "ward.D2",
                                    k_range = c(2:10))),
  tar_target(clust_PCs_findk_means_plot,
             means_clust_find_k_val(clust_PCs_findk_table)), 
  tar_target(clust_PCs_dendro,
             clust_dendro_scats(list_pca,
                                res_compo_scats_clustering,
                                # pcomp_list = list(CN = c(1:7), 
                                #                   PS = c(1:6), 
                                #                   both = c(1:6)), # 99% of var
                                method = "ward.D2",
                                k = c(3, 4, 4))),
  tar_target(clust_PCs,
             clust_compo_PCs(list_pca,
                             # pcomp_list = list(CN = c(1:7), 
                             #                   PS = c(1:6), 
                             #                   both = c(1:6)), # 99% of var
                             k = c(3, 4, 4),
                             method = "ward.D2")),
  # script 3.2.output_clustering_scats_sites.R
  tar_target(boxplot_scat_compo_PCs_clust,
             boxplot_compo_clust_PCs(clust_PCs,
                                     res_compo_scats)),
  tar_target(boxplot_grouped_scat_compo_PCs_clust,
             boxplot_grouped_compo_clust_PCs(clust_PCs,
                                             res_compo_scats)),
  tar_target(barplot_scat_compo_rel_PCs_clust,
             barplot_nut_scat_compo_relative_clusters(clust_PCs,
                                                      res_compo_scats)),
  tar_target(tables_scat_samples_PCs_clust_stats,
             table_stats_clust_PCs_per_site(clust_PCs,
                                            res_compo_scats)),
  tar_target(tables_scat_samples_PCs_clust_attribution,
             clust_PCs_samples(clust_PCs,
                           res_compo_scats_clustering)), 
  tar_target(tables_scat_samples_PCs_clust_percents,
             table_compo_clust_PCs_percent_per_site(clust_PCs,
                                                    res_compo_scats_clustering)),
  tar_target(biplot_scat_samples_PCs_clust,
             biplot_after_clust(list_pca,
                                clust_PCs,
                                res_compo_scats_clustering)),
  tar_target(table_test_clust_PCs_k34_sites,
             MWtest_clust_k34(clust_PCs,
                             res_compo_scats_clustering)), 
  
  # corrplot
  tar_target(corrmat_clusters,
             corr_compo_clusters(clust_PCs,
                                 res_compo_scats)), 
  tar_target(corrmat_comp_clusters_sites_CN_PS1,
             covar_changes_close_clusters(clust_PCs,
                                          res_compo_scats, 
                                          1, 
                                          1)), 
  tar_target(corrmat_comp_clusters_sites_CN2_PS3,
             covar_changes_close_clusters(clust_PCs,
                                          res_compo_scats, 
                                          2, 
                                          3)), 
  tar_target(corrmat_comp_clusters_sites_CN3_PS4,
             covar_changes_close_clusters(clust_PCs,
                                          res_compo_scats, 
                                          3, 
                                          4)),
  
  # tar_target(table_test_clust_PCs_k3_sites,
  #            MWtest_clust_k3(clust_PCs,
  #                            res_compo_scats_clustering))
  # tar_target(table_test_clust_PCs_k4_sites,
  #            MWtest_clust_k4(clust_PCs,
  #                            res_compo_scats_clustering))
  # tar_target(table_test_clust_PCs_k5_sites,
  #            MWtest_clust_k5(clust_PCs,
  #                            res_compo_scats_clustering))
  
  
  ##############################################################################
  ##### script 4.relative-enrichment-thought-experiment.R ######################
  ########### discussion thought experiment: what's the relative ###############
  ########### Fe enrichment provoked by a single fur seal defecation at sea ####
  ############### numbers presented in Fig. 5 ##################################
  # load raw oceanographic data
  tar_target(oceanographic_raw_data_Bowieetal2015, bowieetal2015_data()), 
  # compute averages for the two oceanographic stations studied, adapt units
  tar_target(oceanographic_clean_data_Bowieetal2015, 
             wrangling_bowieetal2015_data(oceanographic_raw_data_Bowieetal2015)),
  # simulate three hypothetical fur seal scat: one highly concentrated in Fe, 
  # one with median concentrations, one with low concentrations.
  tar_target(hypothetical_scats, 
             hypo_scats(data_scat_samples, 
                        res_compo_scats)),
  tar_target(relative_enrichment_at_sea_figures, 
             relative_enrichment(oceanographic_clean_data_Bowieetal2015, 
                                 hypothetical_scats)) 

)





