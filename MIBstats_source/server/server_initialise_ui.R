# _Code for disabling most of the app tabs that is run when the app is loaded. The tabs get enabled later when data is loaded or statistical tests are performed

initialise_tabs <- function(mode_f) {
  
  tabs_to_disable <- c("tab_view_loaded_data",
                       "tab_handling_values",
                       "handling_na_zero_inf",
                       "tab_pca",
                       "tab_sorting",
                       "tab_filtering",
                       "tab_scaling_normalisation_etc",
                       "tab_view_preprocessed_data",
                       "tab_choose_data_for_comparing_two_groups",
                       "tab_shapiro_wilk_test",
                       "tab_f_test",
                       "tab_t_test",
                       "tab_mann_whitney_u_test",
                       "tab_fold_changes",
                       "tab_rank_product",
                       "tab_rank_sum",
                       "tab_view_shapiro_wilk_test_results",
                       "tab_view_f_test_results",
                       "tab_view_t_test_results",
                       "tab_view_mann_whitney_test_results",
                       "tab_view_fold_changes_results",
                       "tab_volcano_plot",
                       "tab_view_rank_product_results",
                       "tab_view_rank_sum_results",
                       "tab_view_iga_results",
                       "tab_view_db_iga_results")
  
  
  for(item_t in tabs_to_disable) {
    js$disableTab(item_t)
  }
}




initialise_tabs()


