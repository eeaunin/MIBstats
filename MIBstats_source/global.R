library(shinyBS)

app_info <- list(name = "MIBstats")

next_button_color <- "#286d11"
skip_button_color <- "#072642"

source("server/server_volcano_plot.R")
source("global/global_two_groups_test_results_page.R")
source("global/global_two_groups_test_data_download.R")
source("global/global_two_groups_test_data_one_variable_statistics_download.R")
source("global/global_handling_na_zero_inf_values.R")
source("global/global_class_labels_summary_box.R")

source("server/preprocessing_pca_module.R")
source("global/global_preprocessing_sorting_delete_items.R")
source("global/global_preproc_snv_plots.R")

source("global/global_two_groups_fold_changes_results_page.R")

source("server/server_viewing_rank_product_results.R")
source("server/results_pages_selectinput_module.R")
source("server/server_data_loading_module.R")
source("server/server_viewing_loaded_data_module.R")
source("server/server_handling_na_zero_inf_values_module.R")
source("server/server_preprocessing_sorting_module.R")

source("server/server_preprocessing_filtering_module.R")
source("server/server_preprocessing_snv_module.R")

source("server/server_viewing_iga_results_module.R")
source("server/server_running_iga_module.R")
source("server/server_viewing_preprocessed_data_module.R")
source("server/server_setting_up_two_groups_comparison_module.R")
source("server/server_viewing_shapiro_results_module.R")
source("server/server_load_r_workspace.R")
source("server/server_save_r_workspace.R")
source("server/server_how_to_use_the_app.R")
source("server/server_help_on_statistical_methods.R")
source("server/server_about_the_app.R")
source("server/server_triggering_statistical_test_module.R")
source("server/server_help_box_module.R")
source("server/server_image_format_selection_module.R")


