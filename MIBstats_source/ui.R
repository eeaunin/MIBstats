source("ui/ui_jscode_disabling_navbar_tabs.R")

library(shinyjs)
library(shinythemes)
library(plotly)
library(ggfortify)
library(ggplot2)
library(DT)


if (interactive()) {
  shinyUI(fluidPage(
    
    theme = "bootstrap.css",
    
    tags$head(
      tags$style(HTML('#run{background-color:orange}'))
    ),

    useShinyjs(),
    extendShinyjs(text = jscode),
    inlineCSS(css),
    
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),

    mainPanel(

      imageOutput("app_logo", width = "auto", height = "auto"),
      tags$br(),
      navlistPanel(id="navbar",
                   "Files",
                   tabPanel("Load data",
                           
                            data_loading_moduleUI("loading_input_data")
                    ),
                   tabPanel(title = "View loaded data", value = "tab_view_loaded_data",
                            viewing_loaded_data_moduleUI("input_data")
                   ),
                   "Preprocessing",
                   tabPanel(title = "Handling NA, zero or infinite values", value = "tab_handling_values",
                            handling_na_zero_inf_values_moduleUI("handling_na_zero_inf")
                   ),
                   tabPanel(title = "PCA", value = "tab_pca",
                            preproc_pca_moduleUI("pca_module1")
                            
                   ),
                   tabPanel(title = "Sorting", value = "tab_sorting",
                            preprocessing_sorting_moduleUI("sorting1")
                   ),
                   tabPanel(title = "Filtering", value = "tab_filtering",
                            preprocessing_filtering_moduleUI("filtering1")
                   ),
                   tabPanel(title = "Scaling, normalisation and variance stabilisation", value = "tab_scaling_normalisation_etc",
                            preprocessing_snv_moduleUI("snv1")
                            
                   ),
                   tabPanel(title = "View preprocessed data", value = "tab_view_preprocessed_data",
                            
                            tags$h3("Viewing preprocessed data"),
                            tags$h4("PCA of preprocessed data"),
                            actionButton("view_preprocessed_data_pca_button", "View PCA plots of preprocessed data", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
                            bsTooltip(id = "view_preprocessed_data_pca_button", title = "Click here to view PCA plots of preprocessed data",
                                      placement = "left", trigger = "hover"),
                            uiOutput("viewing_preprocessed_data_pca2_uioutput"),
                            viewing_preprocessed_data_moduleUI("viewing_preprocessed_data1")
                            
                            
                   ),
                   "Statistical tests",
                   tabPanel(title = "Choose data for comparing two groups", value = "tab_choose_data_for_comparing_two_groups",
                            setting_up_two_groups_comparison_moduleUI("setting_up_two_groups_test1")
                   ),
                   tabPanel(title = "Shapiro-Wilk test", value = "tab_shapiro_wilk_test",
                            triggering_statistical_test_moduleUI("shapiro_test1")
                   ),
                   tabPanel(title = "F-test", value = "tab_f_test",
                            triggering_statistical_test_moduleUI("f_test1")
                   ),
                   tabPanel(title = "t-test", value = "tab_t_test",
                            triggering_statistical_test_moduleUI("t_test1")
                   ),
                   tabPanel(title = "Mann-Whitney U test", value = "tab_mann_whitney_u_test",
                            triggering_statistical_test_moduleUI("mann_whitney_u_test1")
                   ),
                   tabPanel(title = "Fold changes", value = "tab_fold_changes",
                            triggering_statistical_test_moduleUI("fold_changes1")
                            
                   ),
                   tabPanel(title = "Rank product", value = "tab_rank_product",
                            triggering_statistical_test_moduleUI("rank_product1")
                   ),
                   tabPanel(title = "Rank sum", value = "tab_rank_sum",
                            triggering_statistical_test_moduleUI("rank_sum1")
                   ),
                   tabPanel(title = "iGA", value = "tab_iga",
                            running_iga_moduleUI("iga1")
                   ),
                   tabPanel(title = "db-iGA", value = "tab_db_iga",
                            running_iga_moduleUI("db_iga1")
                   ),
                   "Test results",
                   tabPanel(title = "Shapiro-Wilk test results", value = "tab_view_shapiro_wilk_test_results",
                            viewing_shapiro_test_results_moduleUI("tgrp_shapiro_test")
                   ),
                   tabPanel(title = "F-test results", value = "tab_view_f_test_results",
                            two_groups_test_results_pageUI("tgrp_f_test")
                   ),
                   tabPanel(title = "t-test results", value = "tab_view_t_test_results",
                            two_groups_test_results_pageUI("tgrp_t_test")
                   ),
                   tabPanel(title = "Mann-Whitney U test results", value = "tab_view_mann_whitney_test_results",
                            two_groups_test_results_pageUI("tgrp_mann_whitney_test")
                   ),
                   tabPanel(title = "Fold changes results", value = "tab_view_fold_changes_results",
                            two_groups_fold_changes_results_pageUI("tgrp_fold_changes")
                   ),
                   tabPanel(title = "Volcano plot", value = "tab_volcano_plot",
                            volcano_plot_moduleUI("two_groups_test_volcano_plot")
                   ),
                   tabPanel(title = "Rank product results", value = "tab_view_rank_product_results",
                            results_pages_selectinput_moduleUI("rank_product_selectinput"),
                            rank_product_moduleUI("rank_product_results1")
                   ),
                   tabPanel(title = "Rank sum results", value = "tab_view_rank_sum_results",
                            results_pages_selectinput_moduleUI("rank_sum_selectinput"),
                            rank_product_moduleUI("rank_sum_results1")
                   ),
                   tabPanel(title = "iGA results", value = "tab_view_iga_results",
                            iga_results_moduleUI("iga1_results")
                   ),
                   tabPanel(title = "db-iGA results", value = "tab_view_db_iga_results",
                            iga_results_moduleUI("db_iga1_results")
                   ),
                   
                   "R workspace",
                   tabPanel(title = "Load R workspace", value = "tab_load_workspace", 
                            loading_r_workspace_moduleUI("loading_workspace1")
                   ),
                   tabPanel(title = "Save R workspace", value = "tab_save_workspace",
                            saving_r_workspace_moduleUI("saving_workspace1")
                   ),
        
                   "Help",
                   tabPanel(title = "Statistical methods", value = "tab_statistical_methods_help", 
                            statistical_methods_help_moduleUI("statistical_methods_help1")
                   ),
                   tabPanel(title = "How to use this app", value = "tab_how_to_use_the_app",
                            app_help_moduleUI("app_help1")
                   ),
                   tabPanel(title = "About", value = "tab_about",
                            about_the_app_moduleUI("about1")
                   )
      )
    )
    
  ))
  
}
