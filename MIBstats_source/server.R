
options(shiny.maxRequestSize=100*1024^2)


library(plyr)
library(DT)
library(ggplot2)
library(tools)
library(shinyjs)
library(data.table)
library(rlist)

source("object_classes/panel_tracker_class.R")
source("object_classes/preprocessing_shared_class.R")
source("invalid_values_counter.R")
source("file_reading_utilities.R")
source("object_classes/classes_of_items.R")
source("object_classes/input_data_table_class.R")
source("object_classes/input_data_checker_class.R")
source("object_classes/two_groups_test_queue_class.R")
source("object_classes/two_groups_test_results_tracker_class.R")
source("object_classes/two_groups_test_result_class.R")
source("object_classes/two_groups_test_class.R")
source("object_classes/preprocessing_na_zero_inf_class.R")
source("object_classes/preprocessing_pca_class.R")
source("object_classes/preprocessing_filter_class.R")
source("object_classes/preprocessing_scaler_class.R")
source("object_classes/preprocessing_normaliser_class.R")
source("object_classes/preprocessing_variance_stabiliser_class.R")
source("object_classes/preprocessing_processed_data_viewer_class.R")
source("object_classes/rank_product_calculator_class.R")
source("object_classes/iga_calculator_class.R")

source("functions_for_two_groups_tests.R")

shinyServer(
  
  if (interactive()) {
 
    function(input, output, session) {
      source("general_purpose_functions.R")
      
      output$app_logo <- renderImage({
        return(list(
          src = "www/MIBstats_logo.png",
          contentType = "image/png",
          height = "40",
          width = "150",
          alt = "Picture file not found"
        ))
      }, deleteFile = FALSE)
      
      
      source(file.path("server", "server_initialise_ui.R"), local = TRUE)$value
      
      
      source(file.path("server", "initialise_variables.R"), local = TRUE)$value
      
      
      callModule(app_help_module, "app_help1")
      callModule(statistical_methods_help_module, "statistical_methods_help1")
      callModule(about_the_app_module, "about1")

      callModule(loading_r_workspace_module, "loading_workspace1")
      callModule(saving_r_workspace_module, "saving_workspace1")
      
      # _Loading input data
      source(file.path("server", "server_loading_input_data.R"), local = TRUE)$value
      
      source(file.path("object_classes/panel_tracker_class.R"), local = TRUE)$value
      
      
      # _The source files for setting up and running statistical tests are triggered from server_loading_input_data.R
      
      
    }
    
})
