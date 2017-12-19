# _Module for viewing preprocessed data

viewing_preprocessed_data_moduleUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    
    tags$h4("Table of preprocessed data"),
    actionButton(ns("view_preprocessed_data_table_button"), "View preprocessed data as a table", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("view_preprocessed_data_table_button"), title = "Click here to view the preprocessed data as a table", placement = "left", trigger = "hover"),
    uiOutput(ns("results_table_uioutput")),
    tags$hr(),
    tags$h4("Confirming the changes"),
    actionButton(ns("preproc_move_to_two_groups_test_button"), "Proceed to statistical tests using the preprocessed data", style = paste0("color: ", next_button_color)),
    bsTooltip(id = ns("preproc_move_to_two_groups_test_button"), title = "Click here to proceed to statistical tests with the preprocessed data", placement = "left", trigger = "hover"),
    actionButton(ns("preproc_cancel_button"), "Cancel the preprocessing and proceed to statistical tests using unprocessed data", style = paste0("color: ", skip_button_color)),
    bsTooltip(id = ns("preproc_move_to_two_groups_test_button"), title = "Click here to cancel the preprocessing and revert to unprocessed data", placement = "left", trigger = "hover"),
    help_box_moduleUI(ns("help_viewing_preprocessed_data"))
  )
}

viewing_preprocessed_data_module <- function(input, output, session, t1_f_reactive){
  ns <- session$ns
  
  callModule(help_box_module, "help_viewing_preprocessed_data")
  
  t1_f <- reactiveValues(a = NULL)
  output_t1_f <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
  
  pp_processed_data_viewer1 <- reactiveValues(a = NULL)
  pp_processed_data_viewer1$a <- preprocessing_processed_data_viewer()
  
  observe({
    t1_f_observed <- t1_f_reactive()
    if(!is.null(t1_f_observed)) {
      isolate({
        if(!is.null(t1_f_observed@df)) {
          t1_f$a <- t1_f_observed
          reset_preprocessed_data_viewer()
        }
      })
    }
  })
  
  reset_preprocessed_data_viewer <- function() {
    # _Initialises the object for viewing preprocessed data
    pp_processed_data_viewer1$a@t <<- t1_f$a
    pp_processed_data_viewer1$a@l$merged_frame <<- get_merged_frame(t1_f$a)
    pp_processed_data_viewer1$a@panel_tracking$table_visible <<- FALSE
    pp_processed_data_viewer1$a@panel_tracking$pca_visible <<- FALSE
  }
  
  
  
  observe({
    if(pp_processed_data_viewer1$a@panel_tracking$table_visible) {
      isolate({
        output$results_table_uioutput <- renderUI({
          tagList(
            div(dataTableOutput(ns("preproc_results_table")), style = "font-size:80%")
          )
        })
      })
    } else {
      output$results_table_uioutput <- renderUI({})
    }
  })
  
  
  
  preprocessed_data_viewer_update_toggle_button <- function(switch_variable_name_f, button_name_f) {
    pp_processed_data_viewer1$a <<- update_toggle_button(pp_processed_data_viewer1$a, switch_variable_name_f, button_name_f)
  }
  
  observeEvent(input$view_preprocessed_data_table_button, {
    isolate({
      preprocessed_data_viewer_update_toggle_button("table_visible", ns("view_preprocessed_data_table_button"))
      if(pp_processed_data_viewer1$a@panel_tracking$table_visible) {
        output$preproc_results_table <- DT::renderDataTable(pp_processed_data_viewer1$a@l$merged_frame)
      }
    })
  })
  
  
  
  observeEvent(input$preproc_move_to_two_groups_test_button, {
    isolate({
      output_t1_f$a <- t1_f$a
      output_t1_f$counter <- output_t1_f$counter + 1
    })
  })
  
  
  observeEvent(input$preproc_cancel_button, {
    isolate({
      output_t1_f$skip_counter <- output_t1_f$skip_counter + 1
    })
  })
  
  
  out_reactive <- reactive({list(output_t1_f$counter, output_t1_f$a, output_t1_f$skip_counter)})
  return(out_reactive)
}

