# _Module for the sorting step of preprocessing

preprocessing_sorting_moduleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$h3("Sorting of data"),
    
    
    tags$h4("Removal of samples or variables"),
    fluidRow(
      column(6,
             actionButton(ns("sorting_remove_samples_by_list_button"), "Remove samples...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
             bsTooltip(id = ns("sorting_remove_samples_by_list_button"), title = "Click here to select samples for removal",
                       placement = "left", trigger = "hover")
      )
    ),
    
    uiOutput(ns("delete_samples_container")),
    
    fluidRow(
      column(6,
             actionButton(ns("sorting_remove_variables_by_list_button"), "Remove variables...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
             bsTooltip(id = ns("sorting_remove_variables_by_list_button"), title = "Click here to select variables for removal",
                       placement = "left", trigger = "hover")
      )
    ),
    uiOutput(ns("delete_variables_container")),
    
    tags$h4("Reordering samples"),
    actionButton(ns("sorting_reorder_samples_button"), "Reorder samples...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("sorting_reorder_samples_button"), title = "Click here to change the order of the samples in the data table",
              placement = "left", trigger = "hover"),
    uiOutput(ns("reorder_samples_container")),
    tags$h4("Modifying class labels"),
    actionButton(ns("sorting_modify_class_labels_button"), "Modify class labels...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("sorting_modify_class_labels_button"), title = "Click here to modify the class labels that are associated with samples",
              placement = "left", trigger = "hover"),
    uiOutput(ns("modify_class_labels_container")),
    
    tags$h4("Applying the changes"),
    actionButton(ns("sorting_reset_changes_button"), "Reset the previewed changes"),
    bsTooltip(id = ns("sorting_reset_changes_button"), title = "Click here to reset the previewed changes to their initial state",
              placement = "left", trigger = "hover"),
    actionButton(ns("sorting_apply_changes_button"), "Apply the previewed changes to the data and move to the next step of preprocessing", style = paste0("color: ", next_button_color)),
    bsTooltip(id = ns("sorting_apply_changes_button"), title = "Click here to apply the previewed changes to the input data and move to the next step of preprocessing",
              placement = "left", trigger = "hover"),
    actionButton(ns("sorting_skip_button"), "Move to the next step without making changes to the data", style = paste0("color: ", skip_button_color)),
    bsTooltip(id = ns("sorting_skip_button"), title = "Click here to skip this step and move to the next step of preprocessing",
              placement = "left", trigger = "hover"),
    help_box_moduleUI(ns("help_sorting"))
  )
}

preprocessing_sorting_module <- function(input, output, session, t1_f_reactive, current_session_name_f) {
  ns <- session$ns
  # _The server part of sorting data (a part of preprocessing)
  
  callModule(help_box_module, "help_sorting")
  
  delete_items_module_output_samples <- reactiveValues(a = NULL)
  delete_items_module_output_variables <- reactiveValues(a = NULL)
  output1_samples_old <- reactiveValues(a = NULL)
  output1_variables_old <- reactiveValues(a = NULL)
  
  load_sample_order_fileinput_placeholder <- reactiveValues(a = "No file selected")
  
  
  source(file.path("object_classes/preprocessing_sorter_class.R"), local = TRUE)$value
  pp_sorter1 <- reactiveValues(a = NULL)
  t1_f <- reactiveValues(a = input_data_table())
  pp_sorter1$a <- preprocessing_sorter()
  pp_sorter1$a@panel_tracking <- list()
  
  output_t1_f <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
  
  initialise_samples_reordering_trackers <- function(visibility_f) {
    # _Initialises variables that are used to keep track of the state of the sample reordering part of preprocessing
    pp_sorter1$a@l$dt_reordering <<- reactiveValues(in_frame = NULL, in_data = NULL, out_data = NULL, table_initialised = FALSE)
    pp_sorter1$a@l$df_sel <<- reactiveValues(in_frame = NULL, out_frame = NULL, out_switch = FALSE)
    pp_sorter1$a@l$empty_rows_in_output <<- TRUE
    pp_sorter1$a@panel_tracking$reorder_columns_visible <<- visibility_f
  }
  
  reset_samples_reordering_trackers <- function(visibility_f) {
    # _Resets variables that are used to keep track of the state of the sample reordering part of preprocessing
    pp_sorter1$a@l$dt_reordering$in_frame <<- NULL
    pp_sorter1$a@l$dt_reordering$in_data <<- NULL
    pp_sorter1$a@l$dt_reordering$out_data <<- NULL
    pp_sorter1$a@l$dt_reordering$table_initialised <<- FALSE
    pp_sorter1$a@l$df_sel$in_frame <<- NULL
    pp_sorter1$a@l$df_sel$out_frame <<- NULL
    pp_sorter1$a@l$df_sel$out_switch <<- FALSE
    pp_sorter1$a@l$empty_rows_in_output <<- TRUE
    pp_sorter1$a@panel_tracking$reorder_columns_visible <<- visibility_f
  }
  
  old_class_labels <- reactiveValues(a = NULL)
  
  initialise_sorter1 <- function() {
    # _Initialises pp_sorter1$a, which is the main object used for the sorting step of preprocessing
    pp_sorter1$a <- preprocessing_sorter()
    pp_sorter1$a@t <<- t1_f$a
    pp_sorter1$a <<- preproc_initialise_class_labels_vector(pp_sorter1$a, t1_f$a)
    
    pp_sorter1$a@panel_tracking$selected_samples <<- NULL
    pp_sorter1$a@panel_tracking$selected_variables <<- NULL
    pp_sorter1$a@l <<- list()
    pp_sorter1$a@l2 <<- list()
    
    init_col_names <<- colnames(t1_f$a@df)
    init_col_names <<- init_col_names[2:length(init_col_names)]
    pp_sorter1$a@l$reordering_tracker <<- reactiveValues(df_column_names = init_col_names, df_column_names_frame = NULL, result = init_col_names)
    
    pp_sorter1$a@l2$mcl_tracker <<- NULL
    pp_sorter1$a@l$df_deletion_tracker <<- reactiveValues(samples = t1_f$a@df, variables = t1_f$a@df)
    
    old_class_labels <- NULL
    
  }
  
  
  
  observe({
    t1_f_observed <- t1_f_reactive()
    if(!is.null(t1_f_observed)) {
      isolate({
        if(!is.null(t1_f_observed@df)) {
          t1_f$a <- t1_f_observed
          
          initialise_sorter1()
          
          initial_pp_sorter1_a <- pp_sorter1$a

          delete_items_module_output_samples$a <- callModule(preprocessing_sorting_delete_items, "delete_samples", reactive({initial_pp_sorter1_a}), current_session_name_f)
          delete_items_module_output_variables$a <- callModule(preprocessing_sorting_delete_items, "delete_variables", reactive({initial_pp_sorter1_a}), current_session_name_f)
          
          initialise_sorter1_visibility(FALSE)
          reset_samples_reordering_trackers(FALSE)
          
        }
      })
    }
  })
  
  initialise_sorter1_visibility <- function(visibility_f) {
    # _Toggles the visibility of conditional panels on the UI page of the sorting step of preprocessing
    pp_sorter1$a@panel_tracking$remove_samples_visible <<- visibility_f
    pp_sorter1$a@panel_tracking$remove_variables_visible <<- visibility_f
    pp_sorter1$a@panel_tracking$reorder_columns_visible <<- visibility_f
    pp_sorter1$a@panel_tracking$modify_class_labels_visible <<- visibility_f
    pp_sorter1$a@panel_tracking$create_new_class_label_visible <<- visibility_f
    pp_sorter1$a@panel_tracking$load_sample_order_visible <<- visibility_f
    
    
  }
  
  preprocessing_sorting_initialise_for_opening_the_page <- function() {
    initialise_sorter1()
    initialise_samples_reordering_trackers(FALSE)
    initialise_sorter1_visibility(FALSE)
    
    output1_samples <<- list(pp_sorter1$a)
    output1_variables <<- list(pp_sorter1$a)
    
  }
  
  preprocessing_sorting_initialise_for_opening_the_page()
  initialise_sorter1_visibility(FALSE)
  
  initialise_mcl_tracker <- function() {
    # _Initialises variables that are used to keep track of the "modifying class labels" (mcl) part of preprocessing
    pp_sorter1$a@l2$mcl_tracker <<- reactiveValues(samples_frame = NULL, class_labels_frame = NULL, unique_class_labels = NULL, result = NULL, samples_dict = NULL)
  }
  
  sorting_update_toggle_button <- function(switch_variable_name_f, button_name_f) {
    pp_sorter1$a <<- update_toggle_button(pp_sorter1$a, switch_variable_name_f, button_name_f)
  }
  
  sorting_set_toggle_button_to_false <- function(switch_variable_name_f, button_name_f) {
    pp_sorter1$a <<- set_toggle_button_to_false(pp_sorter1$a, switch_variable_name_f, button_name_f)
  }
  
  observe({
    if(pp_sorter1$a@panel_tracking$remove_samples_visible) {
      isolate({
        output$delete_samples_container <- renderUI({
          tagList(
            preprocessing_sorting_delete_itemsUI(ns("delete_samples"))
          )
        })
      })
    } else ({
      isolate({
        output$delete_samples_container <- renderUI({})
      })
    })
  })
  
  observe({
    if(pp_sorter1$a@panel_tracking$remove_variables_visible) { 
      isolate({
        output$delete_variables_container <- renderUI({
          tagList(
            preprocessing_sorting_delete_itemsUI(ns("delete_variables"))
          )
        })
      })
    } else ({
      isolate({
        output$delete_variables_container <- renderUI({})
      })
    })
  })
  
  observe({
    if(pp_sorter1$a@panel_tracking$reorder_columns_visible) {
      isolate({
        output$reorder_samples_container <- renderUI({
          tagList(
            uiOutput(ns("sorting_reorder_samples_tables_container")),
            actionButton(ns("sorting_reorder_samples_apply_button"), "Apply this ordering to the data sorting preview"),
            bsTooltip(id = ns("sorting_reorder_samples_apply_button"), title = "Click here to apply the ordering of columns in the table above to the data sorting preview",
                      placement = "left", trigger = "hover")
          )
        })
      })
    } else ({
      isolate({
        output$reorder_samples_container <- renderUI({})
      })
    })
  })
  
  observe({
    if(pp_sorter1$a@panel_tracking$modify_class_labels_visible) {
      isolate({
        output$modify_class_labels_container <- renderUI({
          tagList(
            fluidRow(
              column(4,
                     tags$h5("New class labels"),
                     div(dataTableOutput(ns("sorting_mcl_class_labels_table")), style = "font-size:80%")),
              column(4,
                     tags$h5("Samples"),
                     div(dataTableOutput(ns("sorting_mcl_samples_table")), style = "font-size:80%")),
              column(4,
                     tags$h5("Old class labels"),
                     div(dataTableOutput(ns("sorting_mcl_old_class_labels_table")), style = "font-size:80%"))
            ),
            selectInput(ns("sorting_mcl_class_labels_selectinput"), "Selected class label", pp_sorter1$a@l2$mcl_tracker$unique_class_labels),
            actionButton(ns("sorting_mcl_apply_class_label_button"), "Apply the selected class label to the selected sample(s)"),
            bsTooltip(id = ns("sorting_mcl_apply_class_label_button"), title = "Click here to apply the selected class label to the selected sample(s)",
                      placement = "left", trigger = "hover"),
            actionButton(ns("sorting_mcl_create_new_class_label_button"), "Create a new class label...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("sorting_mcl_create_new_class_label_button"), title = "Click here to create a new class label",
                      placement = "left", trigger = "hover"),
            uiOutput(ns("new_class_label_panel_container")),
            
            actionButton(ns("sorting_modify_class_labels_reset_button"), "Reset the class labels to initial state"),
            bsTooltip(id = ns("sorting_modify_class_labels_reset_button"), title = "Click here to discard the changes and reset the class labels to their initial state",
                      placement = "left", trigger = "hover"),
            actionButton(ns("sorting_modify_class_labels_apply_button"), "Apply the new class labels to the data sorting preview"),
            bsTooltip(id = ns("sorting_modify_class_labels_apply_button"), title = "Click here to apply the new class labels to the data sorting preview",
                      placement = "left", trigger = "hover")
          )
        })
      })
    } else ({
      isolate({
        output$modify_class_labels_container <- renderUI({})
      })
    })
  })
  
  
  observe({
    if(pp_sorter1$a@panel_tracking$create_new_class_label_visible) {
      isolate({
        output$new_class_label_panel_container <- renderUI({
          wellPanel(
            textInput(ns("sorting_mcl_add_new_class_label_textinput"), "Please enter the name of the new class label:"),
            actionButton(ns("sorting_mcl_add_new_class_label_button"), "Add to the list of class labels"),
            bsTooltip(id = ns("sorting_mcl_add_new_class_label_button"), title = "Click here to add the new class label to the list of class labels",
                      placement = "left", trigger = "hover")
          )
        })
      })
    } else ({
      isolate({
        output$new_class_label_panel_container <- renderUI({})
      })
    })
  })
  

  
  observeEvent(input$sorting_remove_samples_by_list_button, {
    isolate({
      sorting_update_toggle_button("remove_samples_visible", ns("sorting_remove_samples_by_list_button"))
      
    })
  })
  
  observeEvent(input$sorting_remove_variables_by_list_button, {
    isolate({
      sorting_update_toggle_button("remove_variables_visible", ns("sorting_remove_variables_by_list_button"))
    })
  })
  
  initialise_samples_reordering_tables <- function(reset_f) {
    # _Initialises variables and UI components that are used in the sample reordering part of preprocessing
    
    pp_sorter1$a@l$df_sel$out_frame <- NULL
    
    pp_sorter1$a <- sorter_update_sample_names_for_reordering(pp_sorter1$a, reset_f)
    
    output$sorting_reorder_samples_tables_container <- renderUI({
      
      tagList(
        fluidRow(
          column(6,
                 tags$h5("Old ordering"),
                 div(dataTableOutput(ns("sorting_reorder_samples_table_input")), style = "font-size:80%")),
          column(6,
                 tags$h5("New ordering"),
                 div(dataTableOutput(ns("sorting_reorder_samples_table_output")), style = "font-size:80%"))
        ),
        actionButton(ns("reordering_samples_copy_to_new_button"), label = "Move from old list to new list"),
        bsTooltip(id = ns("reordering_samples_copy_to_new_button"), title = "Click here to move the selected sample(s) from the old sample order list to the new sample order list",
                  placement = "left", trigger = "hover"),
        actionButton(ns("reordering_samples_reset_button"), label = "Reset the sample reordering preview"),
        bsTooltip(id = ns("reordering_samples_reset_button"), title = "Click here to discard the changes and reset the sample reordering preview",
                  placement = "left", trigger = "hover"),
        actionButton(ns("reordering_samples_panel_button"), label = "Load the order of samples from a file...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
        bsTooltip(id = ns("reordering_samples_panel_button"), title = "Click here to load the order of samples from a plain text file",
                  placement = "left", trigger = "hover"),
        uiOutput(ns("loading_order_from_file_container"))
      )
      
    })
    pp_sorter1$a <- sorter_reordering_prepare_input_table(pp_sorter1$a)
    
    output$sorting_reorder_samples_table_input <- DT::renderDataTable({
      input$sorting_reorder_samples_table_input_rows_selected
      datatable(pp_sorter1$a@l$dt_reordering$in_data, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "multiple", selected = pp_sorter1$a@l$df_sel$in_frame))
    })
    
    pp_sorter1$a <- sorter_reordering_prepare_output_table(pp_sorter1$a)
    
    output$sorting_reorder_samples_table_output <- DT::renderDataTable({
      input$sorting_reorder_samples_table_output_rows_selected
      datatable(pp_sorter1$a@l$dt_reordering$out_data, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "multiple", selected = pp_sorter1$a@l$df_sel$out_frame))
    })
    
    pp_sorter1$a@l$dt_reordering$table_initialised <- TRUE
  }
  
  observeEvent(input$reordering_samples_panel_button, {
    isolate({
      sorting_update_toggle_button("load_sample_order_visible", ns("reordering_samples_panel_button"))
    })
  })
  
  
  
  observe({
    if(pp_sorter1$a@panel_tracking$load_sample_order_visible) {
      isolate({
        output$loading_order_from_file_container <- renderUI({
          tagList(
            wellPanel(
              helpText("Please select a .TXT file that contains the sample names in the preferred order. The file should be a plain text file without a header and have one sample name per row. The order of samples is determined by the order of sample names in the file (read from the top to the bottom of the file)."),
              fileInput(ns("load_sample_order_fileinput"), "File with the order of samples:", multiple = FALSE, accept = c("text", ".txt"), width = NULL, buttonLabel = "Browse...", placeholder = load_sample_order_fileinput_placeholder$a),
              actionButton(ns("reordering_samples_load_order_from_file_button"), label = "Load the order of samples from the file"),
              bsTooltip(id = ns("reordering_samples_load_order_from_file_button"), title = "Click here to load the order of samples from the selected file", placement = "left", trigger = "hover")
            )
          )
        })
      })
    } else ({
      isolate({
        if(!is.null(input$load_sample_order_fileinput)) {
          load_sample_order_fileinput_placeholder$a <- input$load_sample_order_fileinput$name
        }
        output$loading_order_from_file_container <- renderUI({})
      })
    })
  })
  
  check_sample_order_file <- function(in_data, old_sample_order) {
    # _Function to check if the file of new order of samples that was uploaded by the user has valid contents
    check_result <- "ok"
    
    if(length(in_data) != length(old_sample_order)) {
      check_result <- "Unequal number of samples in the currently loaded data and the sample names file."
    } else {
      for(old_item in old_sample_order) {
        if(!is.element(old_item, in_data)) {
          check_result <- "Sample name mismatch between the currently loaded data and the sample names file."
          break
        }
      }
    }
    return(check_result)
  }
  
  observeEvent(input$reordering_samples_load_order_from_file_button, {
    isolate({
      loaded_file <- input$load_sample_order_fileinput
      load_sample_order_fileinput_placeholder$a <- input$load_sample_order_fileinput$name 
      if(is.null(loaded_file)) {
        show_generic_error_message("There is no file selected in the file browser")
        return()
      }
      file_contents <- read_plain_text_file(input$load_sample_order_fileinput$datapath)
      samples_check_result <- check_sample_order_file(file_contents, df_get_sample_names(pp_sorter1$a@t))
      if(samples_check_result != "ok") {
        show_generic_error_message(paste("Loading of the new sample order failed. ", samples_check_result))
      } else {
        
        pp_sorter1$a <- sorter_apply_samples_order_loaded_from_file(pp_sorter1$a, file_contents)
        
        output$sorting_reorder_samples_table_input <- DT::renderDataTable(pp_sorter1$a@l$dt_reordering$in_frame, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "multiple"))
        output$sorting_reorder_samples_table_output <- DT::renderDataTable({
          input$sorting_reorder_samples_table_output_rows_selected
          datatable(pp_sorter1$a@l$dt_reordering$out_data, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "multiple"))
        })
        sorting_set_toggle_button_to_false("load_sample_order_visible", ns("reordering_samples_panel_button"))
        
        message_out("The order of samples was loaded from the file", "Sorting of samples")
      }
    })
  })
  
  observeEvent(input$sorting_reorder_samples_button, {
    isolate({
      sorting_update_toggle_button("reorder_columns_visible", ns("sorting_reorder_samples_button"))
      
      if(pp_sorter1$a@panel_tracking$reorder_columns_visible) {
        
        pp_sorter1$a@panel_tracking$remove_samples_visible <- FALSE
        updateActionButton(session, "sorting_remove_samples_by_list_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
        shinyjs::disable("sorting_remove_samples_by_list_button")
        
        colnames_comparison <- sorter_compare_sample_names_from_deletion_and_reordering_steps(pp_sorter1$a)
        
        if(!colnames_comparison | !pp_sorter1$a@l$dt_reordering$table_initialised) {
          reset_samples_reordering_trackers(TRUE)
          initialise_samples_reordering_tables(reset_f = FALSE)
        }
        
      } else {
        shinyjs::enable("sorting_remove_samples_by_list_button")
      }
      
    })
  })
  
  observeEvent(input$reordering_samples_copy_to_new_button, {
    isolate({
      new_ppsorter1 <- sorter_samples_reordering_move_cells(pp_sorter1$a)
      if(!is.null(new_ppsorter1)) {
        pp_sorter1$a <- sorter_samples_reordering_move_cells(pp_sorter1$a)
        output$sorting_reorder_samples_table_output <- DT::renderDataTable({
          input$sorting_reorder_samples_table_output_rows_selected
          datatable(pp_sorter1$a@l$dt_reordering$out_data, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "multiple", selected = pp_sorter1$a@l$df_sel$out_frame))
        })
      }
    })
  })
  
  observe({
    sel <- input$sorting_reorder_samples_table_input_rows_selected
    isolate({
      if(!is.null(sel)) {
        pp_sorter1$a@l$df_sel$in_frame <- input$sorting_reorder_samples_table_input_rows_selected
      }
    })
  })
  
  
  observe({
    if(!is.null(input$sorting_reorder_samples_table_output_rows_selected) & !is.null(input$sorting_reorder_samples_table_output_row_last_clicked)) {
      isolate({
        pp_sorter1$a <- sorter_reordering_limit_selected_rows(pp_sorter1$a, input$sorting_reorder_samples_table_output_rows_selected, input$sorting_reorder_samples_table_output_row_last_clicked)
      })
    }

  })
  
  observeEvent(input$sorting_reorder_samples_apply_button, {
    isolate({
      pp_sorter1$a@l$empty_rows_in_output <- FALSE
      for (i in 1:nrow(pp_sorter1$a@l$dt_reordering$out_data)) {
        if(pp_sorter1$a@l$dt_reordering$out_data[i, 1] == "-") {
          pp_sorter1$a@l$empty_rows_in_output <- TRUE
        }
      }
      if(!pp_sorter1$a@l$empty_rows_in_output) {
        pp_sorter1$a@l$reordering_tracker$result <- pp_sorter1$a@l$dt_reordering$out_data[, 1]
        message_out("The new ordering of samples was applied to the preview data", "Reordering of samples")
      } else {
        message_out("The new order list has to be complete before it can be applied to samples", "Reordering of samples")
      }
    })
  })
  
  observeEvent(input$reordering_samples_reset_button, {
    isolate({
      reset_samples_reordering_trackers(TRUE)
      initialise_samples_reordering_tables(reset_f = TRUE)
      pp_sorter1$a@l$reordering_tracker$result <- colnames(pp_sorter1$a@l$df_deletion_tracker$samples[2:ncol(pp_sorter1$a@l$df_deletion_tracker$samples)])
      message_out("The ordering of samples in the preview data was reset to its initial state", "Reordering of samples")
    })
  })
  

  observe({
    output1_samples <- delete_items_module_output_samples$a()
    if(!is.null(output1_samples)) {
      isolate({
        
        pp_sorter1$a <- sorter_update_after_deletion_of_items(pp_sorter1$a, "samples", output1_samples, output1_samples_old$a)
        output1_samples_old$a <- output1_samples
        
      })
    }
    
  })
  
  observe({
    output1_variables <- delete_items_module_output_variables$a()
    if(!is.null(output1_variables)) {
      isolate({
        pp_sorter1$a <- sorter_update_after_deletion_of_items(pp_sorter1$a, "variables", output1_variables, output1_variables_old$a)
        output1_variables_old$a <- output1_variables
        
      })
    }
  })
  
  observeEvent(input$sorting_apply_changes_button, {
    isolate({
      combined_results_list <- sorter_get_combined_results(pp_sorter1$a)
      t1_f$a@df <- combined_results_list$combined_frame
      t1_f$a@class_labels <- combined_results_list$class_labels_frame
      
      
      
      output_t1_f$a <- t1_f$a
      output_t1_f$counter <- output_t1_f$counter + 1
      
      message_out("The preprocessing steps of data sorting were applied to input data", "Sorting")
    })
  })
  
  observeEvent(input$sorting_skip_button, {
    isolate({
      output_t1_f$skip_counter <- output_t1_f$skip_counter + 1
    })
  })
  
  initialise_class_labels_selectinput <- function() {
    # _Initialises the selectinput that is used to choose class labels on the sorting page of preprocessing
    updateSelectInput(session, "sorting_mcl_class_labels_selectinput", choices = pp_sorter1$a@l2$mcl_tracker$unique_class_labels)
  }
  
  initialise_mcl <- function() {
    # _Initialises the "modifying class labels" (mcl) components of the sorting page of preprocessing
    
    pp_sorter1$a <- initialise_mcl_variables(pp_sorter1$a)
    
    output$sorting_mcl_samples_table <- DT::renderDataTable({
      datatable(pp_sorter1$a@l2$mcl_tracker$samples_frame, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "none"))
    })
    
    output$sorting_mcl_class_labels_table <- DT::renderDataTable({
      datatable(pp_sorter1$a@l2$mcl_tracker$class_labels_frame, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "multiple"))
    })
    
    old_class_labels$a <- pp_sorter1$a@l2$mcl_tracker$class_labels_frame
    
    output$sorting_mcl_old_class_labels_table <- DT::renderDataTable({
      datatable(old_class_labels$a, rownames= FALSE, options = list(paging = FALSE), selection = list(mode = "none"))
    })
    
    initialise_class_labels_selectinput()
    
  }
  
  observeEvent(input$sorting_modify_class_labels_button, {
    isolate({
      sorting_update_toggle_button("modify_class_labels_visible", ns("sorting_modify_class_labels_button"))
      if(is.null(pp_sorter1$a@l2$mcl_tracker)) {
        initialise_mcl_tracker()
        initialise_mcl()
      }
      
    })
  })
  
  observeEvent(input$sorting_mcl_apply_class_label_button, {
    isolate({
      selectinput_class_label <- input$sorting_mcl_class_labels_selectinput
      
      cl_table_selected <- input$sorting_mcl_class_labels_table_rows_selected
      
      pp_sorter1$a@l2$mcl_tracker$class_labels_frame[cl_table_selected, 1] <- selectinput_class_label
    })
  })
  
  observeEvent(input$sorting_modify_class_labels_reset_button, {
    isolate({
      initialise_mcl_tracker()
      initialise_mcl()
    })
  })
  
  observeEvent(input$sorting_mcl_create_new_class_label_button, {
    isolate({
      sorting_update_toggle_button("create_new_class_label_visible", ns("sorting_mcl_create_new_class_label_button"))
    })
  })
  
  observeEvent(input$sorting_mcl_add_new_class_label_button, {
    isolate({
      new_class_label_name <- input$sorting_mcl_add_new_class_label_textinput
      pp_sorter1$a <- mcl_add_new_class_label(pp_sorter1$a, new_class_label_name)
      updateSelectInput(session, ns("sorting_mcl_class_labels_selectinput"), choices = pp_sorter1$a@l2$mcl_tracker$unique_class_labels)
      
    })
  })
  
  observeEvent(input$sorting_modify_class_labels_apply_button, {
    isolate({
      pp_sorter1$a <- get_mcl_result(pp_sorter1$a)
      new_class_labels_frame <- sorter_class_labels_vector_to_class_labels_frame(pp_sorter1$a)
      message_out("The class labels of the preview data were updated", "Sorting")
    })
  })
  
  
  
  set_all_toggle_buttons_to_false <- function() {
    sorting_set_toggle_button_to_false("remove_samples_visible", ns("sorting_remove_samples_by_list_button"))
    sorting_set_toggle_button_to_false("remove_variables_visible", ns("sorting_remove_variables_by_list_button"))
    sorting_set_toggle_button_to_false("modify_class_labels_visible", ns("sorting_modify_class_labels_button"))
    sorting_set_toggle_button_to_false("reorder_columns_visible", ns("sorting_reorder_samples_button"))
    sorting_set_toggle_button_to_false("load_sample_order_visible", ns("reordering_samples_panel_button"))
    sorting_set_toggle_button_to_false("create_new_class_label_visible", ns("sorting_mcl_create_new_class_label_button"))
  }
  
  observeEvent(input$sorting_reset_changes_button, {
    isolate({
      preprocessing_sorting_initialise_for_opening_the_page()
      
      set_all_toggle_buttons_to_false()
      
      
      initial_pp_sorter1_a <- pp_sorter1$a
      
      delete_items_module_output_samples$a <- callModule(preprocessing_sorting_delete_items, "delete_samples", reactive({initial_pp_sorter1_a}), current_session_name_f)
      delete_items_module_output_variables$a <- callModule(preprocessing_sorting_delete_items, "delete_variables", reactive({initial_pp_sorter1_a}), current_session_name_f)
      
      shinyjs::enable("sorting_remove_samples_by_list_button")
      
      message_out("Any changed values on the sorting page were reset to their initial state", "Sorting")
      
    })
  })
  
  
  out_reactive <- reactive({list(output_t1_f$counter, output_t1_f$a, output_t1_f$skip_counter)})
  return(out_reactive)
  
}

