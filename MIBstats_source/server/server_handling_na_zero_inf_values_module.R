# _The module for the preprocessing step for handling NA, zero and infinite values

handling_na_zero_inf_values_moduleUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    tags$h3("Handling NA, zero or infinite values"),
    tags$h4("NA, zero or infinite values handling options"),
    helpText("Please choose how to handle NA, zero and infinite values in the data:"),
    wellPanel(
      radioButtons(ns("zeros_radio_button"), "Preprocessing to handle zero values:",
                   c("None" = "zeros_do_nothing_radiobutton",
                     "Replace with NAs" = "change_zeros_to_NA_radiobutton",
                     "Substitute with minimum detected value" = "substitute_zeros_with_detection_limit_radiobutton"))
    ),
    
    wellPanel(
      radioButtons(ns("minus_inf_radio_button"), "Preprocessing to handle -Inf values:",
                   c("None" = "minus_inf_do_nothing_radiobutton",
                     "Replace with NAs" = "change_minus_inf_to_NA_radiobutton",
                     "Substitute with minimum detected value" = "substitute_minus_inf_with_detection_limit_radiobutton"))
    ),
    wellPanel(
      radioButtons(ns("plus_inf_radio_button"), "Preprocessing to handle +Inf values:",
                   c("None" = "plus_inf_do_nothing_radiobutton",
                     "Replace with NAs" = "change_plus_inf_to_NA_radiobutton",
                     "Substitute with maximum detected value" = "substitute_plus_inf_with_detection_limit_radiobutton"))
    ),
    wellPanel(
      radioButtons(ns("NAs_radio_button"), "Preprocessing to handle NA values:",
                   c("None" = "NAs_do_nothing_radiobutton",
                     "Remove variables if the percentage of NA values exceeds a specific threshold" = "NAs_remove_radiobutton",
                     "Substitute with zeros" = "NAs_substitute_with_zeros_radiobutton",
                     "Substitute with minimum detected value" = "NAs_substitute_with_minimum_detected_value_radiobutton",
                     "Substitute with maximum detected value" = "NAs_substitute_with_maximum_detected_value_radiobutton")),
      uiOutput(ns("preprocessing_allowed_NAs_percentage"))
    ),
    tags$hr(),
    tags$h4("Settings of minimum and maximum detected values"),
    actionButton(ns("minimum_detected_value_settings_button"), "Minimum detected value settings...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("minimum_detected_value_settings_button"), title = "Click here to change the settings of the minimum detected value", placement = "left", trigger = "hover"),
    uiOutput(ns("minimum_detected_value_settings_button_uioutput")),
    tags$hr(),
    actionButton(ns("maximum_detected_value_settings_button"), "Maximum detected value settings...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("maximum_detected_value_settings_button"), title = "Click here to change the settings of the maximum detected value", placement = "left", trigger = "hover"),
    uiOutput(ns("maximum_detected_value_settings_button_uioutput")),
    tags$hr(),
    tags$h4("Applying the changes"),
    actionButton(ns("preprocessing_NA_zero_inf_apply_to_preview_button"), "Apply the selected changes to preview data"),
    bsTooltip(id = ns("preprocessing_NA_zero_inf_apply_to_preview_button"), title = "Click here to apply the changes to the preview copy of the data", placement = "left", trigger = "hover"),
    actionButton(ns("show_handling_values_preview_button"), "View preview of this preprocessing step", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("show_handling_values_preview_button"), title = "Click here to view preview of this preprocessing step",
              placement = "left", trigger = "hover"),
    uiOutput(ns("preprocessing_NA_zero_inf_preview")),
    actionButton(ns("preprocessing_NA_zero_inf_reset_preview_button"), "Reset the preview of this step"),
    bsTooltip(id = ns("preprocessing_NA_zero_inf_reset_preview_button"), title = "Click here to reset the preview of this step", placement = "left", trigger = "hover"),
    actionButton(ns("preprocessing_NA_zero_inf_apply_button"), "Apply the previewed changes to input data and proceed to next preprocessing step", style = paste0("color: ", next_button_color)),
    bsTooltip(id = ns("preprocessing_NA_zero_inf_apply_button"), title = "Click here to apply the changes and proceed to next preprocessing step", 
              placement = "left", trigger = "hover"),
    actionButton(ns("preprocessing_NA_zero_inf_skip_button"), "Move to the next step without making changes to the data", style = paste0("color: ", skip_button_color)),
    bsTooltip(id = ns("preprocessing_NA_zero_inf_skip_button"), title = "Click here to skip this step and move to the next step of preprocessing",
              placement = "left", trigger = "hover"),
    help_box_moduleUI(ns("help_na_zero_inf"))
  )
  
}

handling_na_zero_inf_values_module <- function(input, output, session, t1_f_reactive) {
  
  ns <- session$ns
  
  callModule(help_box_module, "help_na_zero_inf")
  
  preproc_limits_tracker <- reactiveValues(minus_inf = 0, plus_inf = 0)
  
  generate_na_zero_inf_panel_component <- function(pre_name_f) {
    heading_str_f <- ""
    limit_str_f <- ""
    if(pre_name_f=="zeros" | pre_name_f == "minus_inf") {
      heading_str_f <- "Please specify the lower detection limit of the measuring instrument"
      limit_str_f <- "minimum"
    } else if (pre_name_f=="plus_inf") {
      heading_str_f <- "Please specify the upper detection limit of the measuring instrument"
      limit_str_f <- "maximum"
    }
    html_output_str_f <- paste(pre_name_f, "_current_detection_limit", sep="")
    numeric_input_str_f <- paste(pre_name_f, "_detection_limit_numericinput", sep="")
    set_detection_limit_str_f <- paste(pre_name_f, "_set_detection_limit_to_specified_value", sep="")
    estimate_detection_limit_from_data_str_f <- paste(pre_name_f, "_estimate_detection_limit_from_data", sep="")
    panel_list_f <- list(p(strong(heading_str_f)),
                         htmlOutput(ns(html_output_str_f)),
                         numericInput(ns(numeric_input_str_f), label = "New detection limit:", value = 0),
                         actionButton(ns(set_detection_limit_str_f), "Set the detection limit to the specified value"),
                         bsTooltip(id = ns(set_detection_limit_str_f), title = "Click here to set the detection limit of the measuring instrument that was used to collect the input data", 
                                   placement = "left", trigger = "hover"),
                         actionButton(ns(estimate_detection_limit_from_data_str_f), paste("Automatically estimate the detection limit based on the ", limit_str_f, " value in the loaded data", sep="")),
                         bsTooltip(id = ns(estimate_detection_limit_from_data_str_f), title = "Click here to get an automatic estimation of the detection limit of the measuring instrument, based on the loaded data file", 
                                   placement = "left", trigger = "hover"))
    return(panel_list_f)
  }
  
  na_inf_upper_limit_panel <- generate_na_zero_inf_panel_component("plus_inf")
  na_inf_lower_limit_panel <- generate_na_zero_inf_panel_component("minus_inf")
  
  pp_na_zero_inf1 <- reactiveValues(a = NULL)
  output_t1_f <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
  preproc_NA_zero_inf <- preprocessing_settings_NA_zero_inf_values()
  
  t1_f <- reactiveValues(a = NULL)
  displaying_frame_f <- reactiveValues(a = NULL)
  na_inf_changes_applied_to_preview_data <- reactiveValues(a = FALSE)
  na_inf_panel_visible <- reactiveValues(max_val_settings = FALSE, min_val_settings = FALSE, results_preview = FALSE)
  
  na_threshold <- reactiveValues(a = 25)
  
  
  shinyjs::disable("show_handling_values_preview_button")
  shinyjs::disable("preprocessing_NA_zero_inf_apply_button")
  
  observe({
    t1_f_observed <- t1_f_reactive()
    if(!is.null(t1_f_observed)) {
      isolate({
        if(!is.null(t1_f_observed@df)) {
          t1_f$a <- t1_f_observed
          
          initialise_pp_na_zero_inf_object()
        }
      })
    }
  })
  
  observe({
    if(input$NAs_radio_button == "NAs_remove_radiobutton") {
      isolate({
        output$preprocessing_allowed_NAs_percentage <- renderUI({
          sliderInput(ns("max_allowed_na_percent_preprocessing"), "Maximum % of allowed NA values in input data:", min = 0, max = 100, value = na_threshold$a)
        })
      })
    } else {
      isolate({
        output$preprocessing_allowed_NAs_percentage <- renderUI({})
      })
    }
  })
  
  observeEvent(input$max_allowed_na_percent_preprocessing, {
    isolate({
      na_threshold$a <- input$max_allowed_na_percent_preprocessing
    })
  })
  
  
  update_lowest_detected_value_by_data <- function() {
    # _Estimates the lower detection limit of the measuring instrument by the lowest value in input data
    pp_na_zero_inf1$a@t <<- t1_f$a
    new_min_limit <<- get_df_minimum_value(pp_na_zero_inf1$a)
    preproc_limits_tracker$minus_inf <<- new_min_limit
    updateNumericInput(session, "minus_inf_detection_limit_numericinput", value = new_min_limit)
  }
  
  update_highest_detected_value_by_data <- function() {
    # _Estimates the upper detection limit of the measuring instrument by the highest value in input data
    pp_na_zero_inf1$a@t <<- t1_f$a
    new_max_limit <<- get_df_maximum_value(pp_na_zero_inf1$a)
    preproc_limits_tracker$plus_inf <<- new_max_limit
    updateNumericInput(session, "plus_inf_detection_limit_numericinput", value = new_max_limit)
  }
  
  update_preproc_NA_zero_inf <- function() {
    # _Reads the state of the user interface elements of this preprocessing page into a set of variables
    preproc_NA_zero_inf_new <- preprocessing_settings_NA_zero_inf_values()
    preproc_NA_zero_inf_new@NA_values_choice <- -1
    if(input$NAs_radio_button == "NAs_do_nothing_radiobutton") {
      preproc_NA_zero_inf_new@NA_values_choice <- 0
    } else if(input$NAs_radio_button == "NAs_remove_radiobutton") {
      preproc_NA_zero_inf_new@NA_values_choice <- 1
      
    } else if(input$NAs_radio_button == "NAs_substitute_with_zeros_radiobutton") {
      preproc_NA_zero_inf_new@NA_values_choice <- 2
    } else if(input$NAs_radio_button == "NAs_substitute_with_minimum_detected_value_radiobutton") {
      preproc_NA_zero_inf_new@NA_values_choice <- 3
    } else if(input$NAs_radio_button == "NAs_substitute_with_maximum_detected_value_radiobutton") {
      preproc_NA_zero_inf_new@NA_values_choice <- 4
    }
    
    if(!is.null(input$max_allowed_na_percent_preprocessing) & (preproc_NA_zero_inf_new@NA_values_choice == 1)) {
      preproc_NA_zero_inf_new@NA_values_threshold <- input$max_allowed_na_percent_preprocessing
    } else {
      preproc_NA_zero_inf_new@NA_values_threshold <- -1
    }
    
    if(input$zeros_radio_button == "zeros_do_nothing_radiobutton") {
      preproc_NA_zero_inf_new@zero_values_choice <- 0
    } else if(input$zeros_radio_button == "change_zeros_to_NA_radiobutton") {
      preproc_NA_zero_inf_new@zero_values_choice <- 1
    } else if(input$zeros_radio_button == "substitute_zeros_with_detection_limit_radiobutton") {
      preproc_NA_zero_inf_new@zero_values_choice <- 2
    }
    
    if(input$minus_inf_radio_button == "minus_inf_do_nothing_radiobutton") {
      preproc_NA_zero_inf_new@minus_inf_values_choice <- 0
    } else if(input$minus_inf_radio_button == "change_minus_inf_to_NA_radiobutton") {
      preproc_NA_zero_inf_new@minus_inf_values_choice <- 1
    } else if(input$minus_inf_radio_button == "substitute_minus_inf_with_detection_limit_radiobutton") {
      preproc_NA_zero_inf_new@minus_inf_values_choice <- 2
    }
    
    if(input$plus_inf_radio_button == "plus_inf_do_nothing_radiobutton") {
      preproc_NA_zero_inf_new@plus_inf_values_choice <- 0
    } else if(input$plus_inf_radio_button == "change_plus_inf_to_NA_radiobutton") {
      preproc_NA_zero_inf_new@plus_inf_values_choice <- 1
    } else if(input$plus_inf_radio_button == "substitute_plus_inf_with_detection_limit_radiobutton") {
      preproc_NA_zero_inf_new@plus_inf_values_choice <- 2
    }
    
    return(preproc_NA_zero_inf_new)
  }
  
  initialise_pp_na_zero_inf_object <- function() {
    pp_na_zero_inf1$a <<- preprocessing_na_zero_inf()
    pp_na_zero_inf1$a@t <<- t1_f$a
    
    update_preproc_NA_zero_inf()
    
    update_lowest_detected_value_by_data()
    update_highest_detected_value_by_data()
    na_inf_panel_visible$max_val_settings <<- FALSE
    na_inf_panel_visible$min_val_settings <<- FALSE
    na_inf_panel_visible$results_preview <<- FALSE
    output$minimum_detected_value_settings_button_uioutput <<- renderUI({})
    output$maximum_detected_value_settings_button_uioutput <<- renderUI({})
    output$preprocessing_NA_zero_inf_preview <<- renderUI({})
    updateActionButton(session, "minimum_detected_value_settings_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    updateActionButton(session, "maximum_detected_value_settings_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    updateActionButton(session, "show_handling_values_preview_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
  }
  
  
  observeEvent(input$minimum_detected_value_settings_button, {
    isolate({
      na_inf_panel_visible$min_val_settings <- !na_inf_panel_visible$min_val_settings
      if(na_inf_panel_visible$min_val_settings) {
        updateActionButton(session, "minimum_detected_value_settings_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "minimum_detected_value_settings_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$maximum_detected_value_settings_button, {
    isolate({
      na_inf_panel_visible$max_val_settings <- !na_inf_panel_visible$max_val_settings
      if(na_inf_panel_visible$max_val_settings) {
        updateActionButton(session, "maximum_detected_value_settings_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "maximum_detected_value_settings_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observe({
    if(na_inf_panel_visible$min_val_settings) {
      isolate({
        output$minimum_detected_value_settings_button_uioutput <- renderUI ({
          wellPanel(
            na_inf_lower_limit_panel
          )
        })
      })
    } else {
      isolate({
        output$minimum_detected_value_settings_button_uioutput <- renderUI({})
      })
    }
  })
  
  observe({
    if(na_inf_panel_visible$max_val_settings) {
      isolate({
        output$maximum_detected_value_settings_button_uioutput <- renderUI ({
          wellPanel(
            na_inf_upper_limit_panel
          )
        })
      })
    } else {
      isolate({
        output$maximum_detected_value_settings_button_uioutput <- renderUI({})
      })
    }
  })
  
  
  output$minus_inf_current_detection_limit <- renderText({
    paste("<b>Currently set detection limit:</b>", toString(preproc_limits_tracker$minus_inf))
  })
  
  output$plus_inf_current_detection_limit <- renderText({
    paste("<b>Currently set detection limit:</b>", toString(preproc_limits_tracker$plus_inf))
  })
  
  observeEvent(input$show_handling_values_preview_button, {
    isolate({
      # _Shows the preview of the preprocessing step for handling NA, zero and infinite values
      
      na_inf_panel_visible$results_preview <- !na_inf_panel_visible$results_preview
      if(na_inf_panel_visible$results_preview) {
        updateActionButton(session, "show_handling_values_preview_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "show_handling_values_preview_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$preprocessing_NA_zero_inf_apply_to_preview_button, {
    # _Shows the preview of the preprocessing step for handling NA, zero and infinite values
    
    isolate({
      preproc_NA_zero_inf <- update_preproc_NA_zero_inf()
      pp_na_zero_inf1$a@t <- t1_f$a
      
      na_zero_inf_preprocessing_result <- run_na_zero_inf_preprocessing(pp_na_zero_inf1$a, preproc_NA_zero_inf, preproc_limits_tracker)
      pp_na_zero_inf1$a@t <- na_zero_inf_preprocessing_result$t
      
      modif_names <- NULL
      for(i in 1:ncol(pp_na_zero_inf1$a@t@df)) {
        modif_names <- c(modif_names, paste("modif_", colnames(pp_na_zero_inf1$a@t@df)[i], sep=""))
      }
      
      colnames(na_zero_inf_preprocessing_result$modification_marks_frame) <- modif_names
      
      samples_count <- ncol(pp_na_zero_inf1$a@t@df) -1
      
      displaying_frame_f$a <- datatable(cbind(pp_na_zero_inf1$a@t@df, na_zero_inf_preprocessing_result$modification_marks_frame), selection = "single",
                                        options=list(columnDefs = list(list(visible=FALSE, targets=c((2+samples_count):(2+2*samples_count))))))%>%
        formatStyle(2:(1+samples_count), valueColumns=(3+samples_count):(2+2*samples_count),
                    backgroundColor = styleEqual(c(0, 1), c('white', 'yellow')))
      
      output$preprocessing_NA_zero_inf_report_box <- renderText({
        na_zero_inf_preprocessing_result$report
      })
      
      output$preprocessing_NA_zero_inf_preview_table <- DT::renderDataTable(displaying_frame_f$a)
      
      message_out("Changes were applied to preview data", "Handling NA, zero and infinite values")
      na_inf_changes_applied_to_preview_data$a <- TRUE
      shinyjs::enable("show_handling_values_preview_button")
      shinyjs::enable("preprocessing_NA_zero_inf_apply_button")
      
    })
    
  })
  
  observeEvent(input$preprocessing_NA_zero_inf_reset_preview_button, {
    isolate({
      na_inf_changes_applied_to_preview_data$a <- FALSE
      shinyjs::disable("show_handling_values_preview_button")
      shinyjs::disable("preprocessing_NA_zero_inf_apply_button")
      initialise_pp_na_zero_inf_object()
      message_out("The previewed changes were reset", "Handling NA, zero and infinite values")
    })
  })
  
  observe({
    if(na_inf_panel_visible$results_preview) {
      isolate({
        
        output$preprocessing_NA_zero_inf_preview <- renderUI({
          tagList(
            tags$h4("Preview of the preprocessing step"),
            htmlOutput(ns("preprocessing_NA_zero_inf_report_box")),
            column(12,
                   dataTableOutput(ns("preprocessing_NA_zero_inf_preview_table"))
            )
          )
        })
        
      })
    } else {
      isolate({
        output$preprocessing_NA_zero_inf_preview <- renderUI({})
      })
    }
  })
  
  observeEvent(input$minus_inf_set_detection_limit_to_specified_value, {
    # _Sets the estimated lower detection limit of the measuring instrument to the value specified by the user
    isolate({
      if(is.numeric(input$minus_inf_detection_limit_numericinput)) {
        preproc_limits_tracker$minus_inf <- as.numeric(input$minus_inf_detection_limit_numericinput)
      } else {
        show_generic_error_message("The specified value is not numeric")
      }
      
    })
  })
  
  observeEvent(input$plus_inf_set_detection_limit_to_specified_value, {
    # _Sets the estimated upper detection limit of the measuring instrument to the value specified by the user
    isolate({
      
      if(is.numeric(input$plus_inf_detection_limit_numericinput)) {
        preproc_limits_tracker$plus_inf <- as.numeric(input$plus_inf_detection_limit_numericinput)
      } else {
        show_generic_error_message("The specified value is not numeric")
      }
      
    })
  })
  
  observeEvent(input$minus_inf_estimate_detection_limit_from_data, {
    
    isolate({
      update_lowest_detected_value_by_data()
    })
  })
  
  observeEvent(input$plus_inf_estimate_detection_limit_from_data, {
    # _Estimates the upper detection limit of the measuring instrument by the highest value in input data
    isolate({
      update_highest_detected_value_by_data()
      
    })
  })
  
  observeEvent(input$preprocessing_NA_zero_inf_skip_button, {
    isolate({
      
      # _The handling of NA, zero and infinite values: skipping this step of preprocessing
      output_t1_f$skip_counter <- output_t1_f$skip_counter + 1
    })
  })
  
  observeEvent(input$preprocessing_NA_zero_inf_apply_button, {
    isolate({
      
      # _The handling of NA, zero and infinite values: applying the preprocessing to input data
      
      preproc_NA_zero_inf <- update_preproc_NA_zero_inf()
      pp_na_zero_inf1$a@t <- t1_f$a
      na_zero_inf_preprocessing_result <- run_na_zero_inf_preprocessing(pp_na_zero_inf1$a, preproc_NA_zero_inf, preproc_limits_tracker)
      pp_na_zero_inf1$a@t <- na_zero_inf_preprocessing_result$t
      t1_f$a <- na_zero_inf_preprocessing_result$t
      
      
      showModal(modalDialog(
        title = "Preprocessing",
        "Preprocessing to handle NA, zero and infinite values was performed on the input data")
      )
      output_t1_f$a <- t1_f$a
      output_t1_f$counter <- output_t1_f$counter + 1
      
      
    })
  })
  
  out_reactive <- reactive({list(output_t1_f$counter, output_t1_f$a, output_t1_f$skip_counter)})
  
  
  return(out_reactive)
}

