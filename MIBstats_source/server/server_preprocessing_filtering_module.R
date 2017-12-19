# _Module for the filtering step of preprocessing

preprocessing_filtering_moduleUI <- function(id) {
  ns <- NS(id)
    tagList(
      
      tags$h3("Filtering of data"),
      tags$h4("Relative standard deviation (RSD) filtering"),
      wellPanel(
        helpText("RSD filtering uses RSD to detect and remove variables with high spread."),
        checkboxGroupInput(ns("filtering_rsd_checkboxgroupinput"), "Select quality control sample classes for RSD filtering. Variables are deleted if their RSD in the quality control classes exceeds a user-defined threshold."),
        actionButton(ns("filtering_rsd_checkboxgroupinput_select_all_button"), "Select all"),
        bsTooltip(id = ns("filtering_rsd_checkboxgroupinput_select_all_button"), title = "Click here to select all sample classes",
                  placement = "left", trigger = "hover"),
        actionButton(ns("filtering_rsd_checkboxgroupinput_select_none_button"), "Select none"),
        bsTooltip(id = ns("filtering_rsd_checkboxgroupinput_select_none_button"), title = "Click here to deselect all sample classes",
                  placement = "left", trigger = "hover")
      ),
      sliderInput(ns("filtering_rsd_slider"), "Select the RSD filtering cutoff (maximum RSD % that is allowed)", value=75, min=0, max=100),
      actionButton(ns("rsd_filtering_button"), "Perform RSD filtering"),
      bsTooltip(id = ns("rsd_filtering_button"), title = "Click here to perform RSD filtering",
                placement = "left", trigger = "hover"),
      help_box_moduleUI(ns("help_rsd")),
      tags$hr(),
      tags$h4("Interquartile range (IQR) filtering"),
      helpText("IQR filtering goes through the data variable by variable and finds values that are outliers. It can be used to remove outlier values of each variable"),
      wellPanel(
        sliderInput(ns("iqr_multiplier_numericinput"), "Multiplier in outlier detection:", value=1.5, min=0.1, max=5),
        htmlOutput(ns("iqr_helptext_htmloutput")),
        radioButtons(ns("iqr_replacement_choice_radiobutton"), "Action that will be taken with outliers:",
                     c("Replace with NA" = "replace_with_na",
                       "Replace with average value of the variable" = "replace_with_average")),
        checkboxGroupInput(ns("filtering_iqr_checkboxgroupinput"), "Select variables to be included in IQR filtering"),
        actionButton(ns("filtering_iqr_checkboxgroupinput_select_all_button"), "Select all"),
        bsTooltip(id = ns("filtering_iqr_checkboxgroupinput_select_all_button"), title = "Click here to select all variables",
                  placement = "left", trigger = "hover"),
        actionButton(ns("filtering_iqr_checkboxgroupinput_select_none_button"), "Select none"),
        bsTooltip(id = ns("filtering_iqr_checkboxgroupinput_select_none_button"), title = "Click here to deselect all variables",
                  placement = "left", trigger = "hover")
      ),
      actionButton(ns("iqr_filtering_button"), "Perform IQR filtering"),
      bsTooltip(id = ns("iqr_filtering_button"), title = "Click here to perform IQR filtering",
                placement = "left", trigger = "hover"),
      help_box_moduleUI(ns("help_iqr")),
      tags$hr(),
      tags$h4("Data preview"),
      actionButton(ns("filtering_view_preview_button"), "View preview of filtered data", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
      bsTooltip(id = ns("filtering_view_preview_button"), title = "Click here to view preview of the filtered data",
                placement = "left", trigger = "hover"),
      uiOutput(ns("filtering_preview_datatable_container")),
      tags$hr(),
      tags$h4("Applying the changes"),
      actionButton(ns("filtering_reset_preview_button"), "Reset the previewed changes"),
      bsTooltip(id = ns("filtering_reset_preview_button"), title = "Click here to reset the previewed changes to their initial state",
                placement = "left", trigger = "hover"),
      actionButton(ns("filtering_apply_button"), "Apply the previewed changes to the data and move to the next step of preprocessing", style = paste0("color: ", next_button_color)),
      bsTooltip(id = ns("filtering_apply_button"), title = "Click here to apply the previewed changes to the data and move to the next step of preprocessing",
                placement = "left", trigger = "hover"),
      actionButton(ns("filtering_skip_button"), "Move to the next step without making changes to the data", style = paste0("color: ", skip_button_color)),
      bsTooltip(id = ns("filtering_skip_button"), title = "Click here to move to the next step without making changes to the data",
                placement = "left", trigger = "hover")
      
    )
}

preprocessing_filtering_module <- function(input, output, session, t1_f_reactive) {
  # _Module for the filtering step of preprocessing
  
  ns <- session$ns
  
  callModule(help_box_module, "help_rsd")
  callModule(help_box_module, "help_iqr")
  
  t1_f <- reactiveValues(a = NULL)
  output_t1_f <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
  
  pp_fltr1 <- reactiveValues(a = NULL)

  initialise_fltr1 <- function() {
    # _Initialises pp_fltr1$a, which is the main object used for the filtering step of preprocessing
    pp_fltr1$a <<- preprocessing_filter()
    pp_fltr1$a@t <<- t1_f$a
    pp_fltr1$a <<- preproc_initialise_class_labels_vector(pp_fltr1$a, pp_fltr1$a@t)
    
    pp_fltr1$a@panel_tracking$rsd_selected_classes <<- NULL
    pp_fltr1$a@panel_tracking$iqr_selected_variables <<- NULL
    pp_fltr1$a@panel_tracking$preview_table_visible <<- FALSE
    
    shinyjs::enable("rsd_filtering_button")
    shinyjs::enable("iqr_filtering_button")
  }
  
  observe({
    t1_f_observed <- t1_f_reactive()
    if(!is.null(t1_f_observed)) {
      isolate({
        if(!is.null(t1_f_observed@df)) {
          t1_f$a <- t1_f_observed
          initialise_fltr1()
          pp_fltr1$a@panel_tracking$rsd_selected_classes <- NULL
          updateCheckboxGroupInput(session, "filtering_rsd_checkboxgroupinput", choices = filtering_get_unique_class_labels(), selected = pp_fltr1$a@panel_tracking$rsd_selected_classes, inline=TRUE)
          updateCheckboxGroupInput(session, "filtering_iqr_checkboxgroupinput", choices = df_get_variable_names(pp_fltr1$a@t), selected = pp_fltr1$a@panel_tracking$iqr_selected_variables, inline=TRUE)
          rsd_select_all()
          iqr_select_all()
        }
      })
    }
  })
  
  
  
  output$iqr_helptext_htmloutput <- renderText({
    HTML("The lower and upper limits in IQR filtering: <br>Lower limit = first quartile−multiplier*IQR<br>Upper limit = third quartile+multiplier*IQR<br>Default value of the multiplier: 1.5<br><br>")
  })
  
  
  filtering_get_unique_class_labels <- function() {
    # _Finds unique class labels in the loaded class labels data
    class_labels_f <- pp_fltr1$a@class_labels_vector
    unique_class_labels_f <- unique(pp_fltr1$a@class_labels_vector)
    return(unique_class_labels_f)
  }
  
  
  
  update_checkboxgroupinputs_after_filtering <- function() {
    # _Updates the RSD and IQR checkbox groupinputs after filtering is performed (as the number of samples or variables may have changed)
    rsd_choices <- filtering_get_unique_class_labels()
    iqr_choices <- df_get_variable_names(pp_fltr1$a@t)
    rsd_selected <- intersect(rsd_choices, pp_fltr1$a@panel_tracking$rsd_selected_classes)
    iqr_selected <- intersect(iqr_choices, pp_fltr1$a@panel_tracking$iqr_selected_variables)
    
    updateCheckboxGroupInput(session, "filtering_rsd_checkboxgroupinput", choices = filtering_get_unique_class_labels(), selected = rsd_selected, inline=TRUE)
    updateCheckboxGroupInput(session, "filtering_iqr_checkboxgroupinput", choices = df_get_variable_names(pp_fltr1$a@t), selected = iqr_selected, inline=TRUE)
  }
  
  rsd_select_all <- function() {
    # _Selects all items in RSD filtering checkBoxGroupInput
    pp_fltr1$a@panel_tracking$rsd_selected_classes <- filtering_get_unique_class_labels()
    updateCheckboxGroupInput(session, "filtering_rsd_checkboxgroupinput", selected = pp_fltr1$a@panel_tracking$rsd_selected_classes)
  }
  
  iqr_select_all <- function() {
    # _Selects all items in IQR filtering checkBoxGroupInput
    pp_fltr1$a@panel_tracking$iqr_selected_variables <- df_get_variable_names(pp_fltr1$a@t)
    updateCheckboxGroupInput(session, "filtering_iqr_checkboxgroupinput", selected = pp_fltr1$a@panel_tracking$iqr_selected_variables)
  }
  
  filtering_update_toggle_button <- function(switch_variable_name_f, button_name_f) {
    # _Updates the plus or minus icons on buttons that open or close panels
    pp_fltr1$a <<- update_toggle_button(pp_fltr1$a, switch_variable_name_f, button_name_f)
  }
  
  filtering_set_toggle_button_to_false <- function(switch_variable_name_f, button_name_f) {
    # _Closes all UI panels on the page
    pp_fltr1$a <<- set_toggle_button_to_false(pp_fltr1$a, switch_variable_name_f, button_name_f)
  }
  
  observeEvent(input$filtering_rsd_checkboxgroupinput_select_all_button, {
    isolate({
      rsd_select_all()
    })
  })
  
  observeEvent(input$filtering_rsd_checkboxgroupinput_select_none_button, {
    isolate({
      pp_fltr1$a@panel_tracking$rsd_selected_classes <- character(0)
      updateCheckboxGroupInput(session, "filtering_rsd_checkboxgroupinput", selected = pp_fltr1$a@panel_tracking$rsd_selected_classes)
    })
  })
  
  observeEvent(input$filtering_iqr_checkboxgroupinput_select_all_button, {
    isolate({
      iqr_select_all()
    })
  })
  
  observeEvent(input$filtering_iqr_checkboxgroupinput_select_none_button, {
    isolate({
      pp_fltr1$a@panel_tracking$iqr_selected_variables <- character(0)
      updateCheckboxGroupInput(session, "filtering_iqr_checkboxgroupinput", selected = pp_fltr1$a@panel_tracking$iqr_selected_variables)
    })
  })
  
  get_rsd_threshold <- function() {
    # _reads RSD filtering threshold from the slider in the UI
    rsd_threshold_f <- input$filtering_rsd_slider
    return(rsd_threshold_f)
  }
  
  observeEvent(input$rsd_filtering_button, {
    isolate({
      pp_fltr1$a@panel_tracking$rsd_selected_classes <- input$filtering_rsd_checkboxgroupinput
      no_selection_error_message <- ("Please first select sample classes for RSD filtering before running RSD filtering")
      if(is.null(input$filtering_rsd_checkboxgroupinput)) {
        show_generic_error_message(no_selection_error_message)
      } else {
        rsd_threshold <- get_rsd_threshold()
        rsd_filtered_data <- rsd_filter(pp_fltr1$a, rsd_threshold, input$filtering_rsd_checkboxgroupinput)
        pp_fltr1$a@t@df <- rsd_filtered_data@t@df
        pp_fltr1$a@rsd_done <- rsd_filtered_data@rsd_done
        if(pp_fltr1$a@rsd_done) {
          shinyjs::disable("rsd_filtering_button")
        }
      }
      
      update_checkboxgroupinputs_after_filtering()
      
    })
  })
  
  observeEvent(input$iqr_filtering_button, {
    isolate({
      pp_fltr1$a@panel_tracking$iqr_selected_variables <- input$filtering_iqr_checkboxgroupinput
      selected_variables <- 1:nrow(pp_fltr1$a@t@df)
      
      
      replacement_choice <- input$iqr_replacement_choice_radiobutton
      
      pp_fltr1$a <- iqr_filter(pp_fltr1$a, selected_variables, input$iqr_multiplier_numericinput, replacement_choice)
      if(pp_fltr1$a@iqr_done) {
        shinyjs::disable("iqr_filtering_button")
      }
      update_checkboxgroupinputs_after_filtering()
      
    })
  })
  
  observeEvent(input$filtering_reset_preview_button, {
    isolate({
      initialise_fltr1()
      update_checkboxgroupinputs_after_filtering()
      rsd_select_all()
      iqr_select_all()
      filtering_set_toggle_button_to_false("preview_table_visible", ns("filtering_view_preview_button"))
      
      message_out("Any changed values on the sorting page were reset to their initial state", "Filtering")
      
    })
  })
  
  observeEvent(input$filtering_view_preview_button, {
    isolate({
      filtering_update_toggle_button("preview_table_visible", ns("filtering_view_preview_button"))
      if(pp_fltr1$a@panel_tracking$preview_table_visible) {
        output$filtering_preview_datatable <- DT::renderDataTable(pp_fltr1$a@t@df)
      }
    })
  })
  
  observeEvent(input$filtering_apply_button, {
    isolate({
      t1_f$a <- pp_fltr1$a@t
      message_out("The preprocessing steps of data sorting were applied to input data", "Filtering")
      output_t1_f$a <- t1_f$a
      output_t1_f$counter <- output_t1_f$counter + 1
      
    })
  })
  
  observeEvent(input$filtering_skip_button, {
    isolate({
      output_t1_f$skip_counter <- output_t1_f$skip_counter + 1
      
    })
  })
  
  observe({
    if(pp_fltr1$a@panel_tracking$preview_table_visible) {
      isolate({
        output$filtering_preview_datatable_container <- renderUI({
          tagList(
            div(dataTableOutput(ns("filtering_preview_datatable")), style = "font-size:80%")
          )
        })
      })
    } else {
      isolate({
        output$filtering_preview_datatable_container <- renderUI({})
      })
    }
  })
  
  out_reactive <- reactive({list(output_t1_f$counter, output_t1_f$a, output_t1_f$skip_counter)})
  return(out_reactive)
}
