# _The module for the preprocessing step of scaling, normalisation and variance stabilisation

library(ggplot2)
library(preprocessCore)

preprocessing_snv_moduleUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    tags$h3("Scaling, normalisation and variance stabilisation"),
    helpText("Step 1 of 2"),
    tags$h4("Mean centering"),
    
    actionButton(ns("snv_mean_centering_button"), "Perform mean centering"),
    bsTooltip(id = ns("snv_mean_centering_button"), title = "Click here to perform mean centering of the data",
              placement = "left", trigger = "hover"),
    preproc_snv_plotsUI(ns("mean_centering")),
    actionButton(ns("snv_skip_mean_centering_button"), "Skip mean centering"),
    bsTooltip(id = ns("snv_skip_mean_centering_button"), title = "Click here to skip mean centering and move to the next step (scaling)",
              placement = "left", trigger = "hover"),
    help_box_moduleUI(ns("help_mean_centering")),
    uiOutput(ns("step2_uioutput")),
    uiOutput(ns("page3_uioutput")),
    uiOutput(ns("page4_uioutput")),
    uiOutput(ns("page5_uioutput")),
    uiOutput(ns("page6_uioutput")),
    tags$hr(),
    uiOutput(ns("applying_changes_uioutput")),
    actionButton(ns("snv_reset_preview_button"), "Reset the previewed changes"),
    bsTooltip(id = ns("snv_reset_preview_button"), title = "Click here to reset the previewed changes to their initial state",
              placement = "left", trigger = "hover"),
    actionButton(ns("snv_skip_button"), "Move to the next step without making changes to the data", style = paste0("color: ", skip_button_color)),
    bsTooltip(id = ns("snv_skip_button"), title = "Click here to move to the next step without making changes to the data",
              placement = "left", trigger = "hover")
  )
}

preprocessing_snv_module <- function(input, output, session, t1_f_reactive) {
  # _Module for rank product
  ns <- session$ns
  
  help_modules_vector <- c("mean_centering", "unit_variance_scaling", "pareto_scaling", "quantile_normalisation", "log_transformation", "glog_transformation", "vsn")
  
  for(item in help_modules_vector) {
    callModule(help_box_module, paste("help_", item, sep=""))
  } 
  
  t1_f <- reactiveValues(a = NULL)
  output_t1_f <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
  
  snv_initialised <- reactiveValues(a = FALSE)
  
  buttons_vector <- reactiveValues(a = c("snv_pareto_scaling_button", "snv_skip_mean_centering_button", "snv_apply_button", "snv_choice_radiobuttons", "snv_variance_stabilisation_button", "snv_mean_centering_button", "snv_unit_variance_scaling_button", "snv_quantile_normalisation_button", "snv_log_transformation_button", "snv_glog_transformation_button", "snv_scaling_radiobutton"))
  disabled_buttons <- reactiveValues(a = NULL)
  
  disabled_buttons_reactive <- reactive({disabled_buttons$a})
  
  pp_scaler1 <- reactiveValues(a = NULL)
  pp_normaliser1 <- reactiveValues(a = NULL)
  pp_variance_stabiliser1 <- reactiveValues(a = NULL)
  
  
  add_to_disabled_buttons <- function(disabled_buttons_a, button_name_f) {
    # _A function for keeping track of which buttons need to be enabled and which ones disabled
    # _here, a button is added to the list of disabled buttons
    new_vector <- c(disabled_buttons_a, button_name_f)
    new_vector <- unique(new_vector)
    return(new_vector)
  }
  
  remove_from_disabled_buttons <- function(disabled_buttons_a, button_name_f) {
    # _A function for keeping track of which buttons need to be enabled and which ones disabled
    # _here, a button is removed from the list of disabled buttons
    ind <- which(disabled_buttons_a == button_name_f)
    if(!is.null(ind)) {
      disabled_buttons_a <- disabled_buttons_a[-ind]
    }
    return(disabled_buttons_a)
  }

  observe({
    # _Loading of input data to this preprocessing module
    t1_f_observed <- t1_f_reactive()
    if(!is.null(t1_f_observed)) {
      isolate({
        if(!is.null(t1_f_observed@df)) {
          t1_f$a <- t1_f_observed
          if(snv_initialised$a == FALSE) {
            # _If input data is loaded for the first time
            initialise_snv()
            initialise_snv_modules()
            snv_initialised$a <- TRUE
          } else {
            # _If input data has already been loaded but needs updating
            reset_snv()
          }
        }
      })
    }
  })
  
  update_snv_choice_radiobuttons <- function(buttons_vector_a, disabled_buttons_a) {
    # _Updates the enabled or disabled state of scaling/normalisation/variance stabilisation choice radio buttons
    output$snv_choice_radiobuttons_container <- renderUI({
      radioButtons(ns("snv_choice_radiobuttons"), "Method of scaling/normalisation/variance stabilisation:",
                   c( "None" = "none", "Scaling" = "scaling", "Quantile normalisation" = "normalisation", "Logarithmic transformation" = "logarithmic_transformation", "Variance stabilising normalisation" = "variance_stabilisation"), selected = "none")
    })
    update_disabling_of_buttons(buttons_vector_a, disabled_buttons_a)
  }
  
  initialise_snv <- function() {
    # _Initialises the objects for scaling, normalisation and variance stabilisation
    snv_close_switch <<- reactiveValues(a = 1)
    pp_snv_last_step_used <<- reactiveValues(a = 0)
    
    pp_scaler1$a <<- preprocessing_scaler()
    pp_normaliser1$a <<- preprocessing_normaliser()
    pp_variance_stabiliser1$a <<- preprocessing_variance_stabiliser()
    
    pp_scaler1$a@t <<- t1_f$a
    pp_scaler1$a@l <<- list()
    pp_scaler1$a@l$scaling_choice <<- "none"
    
    pp_normaliser1$a@t <<- t1_f$a
    pp_normaliser1$a@l$initialised <- FALSE
    pp_variance_stabiliser1$a@t <<- t1_f$a
    
    disabled_buttons$a <<- add_to_disabled_buttons(disabled_buttons$a, "snv_pareto_scaling_button")
    
    preproc_snv_plot_names <<- c("mean_centering", "unit_variance_scaling", "pareto_scaling", "quantile_normalisation", "log_transformation", "glog_transformation", "variance_stabilisation", "snv_summary")
    
    df_pre_list <<- reactiveValues(l = list())
    df_post_list <<- reactiveValues(l = list())
    processing_done_list <<- reactiveValues(l = list())
    
    snv_page_tracker <<- reactiveValues(a = 1)
    
    snv_page_tracker_reactive <<- reactive({snv_page_tracker$a})
    
    
    for(item_f in preproc_snv_plot_names) {
      df_pre_list$l[[item_f]] <<- data.frame()
      df_post_list$l[[item_f]] <<- data.frame()
      processing_done_list$l[[item_f]] <<- FALSE
    }
    
    processing_done_reactive <<- reactive({processing_done_list$l})
    
    df_pre_reactive <<- reactive({df_pre_list$l})
    df_post_reactive <<- reactive({df_post_list$l})
    snv_closebuttons_reactive <<- reactive({snv_close_switch})
    
    step2_panel_visible <<- reactiveValues(a = FALSE)
    
    disabled_buttons$a <<- add_to_disabled_buttons(disabled_buttons$a, "snv_apply_button")
    
    
    update_snv_choice_radiobuttons(buttons_vector$a, disabled_buttons$a)
  }
  
  reset_snv <- function() {
    # _Resets variables that track the state of the page if the user clicks the button that resets the page
    disabled_buttons$a <- NULL
    pp_snv_last_step_used$a <<- 0
    pp_scaler1$a <<- preprocessing_scaler()
    pp_normaliser1$a <<- preprocessing_normaliser()
    pp_variance_stabiliser1$a <<- preprocessing_variance_stabiliser()
    
    pp_scaler1$a@t <<- t1_f$a
    pp_scaler1$a@l <<- list()
    pp_scaler1$a@l$scaling_choice <<- "none"
    
    pp_normaliser1$a@t <<- t1_f$a
    pp_normaliser1$a@l$initialised <- FALSE
    pp_normaliser1$a@log_transf_done <- FALSE
    pp_variance_stabiliser1$a@t <<- t1_f$a
    
    
    df_pre_list$l <<- list()
    df_post_list$l <<- list()
    processing_done_list$l <<- list()
    
    snv_page_tracker$a <<- 1
    
    for(item_f in preproc_snv_plot_names) {
      df_pre_list$l[[item_f]] <<- data.frame()
      df_post_list$l[[item_f]] <<- data.frame()
      processing_done_list$l[[item_f]] <<- FALSE
    }
    
    step2_panel_visible$a <<- FALSE
    
    update_snv_choice_radiobuttons(buttons_vector$a, disabled_buttons$a)
    shinyjs::enable("snv_mean_centering_button")
    shinyjs::enable("snv_skip_mean_centering_button")
  }
  
  
  initialise_snv_modules <- function() {
    # _Initialises the box plots drawing module for each box plot on this preprocessing page
    preproc_snv_plot_names <- c("mean_centering", "unit_variance_scaling", "pareto_scaling", "quantile_normalisation", "log_transformation", "glog_transformation", "variance_stabilisation", "snv_summary")
    names(preproc_snv_plot_names) <- c("Mean centering", "Unit variance scaling", "Pareto scaling", "Quantile normalisation", "Log transformation", "glog transformation", "Variance stabilising normalisation", "Summary of the scaling/normalisation/variance stabilisation pipeline")
    counter <- 1
    for(item_f in preproc_snv_plot_names) {
      callModule(preproc_snv_plots, item_f, df_pre_reactive, df_post_reactive, processing_done_reactive, names(preproc_snv_plot_names)[counter], snv_closebuttons_reactive)
      counter <- counter + 1
    }
  }
  
  update_pp_snv_last_step_used <- function(new_step_f, pp_snv_last_step_used_a) {
    # _This function keeps track of how far the user has got in navigating through the sections of this preprocessing page (scaling == 3, normalisation == 4, variance stabilisation == 5)
    if(new_step_f > pp_snv_last_step_used$a) {
      pp_snv_last_step_used$a <<- new_step_f
    }
    if(new_step_f == 1) pp_normaliser1$a@l$initialised <- FALSE
    
    df_pre_list$l[["snv_summary"]] <- t1_f$a@df
    df_post_list$l[["snv_summary"]] <- get_the_result_of_snv_pipeline(pp_snv_last_step_used_a)
    processing_done_list$l[["snv_summary"]] <- TRUE
  }
  
  get_the_result_of_snv_pipeline <- function(pp_snv_last_step_used_a) {
    # _Gets the resulting data frame of the scaling/normalisation/variance stabilisation pipeline
    snv_final_df <- t1_f$a@df
    if(pp_snv_last_step_used_a == 1) {
      snv_final_df <- get_output_df(pp_scaler1$a)
    } else if (pp_snv_last_step_used_a == 2) {
      snv_final_df <- pp_normaliser1$a@t@df
    } else if (pp_snv_last_step_used_a == 3) {
      snv_final_df <- pp_variance_stabiliser1$a@t@df
    }
    return(snv_final_df)
  }
  
  update_snv_close_switch <- function() {
    # _snv_close_switch$a is used to close all box plots when a preprocessing substep is completed on this page. To trigger the closing of plots, the snv_close_switch$a variable is incremented by one
    snv_close_switch$a <- snv_close_switch$a + 1
  }
  
  snv_mean_centering_button_next <- function() {
    # _Operations to perform after the mean centering step
    
    update_snv_close_switch()
    snv_page_tracker$a <<- 2
  }
  
  snv_update_buttons_after_processing_data <- function(disabled_buttons_a) {
    disabled_buttons$a <<- remove_from_disabled_buttons(disabled_buttons_a, "snv_apply_button")
    disabled_buttons$a <<- add_to_disabled_buttons(disabled_buttons_a, "snv_choice_radiobuttons")
    
  }
  
  snv_temporarily_remove_tooltips <- function() {
    # _Workaround for a bug in bsTooltip that makes tooltips stuck on the screen if they are displayed at the same time with a modalDialog.
    # _So to prevent the clash, tooltips are removed here for the time when a modalDialog is displayed, and are then put back again.
    # _It seems that the tooltips sometimes still get stuck on the screen, though. So this part needs further work
    button_names <- c("snv_mean_centering_button", "snv_pareto_scaling_button", "snv_unit_variance_scaling_button", "snv_quantile_normalisation_button",
                      "snv_log_transformation_button", "snv_glog_transformation_button", "snv_variance_stabilisation_button", "snv_apply_button")
    for(button_name in button_names) {
      removeTooltip(session, id = button_name)
    }
  }
  
  snv_restore_tooltips <- function() {
    # _Restores bsTooltips after they have been temporarily removed to make way for the modalDialog
    addTooltip(session, id = "snv_mean_centering_button", title = "Click here to perform mean centering of the data",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_unit_variance_scaling_button", title = "Click here to perform unit variance scaling",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_pareto_scaling_button", title = "Click here to perform pareto scaling",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_quantile_normalisation_button", title = "Click here to perform quantile normalisation",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_log_transformation_button", title = "Click here to perform log transformation",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_glog_transformation_button", title = "Click here to perform glog transformation",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_variance_stabilisation_button", title = "Click here to perform variance stabilising normalisation",
               placement = "left", trigger = "hover")
    addTooltip(session, id = "snv_apply_button", title = "Click here to apply the previewed changes to the data and go to viewing preprocessed data",
               placement = "left", trigger = "hover")
  }
  
  snv_message_out_without_tooltips <- function(message_f, title_f) {
    # _Removes tooltips, displays a modalDialog message box and restores the tooltips
    snv_temporarily_remove_tooltips()
    message_out(message_f, title_f)
    snv_restore_tooltips()
  }
  
  observeEvent(input$snv_mean_centering_button, {
    isolate({
      pp_scaler1$a <- perform_mean_centering(pp_scaler1$a)
      
      df_pre_list$l[["mean_centering"]] <- pp_scaler1$a@old_df
      df_post_list$l[["mean_centering"]] <- pp_scaler1$a@t@df
      processing_done_list$l[["mean_centering"]] <- TRUE
      
      update_pp_snv_last_step_used(1, pp_snv_last_step_used$a)
      snv_mean_centering_button_next()
      
      snv_message_out_without_tooltips("The preview data was mean centered", "Mean centering")
      
      disabled_buttons$a <- remove_from_disabled_buttons(disabled_buttons$a, "snv_apply_button")
      
      step2_panel_visible$a <- TRUE
      
      disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, c("snv_mean_centering_button", "snv_skip_mean_centering_button"))
      update_disabling_of_buttons(buttons_vector$a, disabled_buttons$a)
      
    })
  })
  
  observe({
    if(step2_panel_visible$a) {
      isolate({
        output$step2_uioutput <- renderUI({
          tagList(
            tags$hr(),
            helpText("Step 2 of 2"),
            uiOutput(ns("snv_choice_radiobuttons_container"))
          )
        })
      })
    } else {
      isolate({
        output$step2_uioutput <- renderUI({})
      })
    }
  })
  
  snv_pareto_scaling_button_next <- function() {
    # _Operations to perform after the pareto scaling step
    update_snv_close_switch()
    
    snv_page_tracker$a <- 3
  }
  
  observe({
    disabled_b <- disabled_buttons_reactive()
    isolate({
      if(!is.null(disabled_b)) {
        update_disabling_of_buttons(buttons_vector$a, disabled_buttons$a)
      }
      
    })
  })
  
  update_disabling_of_buttons <- function(buttons_vector_a, disabled_buttons_a) {
    for(item in buttons_vector_a) {
      if(is.element(item, disabled_buttons_a)) {
        shinyjs::disable(item)
      } else {
        shinyjs::enable(item)
      }
    }
  }
  
  observe({
    snv_page_tracker_observed <- snv_page_tracker_reactive()
    if(!is.null(snv_page_tracker_observed)) {
      isolate({
        
        if(snv_page_tracker_observed == 3) {
          output$page3_uioutput <- renderUI({
            tagList(
              tags$hr(),
              tags$h4("Scaling"),
              radioButtons(ns("snv_scaling_radiobutton"), "Method of scaling:",
                           c("Unit variance scaling" = "unit_variance", "Pareto scaling" = "pareto")),
              uiOutput(ns("unit_variance_uioutput")),
              uiOutput(ns("pareto_uioutput"))
              
            )
          })
        } else {
          output$page3_uioutput <- renderUI({})
        }
        
        if(snv_page_tracker_observed == 4) {
          output$page4_uioutput <- renderUI({
            tagList(
              tags$hr(),
              tags$h4("Quantile normalisation"),
              actionButton(ns("snv_quantile_normalisation_button"), "Perform quantile normalisation"),
              bsTooltip(id = ns("snv_quantile_normalisation_button"), title = "Click here to perform quantile normalisation",
                        placement = "left", trigger = "hover"),
              preproc_snv_plotsUI(ns("quantile_normalisation")),
              help_box_moduleUI(ns("help_quantile_normalisation"))
            )
          })
        } else {
          output$page4_uioutput <- renderUI({})
        }
        
        if(snv_page_tracker_observed == 5) {
          output$page5_uioutput <- renderUI({
            tagList(
              tags$hr(),
              tags$h4("Logarithmic transformation"),
              tags$b("Log transformation"),
              tags$br(),
              radioButtons(ns("snv_log_base_selection_radiobutton"), "Log base:",
                           c("2" = "base_2", "10" = "base_10")),
              actionButton(ns("snv_log_transformation_button"), "Perform log transformation"),
              bsTooltip(id = ns("snv_log_transformation_button"), title = "Click here to perform log transformation",
                        placement = "left", trigger = "hover"),
              preproc_snv_plotsUI(ns("log_transformation")),
              help_box_moduleUI(ns("help_log_transformation")),
              tags$hr(),
              tags$b("Generalised log transformation (glog)"),
              helpText("The formula of glog transformation of a variable y:"),
              helpText("log(y + sqrt(y^2 + lambda))"),
              helpText("lambda = transform parameter (an experimentally determined constant)"),
              numericInput(ns("glog_lambda_input"), "Lambda value in glog transformation: ", value = 0),
              actionButton(ns("snv_glog_transformation_button"), "Perform glog transformation"),
              bsTooltip(id = ns("snv_glog_transformation_button"), title = "Click here to perform glog transformation",
                        placement = "left", trigger = "hover"),
              preproc_snv_plotsUI(ns("glog_transformation")),
              help_box_moduleUI(ns("help_glog_transformation"))
            )
          })
        } else {
          output$page5_uioutput <- renderUI({})
        }
        
        if(snv_page_tracker_observed == 6) {
          output$page6_uioutput <- renderUI({
            tagList(
              tags$hr(),
              tags$h4("Variance stabilising normalisation"),
              actionButton(ns("snv_variance_stabilisation_button"), "Perform variance stabilising normalisation"),
              bsTooltip(id = ns("snv_variance_stabilisation_button"), title = "Click here to perform variance stabilising normalisation",
                        placement = "left", trigger = "hover"),
              preproc_snv_plotsUI(ns("variance_stabilisation")),
              help_box_moduleUI(ns("help_vsn"))
            )
          })
        } else {
          output$page6_uioutput <- renderUI({})
        }
        
        if(snv_page_tracker_observed > 1) {
          output$applying_changes_uioutput <- renderUI({
            tagList(
              tags$h4("Applying the changes"),
              preproc_snv_plotsUI(ns("snv_summary")),
              
              actionButton(ns("snv_apply_button"), "Apply the previewed changes to the data and go to viewing preprocessed data", style = paste0("color: ", next_button_color)),
              bsTooltip(id = ns("snv_apply_button"), title = "Click here to apply the previewed changes to the data and go to viewing preprocessed data",
                        placement = "left", trigger = "hover")
              
            )
          })
        } else {
          output$applying_changes_uioutput <- renderUI({})
        }
        
        update_disabling_of_buttons(buttons_vector$a, disabled_buttons$a)
      })
    }
  })
  
  observe({
    if(!is.null(input$snv_scaling_radiobutton)) {
      isolate({
        if(input$snv_scaling_radiobutton == "unit_variance") {
          output$unit_variance_uioutput <- renderUI({
            tagList(
              actionButton(ns("snv_unit_variance_scaling_button"), "Perform unit variance scaling"),
              bsTooltip(id = ns("snv_unit_variance_scaling_button"), title = "Click here to perform unit variance scaling",
                        placement = "left", trigger = "hover"),
              preproc_snv_plotsUI(ns("unit_variance_scaling")),
              help_box_moduleUI(ns("help_unit_variance_scaling"))
            )
          })
        } else {
          output$unit_variance_uioutput <- renderUI({})
        }
        
        if(input$snv_scaling_radiobutton == "pareto") {
          output$pareto_uioutput <- renderUI({
            tagList(
              actionButton(ns("snv_pareto_scaling_button"), "Perform pareto scaling"),
              bsTooltip(id = ns("snv_pareto_scaling_button"), title = "Click here to perform pareto scaling",
                        placement = "left", trigger = "hover"),
              preproc_snv_plotsUI(ns("pareto_scaling")),
              help_box_moduleUI(ns("help_pareto_scaling"))
            )
          })
        } else {
          output$pareto_uioutput <- renderUI({})
        }
        update_disabling_of_buttons(buttons_vector$a, disabled_buttons$a)
      })
    }
  })
  
  
  observeEvent(input$snv_pareto_scaling_button, {
    isolate({
      pp_scaler1$a <- perform_scaling(pp_scaler1$a, "pareto")
      
      df_pre_list$l[["pareto_scaling"]] <- pp_scaler1$a@old_df
      df_post_list$l[["pareto_scaling"]] <- pp_scaler1$a@pareto_df
      processing_done_list$l[["pareto_scaling"]] <- TRUE
      
      pp_scaler1$a@l$scaling_choice <- "pareto"
      update_pp_snv_last_step_used(1, pp_snv_last_step_used$a)
      
      snv_pareto_scaling_button_next()
      
      snv_message_out_without_tooltips("Pareto scaling was applied to the preview data", "Pareto scaling")
      disabled_buttons$a <<- add_to_disabled_buttons(disabled_buttons$a, c("snv_unit_variance_scaling_button", "snv_pareto_scaling_button", "snv_scaling_radiobutton"))
      snv_update_buttons_after_processing_data(disabled_buttons$a)
    })
  })
  
  snv_unit_variance_scaling_button_next <- function() {
    # _Operations to perform after the unit variance scaling step
    update_snv_close_switch()
    snv_page_tracker$a <- 3
    
  }
  
  observeEvent(input$snv_unit_variance_scaling_button, {
    isolate({
      pp_scaler1$a <- perform_scaling(pp_scaler1$a, "unit_variance")
      
      df_pre_list$l[["unit_variance_scaling"]] <- pp_scaler1$a@old_df
      df_post_list$l[["unit_variance_scaling"]] <- pp_scaler1$a@unit_variance_df
      processing_done_list$l[["unit_variance_scaling"]] <- TRUE
      
      pp_scaler1$a@l$scaling_choice <- "unit_variance"
      update_pp_snv_last_step_used(1, pp_snv_last_step_used$a)
      snv_unit_variance_scaling_button_next()
      
      shinyjs::disable("snv_unit_variance_scaling_button")
      
      
      snv_message_out_without_tooltips("Unit variance scaling was applied to the preview data", "Unit variance scaling")
      disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, c("snv_unit_variance_scaling_button", "snv_pareto_scaling_button", "snv_scaling_radiobutton"))
      snv_update_buttons_after_processing_data(disabled_buttons$a)
    })
  })
  
  snv_quantile_normalisation_button_next <- function() {
    # _Operations to perform after the quantile normalisation step
    
    update_snv_close_switch()
    snv_page_tracker$a <- 4
    
  }
  
  observeEvent(input$snv_quantile_normalisation_button, {
    isolate({
      pp_normaliser1$a <- initialise_normaliser_if_needed(pp_normaliser1$a, pp_scaler1$a)
      pp_normaliser1$a <- perform_quantile_normalisation(pp_normaliser1$a)
      
      df_pre_list$l[["quantile_normalisation"]] <- pp_normaliser1$a@old_df
      df_post_list$l[["quantile_normalisation"]] <- pp_normaliser1$a@t@df
      processing_done_list$l[["quantile_normalisation"]] <- TRUE
      
      update_pp_snv_last_step_used(2, pp_snv_last_step_used$a)
      snv_quantile_normalisation_button_next()
      
      snv_message_out_without_tooltips("Quantile normalisation was applied to the preview data", "Quantile normalisation")
      disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, "snv_quantile_normalisation_button")
      snv_update_buttons_after_processing_data(disabled_buttons$a)
      
    })
  })
  
  snv_log_transformation_button_next <- function() {
    # _Operations to perform after the log transformation step
    
    update_snv_close_switch()
    snv_page_tracker$a <- 5
    
  }
  
  observeEvent(input$snv_log_transformation_button, {
    isolate({
      pp_normaliser1$a <- initialise_normaliser_if_needed(pp_normaliser1$a, pp_scaler1$a)
      
      log_base_sel <- NULL
      if(input$snv_log_base_selection_radiobutton == "base_2") {
        log_base_sel <- 2
      } else if (input$snv_log_base_selection_radiobutton == "base_10") {
        log_base_sel <- 10
      }
      
      
      pp_normaliser1$a <- perform_log_transformation(pp_normaliser1$a, log_base_sel)
      if(pp_normaliser1$a@log_transf_done) {
        df_pre_list$l[["log_transformation"]] <- pp_normaliser1$a@old_df
        df_post_list$l[["log_transformation"]] <- pp_normaliser1$a@t@df
        processing_done_list$l[["log_transformation"]] <- TRUE
        
        update_pp_snv_last_step_used(2, pp_snv_last_step_used$a)
        snv_log_transformation_button_next()
        snv_update_buttons_after_processing_data(disabled_buttons$a)
        disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, c("snv_log_transformation_button", "snv_glog_transformation_button"))
      }
    })
  })
  
  snv_glog_transformation_button_next <- function() {
    # _Operations to perform after the glog transformation step
    
    update_snv_close_switch()
    snv_page_tracker$a <- 5
    update_pp_snv_last_step_used(2, pp_snv_last_step_used$a)
    
  }
  
  observeEvent(input$snv_glog_transformation_button, {
    isolate({
      
      pp_normaliser1$a <- initialise_normaliser_if_needed(pp_normaliser1$a, pp_scaler1$a)
      pp_normaliser1$a <- perform_glog_transformation(pp_normaliser1$a, input$glog_lambda_input)
      if(pp_normaliser1$a@log_transf_done) {
        df_pre_list$l[["glog_transformation"]] <- pp_normaliser1$a@old_df
        df_post_list$l[["glog_transformation"]] <- pp_normaliser1$a@t@df
        processing_done_list$l[["glog_transformation"]] <- TRUE
        snv_glog_transformation_button_next()
        
        snv_update_buttons_after_processing_data(disabled_buttons$a)
        disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, c("snv_log_transformation_button", "snv_glog_transformation_button"))
        
      }
      
    })
  })
  
  snv_variance_stabilisation_button_next <- function() {
    # _Operations to perform after the variance stabilisation step
    
    update_snv_close_switch()
    snv_page_tracker$a <- 6
    
  }
  
  observeEvent(input$snv_variance_stabilisation_button, {
    isolate({
      pp_normaliser1$a <- initialise_normaliser_if_needed(pp_normaliser1$a, pp_scaler1$a)
      pp_variance_stabiliser1$a <- get_input_from_normaliser(pp_variance_stabiliser1$a, pp_normaliser1$a)
      
      pp_variance_stabiliser1$a <- perform_variance_stabilisation(pp_variance_stabiliser1$a)
      df_pre_list$l[["variance_stabilisation"]] <- pp_variance_stabiliser1$a@old_df
      df_post_list$l[["variance_stabilisation"]] <- pp_variance_stabiliser1$a@t@df
      processing_done_list$l[["variance_stabilisation"]] <- TRUE
      
      update_pp_snv_last_step_used(3, pp_snv_last_step_used$a)
      snv_variance_stabilisation_button_next()
      
      snv_message_out_without_tooltips("Variance stabilising normalisation was applied to the preview data", "Variance stabilising normalisation")
      disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, "snv_variance_stabilisation_button")
      snv_update_buttons_after_processing_data(disabled_buttons$a)
      
    })
  })
  
  observeEvent(input$snv_skip_mean_centering_button, {
    isolate({
      
      disabled_buttons$a <- add_to_disabled_buttons(disabled_buttons$a, c("snv_skip_mean_centering_button", "snv_mean_centering_button"))
      snv_mean_centering_button_next()
      step2_panel_visible$a <- TRUE
    })
  })
  
  observeEvent(input$snv_skip_button, {
    isolate({
      output_t1_f$skip_counter <- output_t1_f$skip_counter + 1
    })
  })
  
  observeEvent(input$snv_reset_preview_button, {
    isolate({
      
      
      reset_snv()
      disabled_buttons$a <- remove_from_disabled_buttons(disabled_buttons$a, c("snv_pareto_scaling_button", "snv_mean_centering_button", "snv_skip_mean_centering_button", "snv_unit_variance_scaling_button", "snv_quantile_normalisation_button", "snv_log_transformation_button", "snv_glog_transformation_button", "snv_variance_stabilisation_button", "snv_choice_radiobuttons", "snv_scaling_radiobutton"))
    })
  })
  
  observeEvent(input$snv_apply_button, {
    isolate({
      t1_f$a@df <- get_the_result_of_snv_pipeline(pp_snv_last_step_used$a)
      
      snv_message_out_without_tooltips("The preprocessing steps of scaling, normalisation and variance stabilisation were applied to input data", "Scaling, normalisation and variance stabilisation")
      output_t1_f$a <- t1_f$a
      output_t1_f$counter <- output_t1_f$counter + 1
      
    })
  })
  
  
  observeEvent(input$snv_choice_radiobuttons, {
    isolate({
      
      if(input$snv_choice_radiobuttons == "none") {
        snv_page_tracker$a <- 2
      } else if (input$snv_choice_radiobuttons == "scaling") {
        snv_page_tracker$a <- 3
      } else if (input$snv_choice_radiobuttons == "normalisation") {
        snv_page_tracker$a <- 4
      } else if (input$snv_choice_radiobuttons == "logarithmic_transformation") {
        snv_page_tracker$a <- 5
      } else if (input$snv_choice_radiobuttons == "variance_stabilisation") {
        snv_page_tracker$a <- 6
      }
      
      
    })
  })
  
  
  out_reactive <- reactive({list(output_t1_f$counter, output_t1_f$a, output_t1_f$skip_counter)})
  return(out_reactive)
}

