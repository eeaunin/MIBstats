# _This is the server part that deals with performing iGA


running_iga_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("iga_page_title_container")),
    tags$br(),
    tags$h4("Example data for iGA"),
    actionButton(ns("load_iga_example_data_container_button"), "Load example data for iGA...",  icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("load_iga_example_data_container_button"), title = "Click here to load iGA example data", placement = "left", trigger = "hover"),
    uiOutput(ns("loading_iga_example_data_container")),
    tags$hr(),
    tags$h4("Metrics for iGA"),
    radioButtons(ns("input_metrics_source_choice_radiobuttons"), "Please choose the source of the metrics in iGA:",
                 c("Use test results from the current session of this app" = "current_session",
                   "Load metrics from a CSV file" = "load_from_file")),
    uiOutput(ns("test_results_selectinput_container")),
    uiOutput(ns("loading_metrics_from_file_container")),
    actionButton(ns("add_label_to_iga_metrics_button"), "Add a label to the metrics data (optional)", icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("add_label_to_iga_metrics_button"), title = "Click here to add an optional label to the data", placement = "left", trigger = "hover"),
    uiOutput(ns("iga_metrics_label_container")),
    actionButton(ns("view_iga_metrics_button"), "View iGA metrics table", icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("view_iga_metrics_button"), title = "Click here to view iGA metrics table", placement = "left", trigger = "hover"),
    uiOutput(ns("metrics_table_container")),
    tags$hr(),
    tags$h4("Annotations for iGA"),
    fileInput(ns("iga_annotations_fileinput"), "Please load the CSV file that contains annotations for iGA",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    actionButton(ns("load_iga_annotations_from_file_button"), "Load iGA annotations from file"),
    bsTooltip(id = ns("load_iga_annotations_from_file_button"), title = "Click here to load iGA annotations from a .CSV file", placement = "left", trigger = "hover"),
    uiOutput(ns("loading_annotations_example_data_container")),
    actionButton(ns("view_iga_annotations_table_button"), "View iGA annotations table", icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("view_iga_annotations_table_button"), title = "Click here to view the table of iGA annotations", placement = "left", trigger = "hover"),
    uiOutput(ns("annotations_table_container")),
    tags$hr(),
    tags$h4("iGA settings"),
    actionButton(ns("view_iga_settings_button"), "iGA settings...", icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("view_iga_settings_button"), title = "Click here to modify iGA settings", placement = "left", trigger = "hover"),
    uiOutput(ns("iga_settings_container")),

    tags$hr(),
    actionButton(ns("run_iga_button"), "Run iGA"),
    bsTooltip(id = ns("run_iga_button"), title = "Click here to run iGA", placement = "left", trigger = "hover"),
    uiOutput(ns("iga_help_container"))
  )
}

running_iga_module <- function(input, output, session, rtr_reactive_f, iga_mode_f) {
  ns <- session$ns
  
  help_module_name <- "help_iga"
  if(iga_mode_f=="db-iga") {
    help_module_name <- "help_db_iga"
  } 
  
  output$iga_help_container <- renderUI({
    help_box_moduleUI(ns(help_module_name))
  })
  
  callModule(help_box_module, help_module_name)
  
  shinyjs::disable("load_iga_annotations_from_file_button")
  shinyjs::disable("get_metric_from_performed_tests")
  shinyjs::disable("view_iga_annotations_table_button")
  shinyjs::disable("run_iga_button")
  
  rtr_f <- reactiveValues(a = NULL)
  test_settings <- reactiveValues(l = NULL)
  test_results_selectinput_choices <- reactiveValues(l = NULL)
  fold_changes_direction <- reactiveValues(a = "group1_vs_group2")
  iga_direction <- reactiveValues(a = "increasing")
  loaded_data_frame <- reactiveValues(a = NULL)
  loaded_data_frame_reactive <- reactive({loaded_data_frame$a})
  annotation_booleans_matrix <- reactiveValues(a = NULL)
  loaded_test_type <- reactiveValues(a = NULL)
  panel_tracking <- reactiveValues(metrics = NULL, iga_example_data_visible = FALSE, metrics_table_visible = FALSE, annotations_table_visible = FALSE, test_results_selectinput_visible = FALSE, iga_settings_visible = FALSE, metrics_label_visible = FALSE)
  iga_result <- reactiveValues(a = NULL)
  iga_result_for_rtr <- reactiveValues(a = NULL)
  iga_result_for_rtr_reactive <- reactive({iga_result_for_rtr$a})
  current_rtr_item <- reactiveValues(a = NULL)
  metrics_file_name <- reactiveValues(a = NULL)
  metrics_data_label <- reactiveValues(a = NULL)
  
  
  iga1 <- reactiveValues(a = NULL)
  iga1$a <- iga_calculator()
  
  metrics_example_data <- reactiveValues(a = NULL)
  metrics_example_data$a <- read_csv_file("example_data/variables_and_metrics_for_iGA.csv", TRUE)
  annotations_example_data <- reactiveValues(a = NULL)
  annotations_example_data$a <- read_csv_file("example_data/annotations_matrix_for_iGA.csv", TRUE)
  
  reset_the_running_iga_module <- function() {
    # _Resets the variables that keep track of the state of the module
    loaded_data_frame$a <<- NULL
    annotation_booleans_matrix$a <<- NULL
    loaded_test_type$a <<- NULL
    iga_result$a <<- NULL
    iga_result_for_rtr$a <<- NULL
    current_rtr_item$a <<- NULL
    metrics_file_name$a <<- NULL
    metrics_data_label$a <<- NULL
    iga1$a <<- iga_calculator()
  }
  
  page_title <- "Setting up iGA"
  if(iga_mode_f=="db-iga") {
    page_title <- "Setting up db-iGA"
  } 
  
  output$iga_page_title_container <- renderUI ({
    tagList(
      tags$h3(page_title)
    )
  })
  
  observe({
    rtr_f$a <- rtr_reactive_f()
  })
  
  observe({
    panel_tracking$a <- input$input_metrics_source_choice_radiobuttons
  })
  
  observe({
    if(is.null(test_results_selectinput_choices$l) || length(test_results_selectinput_choices$l) == 0) {
      isolate({
        panel_tracking$test_results_selectinput_visible <- FALSE
      })
    } else {
      isolate({
        panel_tracking$test_results_selectinput_visible <- TRUE
      })
    }
  })
  
  observe({
    if(!is.data.frame(loaded_data_frame$a)) {
      isolate({
        shinyjs::disable("view_iga_metrics_button")
      })
    } else {
      isolate({
        shinyjs::enable("view_iga_metrics_button")
      })
    }
  })
  
  update_test_results_selectinput_choices <- function(test_settings_l) {
    # _Updates the selectInput that displays the list of performed statistical tests
    out_list <- list()
    for(pval_item in test_settings_l$pval_tests) {
      out_list <- append(out_list, list(rtr_test_parameters_to_string(pval_item)))
    }
    for(fc_settings_item in test_settings_l$fc_tests) {
      out_list <- append(out_list, list(rtr_test_parameters_to_string(fc_settings_item)))
    }
    return(out_list)
  }
  
  get_rtr_indices_by_selectinput_choice <- function(ind, test_settings_l) {
    # _Gets the indices showing thr results tracker (rtr) location of a test result based on the index (ind) selected by the user from selectInput
    rtr_location_indices <- NULL
    p_list_len <- length(test_settings_l$pval_tests)
    fc_list_len <- length(test_settings_l$fc_tests)
    
    selected_test_item <- NULL
    if(ind<= p_list_len) {
      selected_test_item <- test_settings_l$pval_tests[[ind]]
    } else {
      fc_list_ind <- ind - p_list_len
      selected_test_item <- test_settings_l$fc_tests[[fc_list_ind]]
    }
    if(!is.null(selected_test_item)) {
       rtr_location_indices <- selected_test_item$rtr_location
    }
    return(rtr_location_indices)
  }
  
  get_var_names_and_metric_by_rtr_indices <- function(rtr_indices_f, rtr_f_a) {
    # _Gets the variable names and the metric that will be used as the input for iGA.
    # _These values are retrieved from the results tracker (rtr) based on rtr indices
    out_list <- list()
    retrieved_result <- get_test_result_from_rtr(rtr_indices_f, rtr_f_a)
    variable_names_vector <- NULL
    metric_vector <- NULL
    
    if(retrieved_result@test_type == "fold_changes") {
      variable_names_vector <- as.character(retrieved_result@out_frame[,1])
      if(fold_changes_direction$a == "group1_vs_group2") {
        metric_vector <- retrieved_result@out_frame[,2]
      } else if (fold_changes_direction$a == "group2_vs_group1") {
        metric_vector <- retrieved_result@out_frame[,3]
      }
    } else {
      variable_names_vector <- as.character(retrieved_result@unadjusted_out_frame[,1])
      metric_vector <- retrieved_result@unadjusted_out_frame[,2]
    }
    out_list$variable_names <- variable_names_vector
    out_list$metric <- metric_vector
    
    loaded_data_frame$a <- data.frame(out_list$variable_names, out_list$metric)
    return(out_list)
  }
  
  get_test_type_by_selectinput_choice <- function(ind, test_settings_l, rtr_f_a) {
    # _Checks if the test selected from the test results selectInput is a fold changes test or a p-value yielding test
    test_type_f <- NULL
    rtr_indices_f <- get_rtr_indices_by_selectinput_choice(ind, test_settings_l)
    retrieved_result <- get_test_result_from_rtr(rtr_indices_f, rtr_f_a)
    if(retrieved_result@test_type == "fold_changes") {
      test_type_f <- "fc"
    } else {
      test_type_f <- "pval"
    }
    return(test_type_f)
  }
  
  observe({
    if(!is.null(rtr_f$a)) {
      isolate({
        test_settings$l <- get_test_queue_details_of_test_results(rtr_f$a, excluded_indices = c(7, 8))
        test_results_selectinput_choices$l <- update_test_results_selectinput_choices(test_settings$l)
      })
    }
  })
  
  observe({
    if(panel_tracking$a == "current_session" & panel_tracking$test_results_selectinput_visible) {
      isolate({
        output$test_results_selectinput_container <- renderUI ({
          tagList(
            selectInput(ns("test_results_selectinput"), "Available test results:", choices = test_results_selectinput_choices$l),
            actionButton(ns("get_metric_from_performed_tests"), "Get metrics for iGA from the selected statistical test"),
            bsTooltip(id = ns("get_metric_from_performed_tests"), title = "Click here to get the metrics for iGA from the current app session, from the selected statistical test", placement = "left", trigger = "hover")
          )
        })
        output$loading_metrics_from_file_container <- renderUI ({})
      })
    } else if(panel_tracking$a == "current_session" & !panel_tracking$test_results_selectinput_visible) {
      isolate({
        output$test_results_selectinput_container <- renderUI ({
          tagList(
            helpText("No results are available from the current app session")
          )
        })
        output$loading_metrics_from_file_container <- renderUI ({})
      })
    } else if (panel_tracking$a == "load_from_file") {
      isolate({
        output$loading_metrics_from_file_container <- renderUI ({
          tagList(
            fileInput(ns("iga_data_fileinput"), "Choose a CSV file that contains a iGA metrics",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            actionButton(ns("load_iga_data_from_file_button"), "Load iGA metrics from file"),
            bsTooltip(id = ns("load_iga_data_from_file_button"), title = "Click here to load iGA metrics data from a .CSV file", placement = "left", trigger = "hover")
          )
        })
        output$test_results_selectinput_container <- renderUI ({})
      })
    }
  })
  
  observe({
    if(panel_tracking$metrics_label_visible) {
      isolate({
        output$iga_metrics_label_container <- renderUI ({
          wellPanel(
            textInput(ns("metrics_label_textinput"), "Label:", value=metrics_data_label$a),
            actionButton(ns("metrics_label_ok_button"), "OK"),
            bsTooltip(id = ns("metrics_label_ok_button"), title = "Click here to assign a label to the metrics data", placement = "left", trigger = "hover")
          )
        })
      })
    } else {
      output$iga_metrics_label_container <- renderUI ({})
    }
  })
  
  observeEvent(input$metrics_label_ok_button, {
    isolate({
      metrics_data_label$a <- input$metrics_label_textinput
    })
  })
  
  observe({
    if(panel_tracking$iga_example_data_visible) {
      isolate({
        output$loading_iga_example_data_container <- renderUI ({
          wellPanel(
            actionButton(ns("load_iga_example_data_button"), "Load iGA example data"),
            bsTooltip(id = ns("load_iga_example_data_button"), title = "Click here to load iGA example data", placement = "left", trigger = "hover"),
            downloadButton(ns("download_metrics_example_data_button"), label = "Download metrics example data", class = NULL),
            bsTooltip(id = ns("download_metrics_example_data_button"), title = "Click here to download iGA metrics example data (to examine the file format)", placement = "left", trigger = "hover"),
            downloadButton(ns("download_annotations_example_data_button"), label = "Download the example data for annotations", class = NULL),
            bsTooltip(id = ns("download_annotations_example_data_button"), title = "Click here to download iGA annotations example data (to examine the file format)", placement = "left", trigger = "hover")
          )
        })
      })
    } else {
      output$loading_iga_example_data_container <- renderUI ({})
    }
  })
  
  observeEvent(input$load_iga_example_data_button, {
    isolate({
      error_loading_example_data <- FALSE
      if(!is.null(metrics_example_data$a) && is.data.frame(metrics_example_data$a)) {
        loaded_data_frame$a <- metrics_example_data$a
        metrics_file_name$a <- "example data"
        shinyjs::enable("load_iga_annotations_from_file_button")
      } else {
        error_loading_example_data <- TRUE
      }
      if(!is.null(annotations_example_data$a) && is.data.frame(annotations_example_data$a)) {
        numbers_matrix <- as.matrix(annotations_example_data$a[,2:ncol(annotations_example_data$a)])
        rownames(numbers_matrix) <- as.character(annotations_example_data$a[,1])
        
        annotation_booleans_matrix$a <- numbers_matrix
        shinyjs::enable("view_iga_annotations_table_button")
        shinyjs::enable("run_iga_button")
      } else {
        error_loading_example_data <- TRUE
      }
      if(error_loading_example_data) {
        show_generic_error_message("Example data file could not be loaded")
      } else {
        message_out("Example data was loaded", "iGA")
      }
    })
  })
  
  observe({
    if(!is.null(metrics_example_data$a)) {
      isolate({
        output$download_metrics_example_data_button <- downloadHandler(
          filename = function() {
            paste( Sys.Date(), "-data", ".csv", sep="")
          },
          content = function(file) {
            write.csv(metrics_example_data$a, file)
          }
        )
      })
    }
  })
  
  observe({
    if(!is.null(annotations_example_data$a)) {
      isolate({
        output$download_annotations_example_data_button <- downloadHandler(
          filename = function() {
            paste( Sys.Date(), "-data", ".csv", sep="")
          },
          content = function(file) {
            write.csv(annotations_example_data$a, file)
          }
        )
      })
    }
  })
  
  
  observe({
    if(panel_tracking$metrics_table_visible) {
      isolate({
        output$metrics_table_container <- renderUI ({
          tagList(
            tags$h4("iGA metrics table"),
            fluidRow(
              column(12,
                     dataTableOutput(ns("iga_input_data_table"))
              )
            )
          )
        })
      })
    } else {
      isolate({
        output$metrics_table_container <- renderUI ({})
      })
    }
  })
  
  observe({
    if(panel_tracking$iga_settings_visible) {
      isolate({
        output$iga_settings_container <- renderUI ({
          tagList(
            wellPanel(
              htmlOutput(ns("fold_changes_direction_htmloutput")),
              radioButtons(ns("fold_changes_direction_radiobutton"), "Choose new direction for fold changes:",
                           c("Group 1 / group 2" = "group1_vs_group2",
                             "Group 2 / group 1" = "group2_vs_group1")),
              actionButton(ns("change_fold_changes_direction_button"), "Change the direction of fold changes"),
              bsTooltip(id = ns("change_fold_changes_direction_button"), title = "Click here to change the direction of fold changes calculation", placement = "left", trigger = "hover")
            ),
            wellPanel(
              htmlOutput(ns("iga_direction_htmloutput")),
              radioButtons(ns("iga_direction_radiobutton"), "Choose new iGA direction:",
                           c("Increasing" = "increasing",
                             "Decreasing" = "decreasing")),
              actionButton(ns("change_iga_direction_button"), "Change the direction of iGA"),
              bsTooltip(id = ns("change_iga_direction_button"), title = "Change iGA direction", placement = "left", trigger = "hover")
            )
          )
        })
      })
    } else {
      isolate({
        output$iga_settings_container <- renderUI ({})
      })
    }
  })
  
  observeEvent(input$get_metric_from_performed_tests, {
    isolate({
      if(!is.null(input$test_results_selectinput)) {
        reset_the_running_iga_module()
        selectinp_choice <- input$test_results_selectinput
        ind <- which(test_results_selectinput_choices$l == selectinp_choice)
        
        rtr_indices <- get_rtr_indices_by_selectinput_choice(ind, test_settings$l)
        
        iga_data <- get_var_names_and_metric_by_rtr_indices(rtr_indices, rtr_f$a)
        
        current_rtr_item$a <- get_test_result_from_rtr(rtr_indices, rtr_f$a)
        
        old_iga_direction <- iga_direction$a
        loaded_test_type$a <- get_test_type_by_selectinput_choice(ind, test_settings$l, rtr_f$a)
        if(loaded_test_type$a=="pval" & old_iga_direction=="decreasing") {
          iga_direction$a <- "increasing"
          message_out("iGA direction was set to 'increasing' because p-values were loaded as the metric", "iGA")
        }
        shinyjs::enable("load_iga_annotations_from_file_button")
 
      }
    })
  })
  
  observe({
    loaded_data_frame_observed <- loaded_data_frame_reactive()
    isolate({
      if(!is.null(loaded_data_frame_observed)) {
        input_preview_table <- DT::datatable(loaded_data_frame$a, colnames = c("Variable name", "Metric"), rownames = FALSE, selection = 'none') 
        output$iga_input_data_table <- renderDataTable({input_preview_table})
      }
    })
  })
  
  observe({
    if(panel_tracking$annotations_table_visible) {
      isolate({
        output$annotations_table_container <- renderUI({
          tagList(
            tags$h4("iGA annotations table"),
            fluidRow(
              column(12,
                     dataTableOutput(ns("iga_annotations_table"))
              )
            )
          )
        })
      })
    } else {
      isolate({
        output$annotations_table_container <- renderUI({})
      })
    }
  })
  
  observe({
    if(!is.null(annotation_booleans_matrix$a)) {
      isolate({
        annotations_frame <- as.data.frame(annotation_booleans_matrix$a)
        rownames(annotations_frame) <- rownames(annotation_booleans_matrix$a)
        colnames(annotations_frame) <- colnames(annotation_booleans_matrix$a)
        annotations_table <- DT::datatable(as.data.frame(annotations_frame), rownames = TRUE, selection = 'none') 
        output$iga_annotations_table <- renderDataTable({annotations_table})
      })
    }
  })
  
  observeEvent(input$change_fold_changes_direction_button, {
    isolate({
      fold_changes_direction$a <- input$fold_changes_direction_radiobutton
    })
  })
  
  observeEvent(input$change_iga_direction_button, {
    isolate({
      if(!is.null(loaded_test_type$a) && (loaded_test_type$a == "pval" & input$iga_direction_radiobutton == "increasing")) {
        message_out("Warning: when using p-values as the metric, it is recommended to use the decreasing direction of iGA", "iGA")
      }
      iga_direction$a <- input$iga_direction_radiobutton
    })
  })
  
  observeEvent(input$view_iga_metrics_button, {
    isolate({
      panel_tracking$metrics_table_visible <- !panel_tracking$metrics_table_visible
      if(panel_tracking$metrics_table_visible) {
        updateActionButton(session, "view_iga_metrics_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "view_iga_metrics_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$view_iga_annotations_table_button, {
    isolate({
      panel_tracking$annotations_table_visible <- !panel_tracking$annotations_table_visible
      if(panel_tracking$annotations_table_visible) {
        updateActionButton(session, "view_iga_annotations_table_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "view_iga_annotations_table_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$load_iga_example_data_container_button, {
    isolate({
      panel_tracking$iga_example_data_visible <- !panel_tracking$iga_example_data_visible
      if(panel_tracking$iga_example_data_visible) {
        updateActionButton(session, "load_iga_example_data_container_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "load_iga_example_data_container_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$view_iga_settings_button, {
    isolate({
      panel_tracking$iga_settings_visible <- !panel_tracking$iga_settings_visible
      if(panel_tracking$iga_settings_visible) {
        updateActionButton(session, "view_iga_settings_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "view_iga_settings_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$add_label_to_iga_metrics_button, {
    isolate({
      panel_tracking$metrics_label_visible <- !panel_tracking$metrics_label_visible
      if(panel_tracking$metrics_label_visible) {
        updateActionButton(session, "add_label_to_iga_metrics_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "add_label_to_iga_metrics_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observe({
    if(fold_changes_direction$a == "group1_vs_group2") {
      isolate({
        output$fold_changes_direction_htmloutput <- renderText({ 
          "<b>Direction of fold changes in iGA, if fold changes are used as the metric</b><br><br><b>Currently selected: </b> group 1 / group 2<br>"
        })
      })
    } else if (fold_changes_direction$a == "group2_vs_group1") {
      isolate({
        output$fold_changes_direction_htmloutput <- renderText({ 
          "<b>Direction of fold changes in iGA, if fold changes are used as the metric</b><br><br><b>Currently selected: </b> group 2 / group 1<br>"
        })
      })
    }
  })
  
  observe({
    if(iga_direction$a == "increasing") {
      isolate({
        output$iga_direction_htmloutput <- renderText({ 
          "<b>Direction of iGA</b><br><br><b>Currently selected: </b> increasing<br>"
        })
      })
    } else if (iga_direction$a == "decreasing") {
      isolate({
        output$iga_direction_htmloutput <- renderText({ 
          "<b>Direction of iGA</b><br><br><b>Currently selected: </b> decreasing<br>"
        })
      })
    }
  })
  
  observeEvent(input$load_iga_data_from_file_button, {
    isolate({
      # _If the button to load data iGA input data from file is clicked
      infile1 <- input$iga_data_fileinput
      
      if(is.null(infile1)) {
        return(NULL)
      }
      reset_the_running_iga_module()
      metrics_file_name$a <- infile1$name
      
      table_data <- read_csv_file(infile1$datapath, TRUE)
      data_frame_ok <- TRUE
      error_report <- NULL
      if(!is.data.frame(table_data)) {
        data_frame_ok <- FALSE
        error_report <- ("The data could not be loaded in a data frame. ")
      } else {
        if(ncol(table_data) != 2 | nrow(table_data)<2) {
          data_frame_ok <- FALSE
          error_report <- ("The data table has wrong dimensions. ")
        } else {
          numbers_col <- as.numeric(table_data[2:nrow(table_data), 2])
          if(sum(is.numeric(numbers_col)) <1) {
            data_frame_ok <- FALSE
            error_report <- ("The data does not contain enough numeric values. ")
          }
        }
      }
      if(!data_frame_ok) {
        show_generic_error_message(paste("The specified file could not be loaded. ", error_report, "Please make sure the file fits the required format", sep=""))
        return(NULL)
      }
      table_data[,1] <- as.character(table_data[,1])
      table_data[2:nrow(table_data),2] <- as.numeric(table_data[2:nrow(table_data),2])
      loaded_data_frame$a <- table_data
      shinyjs::enable("load_iga_annotations_from_file_button")
      message_out("The iGA metrics file was loaded", "iGA")
    })
  })
  
  observeEvent(input$load_iga_annotations_from_file_button, {
    isolate({
      # _If the button to load data iGA annotations from file is clicked
      infile_annotations <- input$iga_annotations_fileinput
      
      if(is.null(infile_annotations)) {
        return(NULL)
      }
      annot_data <- read_csv_file(infile_annotations$datapath, TRUE)
      data_frame_ok <- TRUE
      error_report <- NULL
      if(!is.data.frame(annot_data)) {
        data_frame_ok <- FALSE
        error_report <- ("The data could not be loaded in a data frame. ")
      } else {
        if(ncol(annot_data) < 2 | nrow(annot_data)<2) {
          data_frame_ok <- FALSE
          error_report <- ("The data table has wrong dimensions. ")
        } else {
          numbers_matrix <- as.matrix(annot_data[,2:ncol(annot_data)])
          rownames(numbers_matrix) <- as.character(annot_data[,1])
          if(sum(is.numeric(numbers_matrix)) <1) {
            data_frame_ok <- FALSE
            error_report <- ("The data does not contain enough numeric values. ")
          } else {
            wrong_values_in_numbers_matrix <- sum(numbers_matrix !=0 & numbers_matrix !=1)
            if(wrong_values_in_numbers_matrix != 0) {
              error_report <- ("Values that are neither 1 nor 0 were found in the numeric matrix. ")
              data_frame_ok <- FALSE
            } else {
              loaded_data_variables <- loaded_data_frame$a[,1]
              annotations_file_variables <- rownames(numbers_matrix)
              # _Checks if all variables in the metrics file are annotated in the annotations file
              for(variable in loaded_data_variables) {
                if(!is.element(variable, annotations_file_variables)) {
                  error_report <- ("Mismatch in variable names between the iGA metrics table and the annotations table. ")
                  data_frame_ok <- FALSE
                }
              }
              # _If the annotation file contains variables that are not used in the metrics file, the excess annotations will be removed
              if(length(loaded_data_variables) != length(annotations_file_variables)) {
                new_numbers_matrix <- numbers_matrix
                remove_vector <- NULL
                for(annot_variable in annotations_file_variables) {
                  if(!is.element(annot_variable, loaded_data_variables)) {
                    remove_vector <- c(remove_vector, annot_variable)
                  }
                }
                if(!is.null(remove_vector)) {
                  new_numbers_matrix <- new_numbers_matrix[!rownames(new_numbers_matrix) %in% remove_vector, ]
                  
                  numbers_matrix <- new_numbers_matrix
                }
              }
            }
          }
        }
      }
      if(!data_frame_ok) {
        show_generic_error_message(paste("The specified file could not be loaded. ", error_report, "Please make sure the file fits the required format", sep=""))
        return(NULL)
      }
      message_out("The annotation file was loaded", "iGA")
      annotation_booleans_matrix$a <- numbers_matrix
      shinyjs::enable("view_iga_annotations_table_button")
      shinyjs::enable("run_iga_button")
    })

  })
  
  observeEvent(input$run_iga_button, {
    isolate({
      
      iga_metric <- as.numeric(loaded_data_frame$a[,2])
      var_names <- as.character(loaded_data_frame$a[,1])
      
      group_membership <- annotation_booleans_matrix$a
      iga_groups <- colnames(annotation_booleans_matrix$a)
      iga_decreasing <- FALSE
      if(iga_direction$a == "decreasing") {
        iga_decreasing <- TRUE
      }
      
      iga1$a <- set_iga_inputs(iga1$a, iga_metric, group_membership, iga_groups, var_names, iga_decreasing)
      
      if(iga_mode_f == "iga") {
        iga_result$a <- perform_iga(iga1$a)
      } else if (iga_mode_f == "db-iga") {
        iga_result$a <- perform_db_iga(iga1$a)
      }
      
      iga_input_data <- list(iga_metric, group_membership, iga_groups, var_names, iga_decreasing)
      
      
      iga_result_for_rtr$a <- prepare_iga_result_for_output(iga_result$a, current_rtr_item$a, iga_input_data, metrics_file_name$a)
      
    })
  })
  
  prepare_iga_result_for_output <- function(iga_result_a, current_rtr_item_a, iga_input_data_f, metrics_file_name_f) {
    # _Prepares the result entry that will be stored in the results tracker and sent to the module for viewing iGA results
    iga_result_entry <- two_groups_test_result()
    previous_test_type <- NULL
    if(!is.null(current_rtr_item_a)) {
      previous_test_type <- current_rtr_item_a@test_type
    }
    iga_result_entry@test_type = "iga"
    if(!is.null(current_rtr_item_a)) {
      iga_result_entry@group_a <- current_rtr_item_a@group_a
      iga_result_entry@group_b <- current_rtr_item_a@group_b
      iga_result_entry@header_unique <- current_rtr_item_a@header_unique
      iga_result_entry@group1_labels <- current_rtr_item_a@group1_labels
      iga_result_entry@group2_labels <- current_rtr_item_a@group2_labels
      iga_result_entry@p_adjustment <- current_rtr_item_a@p_adjustment
      iga_result_entry@paired_option <- current_rtr_item_a@paired_option
      iga_result_entry@test_queue_item_details <- current_rtr_item_a@test_queue_item_details
    }
    iga_list <- list()
    iga_list$results <- iga_result_a
    iga_list$input_data <- iga_input_data_f
    iga_list$previous_test_type <- previous_test_type
    input_data_type <- NULL
    if(!is.null(current_rtr_item_a)) {
      input_data_type <- paste(current_rtr_item_a@test_type, " (", vector_to_comma_separated_character(current_rtr_item_a@group_a), " vs ", vector_to_comma_separated_character(current_rtr_item_a@group_b), ")",  sep="")
    } else {
      input_data_type <- paste("loaded from file ('", metrics_file_name_f, "')", sep="")
    }
    
    iga_test_name <- NULL
    classes_count <- as.character(ncol(iga_input_data_f[[2]]))
    vars_count <- as.character(nrow(iga_input_data_f[[2]]))
    
    test_type_title <- NULL
    if(iga_mode_f == "iga") {
      test_type_title <- "iGA"
    } else if (iga_mode_f == "db-iga") {
      test_type_title <- "db-iGA"
    }
    
    name_part1 <- paste(test_type_title, ": ", vars_count, " variables, ", classes_count, " annotation classes. ", sep="")
    name_part2 <- paste("Input data: ")
    iga_test_name <- paste(name_part1, name_part2, input_data_type, sep="")
    if(!is.null(metrics_data_label$a)) {
      iga_test_name <- paste(iga_test_name, ", label: ", metrics_data_label$a, sep="")
    }
    
    iga_list$test_name <- iga_test_name
    
    iga_result_entry@iga <- iga_list
    return(iga_result_entry)
  }
  
  return(iga_result_for_rtr_reactive)
}