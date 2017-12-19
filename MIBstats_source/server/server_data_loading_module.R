# _This is the module that deals with loading input data

library(ggplot2)

data_loading_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Loading data"),
    
    uiOutput(ns("data_source_selection_container")),
    
    uiOutput(ns("example_data_container")),
    uiOutput(ns("user_data_container")),
    uiOutput(ns("input_data_loading_configuration")),
    uiOutput(ns("input_data_preview_interface")),
    tags$hr(),
    htmlOutput(ns("help_indicator_html")),
    help_box_moduleUI(ns("help_loading_of_data"))
    
  )
}

data_loading_module <- function(input, output, session) {
  # _Module for loading data
  
  ns <- session$ns
  
  callModule(help_box_module, "help_loading_of_data")
  input_data_loading_options <- reactiveValues(class_labels_source = "data_file", row_labels = TRUE, class_labels_separator_choice = "dot", log_transformed = FALSE, log_base = 2)
  
  idc1 <- reactiveValues(a = input_data_checker())
  submit_counter <- reactiveValues(a = 0)
  updated_summary <- reactiveValues(a = 0)
  out_list <- reactive({list(idc1$a, submit_counter$a, updated_summary$a)})
  panel_tracking <- reactiveValues(data_loaded = FALSE, user_data_visible = TRUE, file_loading_options_visible = FALSE, file2_visible = FALSE, row_and_column_statistics_visible = FALSE, sample_statistics_visible = FALSE, variable_statistics_visible = FALSE, sample_statistics_table_visible = FALSE)
  
  example_file_path <- reactiveValues(a = NULL)
  example_f_path <- "example_data/example_data.csv"
  example_file_path$a <- example_f_path
  example_data_frame <- reactiveValues(a = NULL)
  example_data_csv <- read_csv_file(example_f_path, FALSE)
  example_data_frame$a <- example_data_csv
  
  minimum_nr_of_cols <- reactiveValues(a = 6)
  
  output$help_indicator_html <- renderText({
    "&darr; Context-specific help is available throughout the app from the buttons with the information icon"
  })
  
  observeEvent(input$data_choice_radiobutton, {
    isolate({
      if(input$data_choice_radiobutton == "upload_file") {
        panel_tracking$user_data_visible <- TRUE
      } else if(input$data_choice_radiobutton == "example_data") {
        panel_tracking$user_data_visible <- FALSE
      }
    })
  })
  
  observe({
    if(!panel_tracking$data_loaded) {
      isolate({
        output$data_source_selection_container <- renderUI({
          tagList(
            tags$h4("Selecting files for loading"),
            radioButtons(ns("data_choice_radiobutton"), "Data source:",
                         c("Upload a data file to the app" = "upload_file",
                           "Use example data" = "example_data"), selected = "upload_file")
          )
        })
      })
    } else {
      isolate({
        output$data_source_selection_container <- renderUI({})
      })
    }
  })
  
  observe({
    if(!panel_tracking$data_loaded & panel_tracking$user_data_visible) {
      isolate({
        output$user_data_container <- renderUI({
          tagList(
            tags$hr(),
            tags$h5("Loading the data"),
            fileInput(ns("file1"), "Choose the data file (csv format)",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            actionButton(ns("load_data_table"), label = "Load data from the selected file(s)", icon("upload", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("load_data_table"), title = "Click here to load input data for the statistical analysis", placement = "left", trigger = "hover"),
            HTML("<br><br>"),
            actionButton(ns("configure_file_loading_button"), label = "File loading options...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("configure_file_loading_button"), title = "Click here to configure file loading options", placement = "left", trigger = "hover")
          )
        })
        output$example_data_container <- renderUI({})
      })
    } else if (!panel_tracking$data_loaded & !panel_tracking$user_data_visible) {
      isolate({
        output$example_data_container <- renderUI({
          tagList(
            tags$hr(),
            tags$h5("Example data"),
            actionButton(ns("load_example_data_table"), label = "Load example data file"),
            downloadButton(ns("download_example_data_table"), label = "Download the example data .CSV file"),
            bsTooltip(id = ns("download_example_data_table"), title = "Click here to download the example data .CSV file", placement = "left", trigger = "hover"),
            bsTooltip(id = ns("load_example_data_table"), title = "Click here to load an example data file", placement = "left", trigger = "hover")
          )
        })
        output$user_data_container <- renderUI({})
      })
    } else if (panel_tracking$data_loaded) {
      isolate({
        output$example_data_container <- renderUI({})
        output$user_data_container <- renderUI({})
      })
    }
  })
  
  output$download_example_data_table <- downloadHandler(
    filename = function() {
      "example_data.csv"
    },
    content = function(file) {
      write.csv(example_data_frame$a, file)
    }
  )
  
  
  file2_placeholder <- reactiveValues(a = "No file selected")
  class_label_radiobutton_selected <- reactiveValues(a = "data_file")

  update_panel_toggle <- function(button_name, variable_name) {
    panel_tracking[[variable_name]] <<- !panel_tracking[[variable_name]]
    if(panel_tracking[[variable_name]]) {
      updateActionButton(session, button_name, icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
    } else {
      updateActionButton(session, button_name, icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    }
  }

  
  
  observe({
    if(!panel_tracking$data_loaded & panel_tracking$file_loading_options_visible) {
      isolate({
        output$input_data_loading_configuration <- renderUI({
          wellPanel(
            tags$h4("File loading options"),
            tags$b("Scale of the input data"),
            checkboxInput(ns("log_transformed_input_checkbox"), "The data has been logarithmically transformed before loading", value = input_data_loading_options$log_transformed),
            uiOutput(ns("log_base_container")),
            tags$hr(),
            radioButtons(ns("class_labels_radiobutton"), "The loading of class labels:",
                         c("Load from the header of the data file" = "data_file",
                           "Load from a separate file" = "separate_file", "Do not load class labels and assign the same class for all samples" = "no_class_labels"), selected = input_data_loading_options$class_labels_source),
            tags$hr(),
            uiOutput(ns("class_labels_radiobutton_choice")),
            uiOutput(ns("class_labels_separator")),
            
            checkboxInput(ns("row_labels_checkbox"), "The first column of the input data file contains row labels", input_data_loading_options$row_labels),
            tags$hr(),
            actionButton(ns("options_apply_settings"), label = "Apply these settings"),
            bsTooltip(id = ns("options_apply_settings"), title = "Click here to apply the changes", placement = "left", trigger = "hover")
          )
        })
      })

    } else {
      isolate({
        if(!is.null(input$file2)) {
          file2_placeholder$a <- input$file2$name
        }
        output$input_data_loading_configuration <- renderUI({})
      })
    }
  })
  
  observeEvent(input$log_transformed_input_checkbox, {
    isolate({
      if(input$log_transformed_input_checkbox) {
        output$log_base_container <- renderUI({
          tagList(
            numericInput(ns("log_base_input"), "Base of the logarithm:", value = input_data_loading_options$log_base, min = 0.01)
          )
        })
      } else ({
        output$log_base_container <- renderUI({})
      })
    })
  })
  
  
  observe({
    if(panel_tracking$file2_visible) {
      isolate({
        output$class_labels_radiobutton_choice <- renderUI({
          tagList(
            fileInput(ns("file2"), "Choose the class labels file (csv format)", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), placeholder = file2_placeholder$a),
            tags$hr()
          )
        })
        output$class_labels_separator <- renderUI({})
      })
    } else {
      isolate({
        file2_placeholder$a <- input$file2$name
        output$class_labels_radiobutton_choice <- renderUI({})
        
        
      })
    }
  })
  
  observe({
    if(class_label_radiobutton_selected$a == "data_file") {
      isolate({
        output$class_labels_separator <- renderUI({
          tagList(
            selectInput(ns("class_labels_separator_choice"), "Separator symbol between class labels and column labels:",
                        c("." = "dot",
                          "-" = "hyphen",
                          "_" = "underscore"), selected = input_data_loading_options$class_labels_separator_choice),
            helpText("Note: spaces will be converted to dots when loading the file"),
            tags$hr()
          )
        })
      })
    } else {
      isolate({
        output$class_labels_separator <- renderUI({})
      })
    }
  })
  
  
  
  
  observeEvent(input$options_apply_settings, {
    isolate({
      

      log_ok <- TRUE
      if(input$log_transformed_input_checkbox) {
        if(!is.null(input$log_base_input) && is.numeric(input$log_base_input) & input$log_base_input > 0 & input$log_base_input != 1) {
          input_data_loading_options$log_base <- input$log_base_input
        } else {
          log_ok <- FALSE
          updateNumericInput(session, "log_base_input", value = input_data_loading_options$log_base)
          show_generic_error_message("The value entered as the logarithm base is not a valid value for the base of logarithm (non-numeric or negative values and 1 are not accepted)")
        }
      }
      
      if(log_ok) {
        input_data_loading_options$row_labels <- input$row_labels_checkbox
        input_data_loading_options$class_labels_separator_choice <- input$class_labels_separator_choice
        input_data_loading_options$log_transformed <- input$log_transformed_input_checkbox
        input_data_loading_options$class_labels_source <- input$class_labels_radiobutton
        message_out("The changes were applied to file loading settings", "Loading input data")
      }
      
    })
  })
  
  observeEvent(input$class_labels_radiobutton, {
    isolate({
      class_label_radiobutton_selected$a <- input$class_labels_radiobutton
      if(input$class_labels_radiobutton == "separate_file") {
        panel_tracking$file2_visible <- TRUE
      } else if(input$class_labels_radiobutton == "data_file" | input$class_labels_radiobutton == "no_class_labels") {
        panel_tracking$file2_visible <- FALSE
        if(!is.null(input$file2)) {
          file2_placeholder$a <- input$file2$name
        }
      }
    })
  })

  
  observeEvent(input$configure_file_loading_button, {
    isolate({
      update_panel_toggle("configure_file_loading_button", "file_loading_options_visible")
    })
  })

  check_loading_of_files <- function(infile1_f, infile2_f) {
    # _Function for checking if files are loaded to filebrowsers for the data file and the class labels file
    file_loading_ok <- TRUE
    
    if(input_data_loading_options$class_labels_source == "separate_file") {
      if(!file_is_properly_loaded_to_filebrowser(infile1_f, list("csv", "txt", "CSV", "TXT"))) {

        reset("file1")
        file_loading_ok <- FALSE
        
        if(!file_is_properly_loaded_to_filebrowser(infile2_f, list("csv", "txt", "CSV", "TXT"))) {
          file_loading_ok <- FALSE
        }
      }
    } else {
      if(!file_is_properly_loaded_to_filebrowser(infile1_f, list("csv", "txt", "CSV", "TXT"))) {
        reset("file1")
        file_loading_ok <- FALSE
      }
    }
    return(file_loading_ok)
  }
  
  observe({
    if(panel_tracking$row_and_column_statistics_visible) {
      isolate({
        output$row_and_column_statistics_uioutput <- renderUI ({
          tagList(
            wellPanel(
              tags$h4("Row and column statistics"),
              class_labels_summary_boxUI(ns("class_labels_summary_box_file_loading"))
            )
          )
        })
      })
    } else {
      isolate({
        output$row_and_column_statistics_uioutput <- renderUI ({})
      })
    }
  })
  
  observeEvent(input$view_row_and_column_statistics_button, {
    isolate({
      update_panel_toggle("view_row_and_column_statistics_button", "row_and_column_statistics_visible")
    })
  })
  
  observe({
    if(panel_tracking$sample_statistics_visible) {
      isolate({
        output$sample_statistics_uioutput <- renderUI ({
          tagList(
            tags$h4("Samples (columns)"),
            plotOutput(ns("sample_statistics_plot"), height = "100%", width = "100%"),
            actionButton(ns("view_sample_statistics_table_button"), label = "View as NA statistics table...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("view_sample_statistics_table_button"), title = "Click here to view NA statistics of samples as a table", placement = "left", trigger = "hover"),
            uiOutput(ns("sample_statistics_table_uioutput"))
          )
        })
      })
    } else {
      isolate({
        output$sample_statistics_uioutput <- renderUI ({})
      })
    }
  })
  
  observe({
    if(panel_tracking$sample_statistics_table_visible) {
      isolate({
        output$sample_statistics_table_uioutput <- renderUI ({
          tagList(
            helpText("Summary statistics about samples (columns) in the table:"),
            fluidRow(
              column(12, dataTableOutput(ns("column_statistics_table")))
            )
          )
        })
      })
    } else {
      isolate({
        output$sample_statistics_table_uioutput <- renderUI ({})
      })
    }
  })
  
  observeEvent(input$view_sample_statistics_table_button, {
    isolate({
      update_panel_toggle("view_sample_statistics_table_button", "sample_statistics_table_visible")
    })
  })
  
  observeEvent(input$view_sample_statistics_button, {
    isolate({
      update_panel_toggle("view_sample_statistics_button", "sample_statistics_visible")
    })
  })
  
  observe({
    if(panel_tracking$variable_statistics_visible) {
      isolate({
        output$variable_statistics_uioutput <- renderUI ({
          tagList(
            tags$h4("Variables (rows)"),
            helpText("Summary statistics about variables (rows) in the table:"),
            fluidRow(
              column(12,
                     dataTableOutput(ns("row_statistics_table")))
            )
          )
        })
      })
    } else {
      isolate({
        output$variable_statistics_uioutput <- renderUI ({})
      })
    }
  })
  
  observeEvent(input$view_variable_statistics_button, {
    isolate({
      update_panel_toggle("view_variable_statistics_button", "variable_statistics_visible")
    })
  })
  
  
  
  generate_samples_composition_plot <- function(col_summary_frame) {
    # _Generates the stacked bar chart that shows the percentages of numeric and NA values in samples
    nr_of_samples <- nrow(col_summary_frame)
    var_type_vector <- c(rep("Numeric", nr_of_samples), rep("NA", nr_of_samples))
    sample_names_vector <- c(rep(rownames(col_summary_frame), 2))
    percentage_not_na <- (col_summary_frame$not_na/(col_summary_frame$not_na + col_summary_frame$is_na))*100
    percentage_is_na <- (col_summary_frame$is_na/(col_summary_frame$not_na + col_summary_frame$is_na))*100
    
    percentage_vector2 <- NULL
    for(i in 1:length(percentage_not_na)) {
      percentage_vector2 <- c(percentage_vector2, percentage_not_na[i])
      percentage_vector2 <- c(percentage_vector2, percentage_is_na[i])
    }
    
    percentage_vector <- c(percentage_not_na, percentage_is_na)
    plotting_data <- data.frame(var_type_vector, sample_names_vector, percentage_vector)
    
    plotting_data <- ddply(plotting_data, .(sample_names_vector),
                           transform, pos = cumsum(percentage_vector) - (0.5 * percentage_vector))
    
    
    percentage_character_vector <- as.character(percentage_vector2)
    for(i in 1:length(percentage_character_vector)) {
      if(percentage_character_vector[i] == "0") {
        percentage_character_vector[i] <- ""
      } else {
        percentage_character_vector[i] <- paste(percentage_character_vector[i], "%")
      }
    }
    
    p4 <- ggplot() + geom_bar(aes(y = percentage_vector, x = sample_names_vector, fill = var_type_vector), data = plotting_data,
                              stat="identity")
    p4 <- p4 + geom_text(data=plotting_data, aes(x = sample_names_vector, y = pos, label = percentage_character_vector),
                         size=4) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("Composition of values by sample (%)") + 
      theme(legend.title = element_blank()) +
      xlab("Samples") +
      ylab("Composition (%)")
    return(p4)
  }
  
  observeEvent(input$load_example_data_table, {
    isolate({
      input_data_loading_options$class_labels_source <- "data_file"
      input_data_loading_options$row_labels <- TRUE
      input_data_loading_options$class_labels_separator_choice <- "dot"
      input_data_loading_options$log_transformed <- TRUE
      
      data_loading_response <- load_data_from_file(example_file_path$a, NULL, input_data_loading_options, idc1$a, minimum_nr_of_cols$a)
      if(is.null(data_loading_response$table_error_report)) {
        idc1$a <- data_loading_response$idc1
        updated_summary$a <- data_loading_response$updated_summary
        panel_tracking$data_loaded <- data_loading_response$data_loaded
        
        
        callModule(class_labels_summary_box, "class_labels_summary_box_file_loading", reactive({updated_summary$a$data_summary_frame_class_label_statistics}))
      }
    })
  })
  
  observeEvent(input$load_data_table, {
    isolate({
      if(!check_loading_of_files(input$file1, input$file2)) {
        return(NULL)
      }
      data_loading_response <- load_data_from_file(input$file1$datapath, input$file2$datapath, input_data_loading_options, idc1$a, minimum_nr_of_cols$a)
      if(is.null(data_loading_response$table_error_report)) {
        idc1$a <- data_loading_response$idc1
        updated_summary$a <- data_loading_response$updated_summary
        panel_tracking$data_loaded <- data_loading_response$data_loaded
        callModule(class_labels_summary_box, "class_labels_summary_box_file_loading", reactive({updated_summary$a$data_summary_frame_class_label_statistics}))
      }
    })
  })
  
  add_row_labels_column <- function(table_data_f) {
    # _Function that adds row labels (consisting of row numbers) as the first column of a data frame
    row_titles <- seq(1, nrow(table_data_f), 1)
    row_titles_frame <- data.frame(matrix(NA, nrow = length(row_titles), ncol = 1))
    colnames(row_titles_frame) <- c("X")
    row_titles_frame$X <- c(row_titles)
    table_data_f <- cbind(row_titles_frame, table_data_f)
    return(table_data_f)
  }
  
  extract_class_labels_from_data_file <- function(idc1_a, input_data_loading_options_f) {
    # _Server function to get class labels data from the header of the data file
    out_list_f <- list()
    out_list_f$error <- FALSE
    table_class_labels <- data.frame()
    labels_separator <- ""
    
    if(input_data_loading_options_f$class_labels_separator_choice == "dot") {
      labels_separator <- "."
    } else if(input_data_loading_options_f$class_labels_separator_choice == "hyphen") {
      labels_separator <- "-"
    } else if(input_data_loading_options_f$class_labels_separator_choice == "underscore") {
      labels_separator <- "_"
    }
    
    table_class_labels_extracted <- get_class_labels_from_data_file(idc1_a, TRUE, labels_separator)
    if(!table_class_labels_extracted$error_occurred) {
      idc1_a@class_labels_csv <- table_class_labels_extracted$data_frame
    } else {
      out_list_f$error <- TRUE
    }
    out_list_f$idc1 <- idc1_a
    out_list_f$error_message <- table_class_labels_extracted$error_message
    return(out_list_f)
  }
  
  generate_class_labels_frame_with_one_class <- function(table_data_f) {
    # _If no class labels are provided by the user, all the samples are assigned one class, labelled as "Untitled". A new class labels data frame is created for this.
    cols_count <- ncol(table_data_f)-1
    untitled_class_labels_vector <- c(rep("Untitled", cols_count))
    untitled_class_labels_row_titles_vector <- as.character(c(1:cols_count))
    untitled_class_labels_frame_f <- as.data.frame(cbind(untitled_class_labels_row_titles_vector, untitled_class_labels_vector), stringsAsFactors = FALSE)
    colnames(untitled_class_labels_frame_f) <- c("", "x")
    return(untitled_class_labels_frame_f)
  }
  
  show_data_preview_interface <- function() {
    # _Displays the UI elements that appear after loading a file
    output$input_data_preview_interface <- renderUI({
      tagList(
        tags$h4("Preview of the loaded data"),
        tags$h5("Table preview"),
        helpText("Preview of the data (only up to 8 rows and columns are displayed in the preview):"),
        fluidRow(
          column(12,
                 dataTableOutput(ns("data_loading_preview_table")))
        ),
        tags$hr(),
        actionButton(ns("view_row_and_column_statistics_button"), label = "View summary table of rows and columns...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
        bsTooltip(id = ns("view_row_and_column_statistics_button"), title = "Click here to view the summary table of row and column statistics", placement = "left", trigger = "hover"),
        uiOutput(ns("row_and_column_statistics_uioutput")),
        tags$hr(),
        actionButton(ns("view_sample_statistics_button"), label = "View NA value statistics of samples...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
        bsTooltip(id = ns("view_sample_statistics_button"), title = "Click here to view statistics of NA values in samples", placement = "left", trigger = "hover"),
        uiOutput(ns("sample_statistics_uioutput")),
        tags$hr(),
        actionButton(ns("view_variable_statistics_button"), label = "View variable statistics...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
        bsTooltip(id = ns("view_variable_statistics_button"), title = "Click here to view statistics of variables", placement = "left", trigger = "hover"),
        uiOutput(ns("variable_statistics_uioutput")),
        uiOutput(ns("proceed_to_viewing_data_table_button_container"))
      )
    })
  }
  
  generate_preview_data_frame <- function(idc1_a) {
    # _Function to generate the data frame that will be displayed as the data preview table after loading the data
    t1_f2 <- input_data_table()
    t1_f2@df <- idc1_a@mydata_csv
    t1_f2@class_labels <- idc1_a@class_labels_csv
    
    merged_frame <- get_merged_frame(t1_f2)
    
    # _Cropping the data loading preview table
    merged_frame_preview_f <- merged_frame
    if(ncol(merged_frame_preview_f)>10) {
      merged_frame_preview_f <- merged_frame_preview_f[,1:9]
    }
    if(nrow(merged_frame_preview_f)>10) {
      merged_frame_preview_f <- merged_frame_preview_f[1:10,]
    }
    merged_frame_preview_f <- add_header_titles_to_merged_frame(input_data_table(), merged_frame_preview_f)
    return(merged_frame_preview_f)
  }
  
  get_updated_summary <- function(idc1_a) {
    # _Updates the data summary of the loaded data
    t1_f2 <- input_data_table()
    t1_f2@df <- idc1_a@mydata_csv
    t1_f2@class_labels <- idc1_a@class_labels_csv
    updated_summary_f2 <- update_input_data_summary(t1_f2)
    return(updated_summary_f2)
  }
  
  generate_row_summary_frame <- function(idc1_a) {
    # _Generates the data frame that contains the summary of variables (rows) in the loaded data
    t1_f2 <- input_data_table()
    t1_f2@df <- idc1_a@mydata_csv
    t1_f2@class_labels <- idc1_a@class_labels_csv
    t_merged_frame <- transpose(get_merged_frame(t1_f2))
    row_titles <- t_merged_frame[1,3:ncol(t_merged_frame)]
    t_merged_frame_cropped <- t_merged_frame[2:nrow(t_merged_frame),3:ncol(t_merged_frame)]
    idc1_a@t_merged_frame <- t_merged_frame
    data_summary_by_variable <- summarise_data_frame_statistics_by_variable(idc1_a)
    row_summary_frame <- prepare_table_statistics_data_frame_by_variable(idc1_a, data_summary_by_variable, row_titles)
    return(row_summary_frame)
  }
  
  display_the_next_step_container <- function() {
    # _Displays the button for proceeding to the next page
    output$proceed_to_viewing_data_table_button_container <- renderUI({
      tagList(
        tags$hr(),
        helpText("Click \"Next step\" to proceed to viewing the full data table"),
        actionButton(ns("proceed_to_viewing_full_data_table_button"), "Next step", icon("chevron-right", class = NULL, lib = "font-awesome"), style = paste0("color: ", next_button_color)),
        bsTooltip(id = ns("proceed_to_viewing_full_data_table_button"), title = "Click here to proceed to viewing the full data table",
                  placement = "left", trigger = "hover")
      )
    })
  }
  
  display_samples_composition_plot <- function(col_summary_frame_f) {
    # _Displays the plot of NA and numeric value composition of samples
    samples_plot <- generate_samples_composition_plot(col_summary_frame_f)
    output$sample_statistics_plot <- renderPlot({
      samples_plot
    }, height = 500, width = 1000)
  }

  load_data_from_file <- function(infile1_datapath, infile2_datapath, input_data_loading_options_f, idc1_a, min_cols) {
    # _Function for loading the input data
    out_list_f <- list()
    updated_summary_f <- NULL
    data_loaded_f <- FALSE
    
    table_data <- read_csv_file(infile1_datapath, TRUE)

    if(!input_data_loading_options_f$row_labels) {
      table_data <- add_row_labels_column(table_data)
    }
    
    table_class_labels <- data.frame()
    idc1_a@mydata_csv <- table_data
    idc1_a@log_transformed <- input_data_loading_options_f$log_transformed
    idc1_a@log_base <- input_data_loading_options_f$log_base
    table_data_file_check <- check_data_frame(idc1_a, TRUE, 1, min_cols)
    
    error_occurred_when_extracting_class_labels <- FALSE
    class_labels_extraction_error_message <- ""
    class_labels_extraction_report <- list()
    
    if(input_data_loading_options_f$class_labels_source == "separate_file") {
      table_class_labels <- read_csv_file(infile2_datapath, TRUE)
      idc1_a@class_labels_csv <- table_class_labels
    } else {
      if (input_data_loading_options_f$class_labels_source == "data_file") {
        if(table_data_file_check$result == TRUE) {
          # _extract class labels from data file
          class_labels_extraction_response <- extract_class_labels_from_data_file(idc1_a, input_data_loading_options_f)
          idc1_a <- class_labels_extraction_response$idc1
          class_labels_extraction_error_message <- class_labels_extraction_response$error_message
          error_occurred_when_extracting_class_labels <- class_labels_extraction_response$error
        } else {
          error_occurred_when_extracting_class_labels <- TRUE
        }
        
      } else if (input_data_loading_options_f$class_labels_source == "no_class_labels"){
        idc1_a@class_labels_csv <- generate_class_labels_frame_with_one_class(table_data)
      }
    }
    class_labels_extraction_report$error_occurred <- error_occurred_when_extracting_class_labels
    class_labels_extraction_report$error_message <- class_labels_extraction_error_message
    
    table_class_labels_check <- list()
    table_class_labels_check$result <- FALSE
    table_class_labels_check$error_message <- ""
    
    if(table_data_file_check$result == TRUE & !error_occurred_when_extracting_class_labels) {
      table_class_labels_check <- check_class_labels_file(idc1_a, table_data_file_check$col_count)
      if(table_class_labels_check$result == TRUE) {
        
        data_loaded_f <- TRUE
        show_data_preview_interface()
        merged_frame_preview <- generate_preview_data_frame(idc1_a)

        # _Displays the results in a table
        output$data_loading_preview_table <- DT::renderDataTable(merged_frame_preview, rownames = FALSE, selection = "none")

        updated_summary_f <- get_updated_summary(idc1_a)
        idc1_a@summary <- updated_summary_f$data_summary
        
        col_summary_frame <- prepare_table_statistics_data_frame_by_sample(idc1_a)
        output$column_statistics_table <- DT::renderDataTable(col_summary_frame, colnames=c("Numeric values", "NA values"), rownames = TRUE, selection = "none")
        display_samples_composition_plot(col_summary_frame)
        
        row_summary_frame <- generate_row_summary_frame(idc1_a)
        
        output$row_statistics_table <- DT::renderDataTable(row_summary_frame, colnames=c("Numeric values", "NA values", "Minimum", "Maximum", "Mean"), rownames = TRUE, selection = "none")
        display_the_next_step_container()
      }
    }

    table_error_report <- data_loading_error_report_for_output(idc1_a, table_data_file_check, class_labels_extraction_report, table_class_labels_check)
    if(!is.null(table_error_report)) {
      show_generic_error_message(table_error_report)
    }
    
    out_list_f$idc1 <- idc1_a
    out_list_f$updated_summary <- updated_summary_f
    out_list_f$data_loaded <- data_loaded_f
    out_list_f$table_error_report <- table_error_report
    return(out_list_f)
  }
  
  observeEvent(input$proceed_to_viewing_full_data_table_button, {
    isolate({
      submit_counter$a <- submit_counter$a + 1
    })
  })
  
  return(out_list)
}