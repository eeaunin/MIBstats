# _Module for viewing the full table of data after loading it to the app

viewing_loaded_data_moduleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$h3("Loaded data"),
    column(12,
           dataTableOutput(ns("data_table_for_viewing"))
    ),
    uiOutput(ns("proceed_to_preprocessing_button_container")),
    help_box_moduleUI(ns("help_viewing_of_loaded_data"))
  )
  
}

viewing_loaded_data_module <- function(input, output, session, t1_f_reactive) {
  
  ns <- session$ns
  next_button_clicked <- reactiveValues(preprocessing = 0, test_setup = 0)
  
  callModule(help_box_module, "help_viewing_of_loaded_data")
  
  observe({
    t1_f <- t1_f_reactive()
    if(!is.null(t1_f)) {
      isolate({
        merged_frame <- get_merged_frame(t1_f)
        output$data_table_for_viewing <- DT::renderDataTable(merged_frame, rownames = FALSE, selection = "none")
        
        output$proceed_to_preprocessing_button_container <- renderUI({
          tagList(
            tags$hr(),
            helpText("Please choose whether to perform preprocessing"),
            actionButton(ns("proceed_to_preprocessing_button"), "Proceed to preprocessing", icon("chevron-right", class = NULL, lib = "font-awesome"), style = paste0("color: ", next_button_color)),
            bsTooltip(id = ns("proceed_to_preprocessing_button"), title = "Click here to proceed to preprocessing of the input data",
                      placement = "left", trigger = "hover"),
            actionButton(ns("proceed_to_test_setup_button"), "Proceed to setting up statistical tests", icon("chevron-right", class = NULL, lib = "font-awesome"), style = paste0("color: ", next_button_color)),
            bsTooltip(id = ns("proceed_to_test_setup_button"), title = "Click here to skip the preprocessing of the input data and proceed to setting up statistical tests",
                      placement = "left", trigger = "hover")
          )
        })
        
      })
    }
  })
  
  observeEvent(input$proceed_to_preprocessing_button, {
    isolate({
      next_button_clicked$preprocessing <- next_button_clicked$preprocessing + 1
    })
  })
  
  observeEvent(input$proceed_to_test_setup_button, {
    isolate({
      next_button_clicked$test_setup <- next_button_clicked$test_setup + 1
    })
  })

  return(reactive({list(next_button_clicked$preprocessing, next_button_clicked$test_setup)}))
}

