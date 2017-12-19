# _Module for saving R workspace

saving_r_workspace_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Saving R workspace"),
    textInput(ns("save_workspace_textinput"), "Please enter a name for the new workspace file:", ""),
    checkboxInput(ns("workspace_saving_overwrite_file_checkbox"), "Overwrite the file if a file with the same name already exists", TRUE),
    actionButton(ns("save_workspace_button"), label = "Save the workspace file"),
    bsTooltip(id = ns("save_workspace_button"), title = "Click here to save R workspace file (to the folder where this app is located)", 
              placement = "left", trigger = "hover"),
    htmlOutput(ns("workspace_saving_summary2")),
    htmlOutput(ns("workspace_saving_summary")),
    help_box_moduleUI(ns("workspace_saving_help_saving_and_loading_workspace"))
  )
}

saving_r_workspace_module <- function(input, output, session) {
  ns <- session$ns
  
  callModule(help_box_module, "workspace_saving_help_saving_and_loading_workspace")
  
  saving_in_progress <- reactiveValues(a = FALSE)
  saving_in_progress_reactive <- reactive({saving_in_progress$a})
  
  observe({
    saving_state <- saving_in_progress_reactive()
    if(!is.null(saving_state)) {
      isolate({
        if(saving_state) {
          output$workspace_saving_summary2 <- renderText({
            "Saving workspace..."
          })
        } else {
          output$workspace_saving_summary2 <- renderText({
            ""
          })
        }
      })
    }
  })
  
  observeEvent(input$save_workspace_button, {
    isolate({
      
      saving_in_progress$a <- TRUE
      
      outfile_w <- input$save_workspace_textinput
      if(outfile_w != "") {
        outfile_w <- add_extension_to_file_name_if_needed(outfile_w, "Rdata")
        
        modified_name <- make.names(c(outfile_w), unique = TRUE)
        
        invalid_characters_message <- paste("The file name entered by the user contains characters that cannot be used in a file name.\n",
                                            "A recommended new name for the file: ", modified_name, ".\n", "Please click the save button again to proceed with the new name", sep="")
        
        if(outfile_w != modified_name) {
          show_generic_error_message(invalid_characters_message)
          updateTextInput(session, "save_workspace_textinput",
                          value = modified_name, 
                          label = modified_name)
          return(NULL)
        }
        
        if(file.exists(outfile_w)) {
          if(!input$workspace_saving_overwrite_file_checkbox) {
            show_generic_error_message("A file with the specified name already exists")
            return(NULL)
          }
        }
        
        w.list <- NULL # init warning
        w.handler <- function(w){ # warning handler
          warn <- simpleWarning(w$message, w$call) # build warning
          w.list <<- c(w.list, paste(warn, collapse = " ")) # save warning
          invokeRestart("muffleWarning")
        }
        
        e.list <- NULL # init errors
        e.handler <- function(e){ # error handler
          err <- simpleError(e$message, e$call)
          e.list <<- c(e.list, paste(err, collapse = " ")) # save error
        } 
        
        mtry <- try(withCallingHandlers(save.image(outfile_w), warning = w.handler, error = e.handler), silent = TRUE)
        
        warning = w.list
        error = e.list
        
        if ((length(warning) == 0) && (length(error) == 0)) {
          save.image(outfile_w)
        } else {
          show_generic_error_message_w_e(error_message_f, warning, error)
        }
        
        outfile_w_summary <- paste("<b>The following workspace file was saved:</b><br><b>Name:</b> ", outfile_w, sep="")
        output$workspace_saving_summary <- renderText({
          outfile_w_summary
        })
      } else {
        return(NULL)
      }
    })
  })
}

