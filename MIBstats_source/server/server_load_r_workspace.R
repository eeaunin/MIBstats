# _Module for loading R workspace

loading_r_workspace_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Loading R workspace"),
    helpText("Please choose the R workspace file"),
    fileInput(ns("load_workspace_filebrowser"), "Choose the R workspace file (in .Rdata format)",
              accept = c(
                ".Rdata")
    ),
    actionButton(ns("load_workspace_button"), label = "Load R workspace from the selected file"),
    bsTooltip(id = ns("load_workspace_button"), title = "Click here to load R workspace from the selected file",
              placement = "left", trigger = "hover"),
    htmlOutput(ns("workspace_loading_summary")),
    help_box_moduleUI(ns("workspace_loading_help_saving_and_loading_workspace"))
  )
}

loading_r_workspace_module <- function(input, output, session) {
  ns <- session$ns
  
  callModule(help_box_module, "workspace_loading_help_saving_and_loading_workspace")
  
  observeEvent(input$load_workspace_button, {
    isolate({
      infile_w <- input$load_workspace_filebrowser
      if(file_is_properly_loaded_to_filebrowser(infile_w, list("Rdata"))) {
        
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
        
        mtry <- try(withCallingHandlers(load(infile_w$datapath), warning = w.handler, error = e.handler), silent = TRUE)
        
        warning = w.list
        error = e.list
        
        if ((length(warning) == 0) && (length(error) == 0)) {
          load(infile_w$datapath)
          infile_w_summary <- paste("<b>The following workspace file was loaded:</b><br><b>Name:</b> ", infile_w$name, "<br><b>Size:</b> ", infile_w$size, "<br><b>Type:</b> ", infile_w$type, "<br><b>Datapath:</b> ", infile_w$datapath, sep="")
          output$workspace_loading_summary <- renderText({
            infile_w_summary
          })
        } else {
          show_generic_error_message_w_e("Failed to load the workspace file", warning, error)
        }
        
      } else {
        return(NULL)
      }
    })
  })
  
}

