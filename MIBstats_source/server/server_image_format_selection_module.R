# _The module choosing image format when downloading a plot (.PNG or .SVG)

image_format_selection_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("choose_image_format_button"), "Plot file format...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("choose_image_format_button"), title = "Click here to choose the file format for saving a plot", placement = "left", trigger = "hover"),
    uiOutput(ns("plot_file_format_container"))
  )
}

image_format_selection_module <- function(input, output, session) {
  
  ns <- session$ns
  
  panel_tracking <- reactiveValues(file_format_panel_visible = FALSE)
  
  selected_file_format <- reactiveValues(a = NULL)
  selected_file_format$a <- "svg"
  
  observeEvent(input$choose_image_format_button, {
    isolate({
      panel_tracking$file_format_panel_visible <- !panel_tracking$file_format_panel_visible
      if(panel_tracking$file_format_panel_visible) {
        updateActionButton(session, "choose_image_format_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "choose_image_format_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observe({
    if (panel_tracking$file_format_panel_visible == TRUE) {
      isolate({
        output$plot_file_format_container <- renderUI({
          wellPanel(
            radioButtons(ns("file_format_radiobuttons"), "Image file format:",
                         c(".SVG" = "svg",
                           ".PNG" = "png"), selected = selected_file_format$a),
            textOutput(ns("current_file_format_textoutput")),
            actionButton(ns("pca_update_file_format_button"), "Update file format", icon("refresh", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("pca_update_file_format_button"), title = "Click here to update the file format for saving plots", placement = "left", trigger = "hover")
          )
        })
      })
      
    } else {
      isolate({
        output$plot_file_format_container <- renderUI({})
      })
    }
  })
  
  observe({
    if(selected_file_format$a == "svg") {
      isolate({
        output$current_file_format_textoutput <- renderText({
          "Currently selected: .SVG"
        })
      })
    } else if(selected_file_format$a == "png") {
      isolate({
        output$current_file_format_textoutput <- renderText({
          "Currently selected: .PNG"
        })
      })
    }
  })
  
  observeEvent(input$pca_update_file_format_button, {
    isolate({
      if(selected_file_format$a != input$file_format_radiobuttons) {
        selected_file_format$a <- input$file_format_radiobuttons
      }
    })
  })
  
  return(reactive({selected_file_format$a}))
}