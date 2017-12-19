# _The module for viewing context-specific help in the app
# _The help texts that are going to be viewed are selected based on the name of the help module instance

help_box_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    actionButton(ns("show_help_button"), "", icon = icon("info-circle", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("show_help_button"), title = "Click here to view help text on this page", placement = "left", trigger = "hover"),
    uiOutput(ns("help_box_container")),
    htmlOutput(ns("help_box"))
  )
}

help_box_module <- function(input, output, session) {
  
  ns <- session$ns
  
  source("server/server_help_shared_functions.R")
  
  
  panel_tracking <- reactiveValues(help_visible = FALSE, references_visible = FALSE)
  input_html <- reactiveValues(a = NULL)
  
  load_help_sections_table <- function() {
    # _Loads the table that contains the information on what parts of the help texts correspond to which modules in the app
    help_sections_table_f <- read.csv(file = "help_files/help_sections.0csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
    return(help_sections_table_f)
  }
  
  get_help_table_row <- function(in_str, help_table_f) {
    # _Locates the part in the help sections table that is relevant for the module for which the help is displayed
    ind <- which(help_table_f[,1] == in_str)
    help_t_row <- c(help_table_f[ind, 2], help_table_f[ind, 3], help_table_f[ind, 4])
    return(help_t_row)
  }
  
  help_table <- load_help_sections_table()
  
  get_session_name <- function() {
    # _Gets the name of the module session
    current_session_name <- session$ns("name")
    current_session_name <- as.character(strsplit(current_session_name, "-name"))
    splitname <- unlist(strsplit(current_session_name, "-"))
    current_session_name <- splitname[2]
    return(current_session_name)
  }
  
  get_help_section_name <- function() {
    # _Extracts help section name from the name of the currently running module session
    session_name <- get_session_name()
    help_section_name_f <- unlist(strsplit(session_name, "help_"))[2]
    return(help_section_name_f)
  }
  
  help_section_name <- get_help_section_name()

  input_html_a <- load_help_html()
  
  load_help_string <- function(in_html2, help_section, help_type) {
    # _Loads help texts from the help html file
    split_input_in_half <- function(in_html_f, half_nr) {
      splitter <- "</p><h1 class=\"Heading_20_1\"><a id=\"a__Results\"><span/></a>Results</h1>"
      split_html <- unlist(strsplit(in_html_f, splitter, fixed = TRUE))
      in_html_f <- split_html[half_nr]
      return(in_html_f)
    }
    
    in_html <- NULL
    for(item in in_html2) {
      in_html <- paste(in_html, item, sep="")
    }
    
    
    help_section_underscores <- gsub(" ", "_", help_section, fixed = TRUE)
    help_section_underscores <- gsub(",", "_", help_section_underscores, fixed = TRUE)
    help_section_underscores <- gsub("Mann-Whitney", "Mann_Whitney", help_section_underscores, fixed = TRUE)
    
    query <- ""
    end_string <- ""
    if(help_type == "ui") {
      in_html <- split_input_in_half(in_html, 2)
      query <- paste("<h3 class=\"Heading_20_3\"><a id=\"a__", help_section_underscores, "\"><span/></a>", help_section, "</h3>", sep="")
      end_string <- "</p><p class=\"Standard_20__28_user_29_\"><span class=\"T2\">Server part</span></p>"
    } else if (help_type == "results_ui") {
      in_html <- split_input_in_half(in_html, 2)
      
      
      query <- paste("<h3 class=\"Heading_20_3\"><a id=\"a__", help_section_underscores, "\"><span/></a>", help_section, "</h3>", sep="")
      
      
      
      end_string <- "</p><p class=\"Standard_20__28_user_29_\"><span class=\"T2\">Server part</span></p><p class=\"P8\">"
    } else if (help_type == "methods") {
      in_html <- split_input_in_half(in_html, 1)
      
      query <- paste("<h3 class=\"Heading_20_3\"><a id=\"a__", help_section_underscores, "\"><span/></a>", help_section, "</h3>", sep="")
      
      
      end_string <- "<h3 class=\"Heading_20_3\">"
      
    } else if (help_type == "references") {
      in_html <- split_input_in_half(in_html, 2)
      query <- "<p class=\"Standard_20__28_user_29_\"><a id=\"_GoBack\"/>"
      end_string <- "</body></html>"
    }
    
    
    
    help_string <- ""
    search_result <- grep(query, in_html, fixed = TRUE)
    if(!is.null(search_result) && length(search_result != 0)) {
      splitstr <- unlist(strsplit(in_html, query, fixed = TRUE))
      splitstr2 <- splitstr[2]
      splitstr3 <- unlist(strsplit(splitstr2, end_string, fixed = TRUE))
      help_string <- paste(query, splitstr3[1], sep="")
    } 
    
    if(help_type == "results_ui") {
      help_string <- gsub("User interface", "User interface of the results page", help_string)
    }
    
    return(help_string)
  }
  
  get_output_string <- function(in_html, methods_str = NULL, ui_str = NULL, results_ui_str = NULL) {
    # _Combines the help texts for 1) a statistical method, 2) the app page of running the method and 3) the results page of the method in this app
    
    html_header_string <- get_html_header()
    
    help_string_methods <- NULL
    help_string_ui <- NULL
    help_string_results_ui <- NULL
    help_string_references <- NULL
    
    if(!is.null(methods_str) && length(methods_str) > 0) {
      help_string_methods <- load_help_string(in_html, methods_str, "methods")
      help_string_methods <- paste(help_string_methods, "<br>", sep="")
    }
    if(!is.null(ui_str) && length(ui_str) > 0) {
      help_string_ui <- load_help_string(in_html, ui_str, "ui")
    }
    if(!is.null(results_ui_str) && length(results_ui_str) > 0) {
      help_string_results_ui <- load_help_string(in_html, results_ui_str, "results_ui")
    }
   
    html_footer <- "</body></html>"
    output_string <- paste(html_header_string, help_string_methods, help_string_ui, help_string_results_ui, html_footer, sep="")
    return(output_string)
  }
  
  get_references_string <- function() {
    # _Gets the list of references for the help texts
    html_header_string <- get_html_header()
    help_string_references <- load_help_string(input_html_a, "none", "references")
    
    html_footer <- "</body></html>"
    output_string <- paste(html_header_string, help_string_references, html_footer, sep="")
    return(output_string)
  }
  
  
  help_topics <- help_table[,1]
  
  
  ht_row <- get_help_table_row(help_section_name, help_table)
  
  
  output_string <- get_output_string(input_html_a, methods_str = ht_row[1], ui_str = ht_row[2], results_ui_str = ht_row[3])
  
  references_string <- get_references_string()
  
  observeEvent(input$show_help_button, {
    isolate({
      panel_tracking$help_visible <- !panel_tracking$help_visible
    })
  })
  
  observe({
    if(panel_tracking$help_visible) {
      isolate({
        output$help_box_container <- renderUI({
          tagList(
            tags$h3("Help"),
            htmlOutput(ns("help_text_container")),
            
            actionButton(ns("view_references_button"), label = "View references list", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("view_references_button"), title = "Click here to view the list of references", placement = "left", trigger = "hover"),
            uiOutput(ns("references_container"))
          )
        })
        
        output$help_text_container <- renderText({
          output_string
        })
        
      })
    } else {
      isolate({
        output$help_box_container <- renderUI({})
      })
    }
  })
  
  observeEvent(input$view_references_button, {
    isolate({
      panel_tracking$references_visible <- !panel_tracking$references_visible
      
      if(panel_tracking$references_visible) {
        updateActionButton(session, "view_references_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "view_references_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observe({
    if(panel_tracking$references_visible) {
      isolate({
        output$references_container <- renderUI({
          tagList(
            HTML(references_string)
          )
        })
      })
    } else {
      isolate({
        output$references_container <- renderUI({})
      })
    }
  })
  
}