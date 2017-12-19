# _Module for the help page that contains the UI descriptions of the app pages

app_help_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("How to use the app (descriptions of the pages in the app)"),
    htmlOutput(ns("how_to_use_this_app"))
  )
}

app_help_module <- function(input, output, session) {
  ns <- session$ns
  
  html_header <- get_html_header()
  in_html2 <- load_help_html()
  in_html <- NULL
  for(item in in_html2) {
    in_html <- paste(in_html, item, sep="")
  }
  
  get_app_description_string <- function(in_html_f) {
    # _Loads the UI page description
    splitter1 <- "<h3 class=\"Heading_20_3\"><a id=\"a__Loading_of_data\"><span/></a>Loading of data</h3>"
    split_html <- unlist(strsplit(in_html_f, splitter1, fixed = TRUE))[2]
    split_html <- paste(splitter1, split_html, sep="")
    splitter2 <- "<h2 class=\"Heading_20_2\"><a id=\"a__Benchmarking_of_the_app\">"
    split_html <- unlist(strsplit(split_html, splitter2, fixed = TRUE))[1]
    return(split_html)
  }
  
  get_references_string <- function(in_html_f) {
    # _Loads the references list of the help
    splitter <- "<p class=\"Standard_20__28_user_29_\"><a id=\"_GoBack\"/>"
    split_html <- unlist(strsplit(in_html_f, splitter, fixed = TRUE))[2]
    return(split_html)
  }
  
  remove_server_part_texts <- function(in_str) {
    # _The help file texts have been taken from the MSc project report for this app, which also contains the descriptions of the server parts of the app.
    # _The descriptions of the server parts of the app will not be displayed in the app help. They are removed in this function
    splitter1 <- "</p><h3 class=\"Heading_20_3\">"
    splitter2 <- "<p class=\"Standard_20__28_user_29_\"><span class=\"T2\">Server part</span></p>"
    split_html <- unlist(strsplit(in_str, splitter1, fixed = TRUE))
    split_html2 <- NULL
    
    for(item in split_html) {
      split_item <- unlist(strsplit(item, splitter2, fixed = TRUE))[1]
      split_item <- paste("<p class=\"Standard_20__28_user_29_\">", splitter1, split_item, sep="")
      split_html2 <- c(split_html2, split_item)
    }
    
    
    split_html3 <- NULL
    for(item2 in split_html2) {
      split_html3 <- paste(split_html3, item2, sep="")
    }
    
    
    splitter3 <- "<p class=\"Standard_20__28_user_29_\"><span class=\"T2\">User interface</span></p>"
    split_html4 <- unlist(strsplit(split_html3, splitter3, fixed = TRUE))
    
    split_html5 <- NULL
    for(item3 in split_html4) {
      split_html5 <- paste(split_html5, item3, sep="")
    }

    return(split_html5)
  }
  
  app_description_string <- get_app_description_string(in_html)
  references_string <- get_references_string(in_html)
  
  app_description_string2 <- remove_server_part_texts(app_description_string)
  
  combined_string <- paste(html_header, app_description_string2, references_string, sep="")
  
  
  
  output$how_to_use_this_app <- renderUI({
    HTML(combined_string)
  })
  
}
