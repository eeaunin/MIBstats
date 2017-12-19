# _Module for the page that displays the help of all statistical methods in the app

statistical_methods_help_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Statistical methods"),
    htmlOutput(ns("statistical_methods_help_box"))
  )
}

statistical_methods_help_module <- function(input, output, session) {
  ns <- session$ns
  # _Reads help texts from html files
  
  html_header <- get_html_header()
  in_html2 <- load_help_html()
  in_html <- NULL
  for(item in in_html2) {
    in_html <- paste(in_html, item, sep="")
  }
  
  get_methods_string <- function(in_html_f) {
    splitter1 <- "<h2 class=\"Heading_20_2\"><a id=\"a__Statistical_methods\"><span/></a>Statistical methods</h2>"
    split_html <- unlist(strsplit(in_html_f, splitter1, fixed = TRUE))[2]
    splitter2 <- "<h1 class=\"Heading_20_1\"><a id=\"a__Results\"><span/></a>Results</h1>"
    split_html <- unlist(strsplit(split_html, splitter2, fixed = TRUE))[1]
    return(split_html)
  }
  
  get_references_string <- function(in_html_f) {
    splitter <- "<p class=\"Standard_20__28_user_29_\"><a id=\"_GoBack\"/>"
    split_html <- unlist(strsplit(in_html_f, splitter, fixed = TRUE))[2]
    return(split_html)
  }
  
  methods_string <- get_methods_string(in_html)
  references_string <- get_references_string(in_html)
  
  # _Displays help texts in text fields
  
  output$statistical_methods_help_box <- renderUI({
    HTML(paste(html_header, methods_string, references_string, sep=""))
  })
}