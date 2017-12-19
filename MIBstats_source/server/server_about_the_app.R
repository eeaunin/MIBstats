# _Module for displaying the "About" section of the app

about_the_app_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("About this app"),
    htmlOutput(ns("about_box"))
  )
}

about_the_app_module <- function(input, output, session) {
  ns <- session$ns
  # _Reads help texts from html files
  about_text_html <- read_plain_text_file("help_files/about_the_app.0html")
  
  # _Displays help texts in text fields
  output$about_box <- renderUI({
    HTML(paste(about_text_html))
  })
}