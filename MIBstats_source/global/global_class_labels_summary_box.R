# _Module that displays a summary of table statistics, including the number of class labels.
# _Used in the input data loading page and on the page of setting up a two groups comparison test

class_labels_summary_boxUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    helpText("Summary statistics of the data table:"),
    tableOutput(ns("data_loading_summary_table_f2"))
  )

}

class_labels_summary_box <- function(input, output, session, data_summary_f) {
  ns <- session$ns
  
  data_for_table <- reactiveValues(a = NULL)
  
  observe({
    data_summary_observed <- data_summary_f()
    isolate({
      if(!is.null(data_summary_observed)) {
        data_for_table$a <- data_summary_observed
        if(!is.null(data_for_table$a)) {
          output$data_loading_summary_table_f2 <- renderTable(data_for_table$a, rownames = TRUE, selection = "none")
        }
      }
    })
  })
  
}