# _Module for downloading test statistics of one variable (selected by the user by clicking on a row of the results table)

two_groups_test_data_one_variable_statistics_downloadUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("two_groups_test_results_one_variable_statistics_download_button"))
  )
}

two_groups_test_data_one_variable_statistics_download <- function(input, output, session, output_str_f2) {
  ns <- session$ns
  
  output$two_groups_test_results_one_variable_statistics_download_button = renderUI({
    
    tagList(
      downloadButton(ns("download_one_variable_statistics"), "Download this list"),
      bsTooltip(id = ns("download_one_variable_statistics"), title = "Click here to download the list of test statistics of the selected variable", 
                placement = "left", trigger = "hover")
    )
    
  })
  
  output$download_one_variable_statistics <- downloadHandler(
    filename = function() {
      paste( Sys.Date(), "-data", ".txt", sep="")
    },
    content = function(file) {
      write(output_str_f2, file)
    }
  )
  
}
