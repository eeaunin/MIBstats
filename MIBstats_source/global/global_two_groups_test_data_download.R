# _Module for handling the downloading the data of the t-test, F-test and Mann-Whitney U test from the test results pages

two_groups_test_data_downloadUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("download_test_results_selector"))
  )
}

two_groups_test_data_download <- function(input, output, session, two_groups_test_result_r_f2, data_rtr, data_rtr_sel, selected_test_f, pval_correction_input_f) {
  ns <- session$ns
  

  output$download_test_results_selector <- renderUI({
    wellPanel(
      helpText("Downloading test results from this results page"),
      selectInput(ns("results_set_for_download"), "Choose a results set for downloading:", choices = c("p-values table" = "p_values_table", "Complete set of test statistics" = "complete_set_of_test_statistics")),
      downloadButton(ns("download_test_results"), "Download"),
      bsTooltip(id = ns("download_test_results"), title = "Click here to download the list of test results", 
                placement = "left", trigger = "hover")
    )
  })
  
  pval_correction_mode <- reactiveValues(a = FALSE)
  selected_test <- reactiveValues(a = NULL)
  two_groups_test_result_r_f <- reactiveValues(a = NULL)
  
  results_set_for_download_reactiveval <- reactiveValues(a = "p_values_table")
  
  observe({
    two_groups_test_result_r_f$a <- two_groups_test_result_r_f2()
  })
  
  observe({
    pval_correction_mode$a <- pval_correction_input_f()
  })
  
  observe({
    selected_test$a <- selected_test_f()
  })
  
  observeEvent(input$results_set_for_download, {
    isolate({
      if(input$results_set_for_download == "p_values_table") {
        results_set_for_download_reactiveval$a <- "p_values_table"
      } else if (input$results_set_for_download == "complete_set_of_test_statistics") {
        results_set_for_download_reactiveval$a <- "complete_set_of_test_statistics"
      }
    })
  })

  observe({
    if(results_set_for_download_reactiveval$a == "p_values_table") {
      isolate({
        if(pval_correction_mode$a) {
          p_values_answer1 <- two_groups_test_result_r_f$a@out_frame
          output$download_test_results <- downloadHandler(
            filename = function() {
              paste( Sys.Date(), "-data", ".csv", sep="")
            },
            content = function(file) {
              write.csv(p_values_answer1, file)
            }
          )
        } else if(!pval_correction_mode$a) {
          unadjusted_p_values_answer1 <- two_groups_test_result_r_f$a@unadjusted_out_frame
          output$download_test_results <- downloadHandler(
            filename = function() {
              paste( Sys.Date(), "-data", ".csv", sep="")
            },
            content = function(file2) {
              write.csv(unadjusted_p_values_answer1, file2)
            }
          )
        }
      })
      
    } else if (results_set_for_download_reactiveval$a == "complete_set_of_test_statistics") {
      isolate({
        output_str_f <- format_two_groups_full_statistics_for_downloading(two_groups_test_result_r_f$a, selected_test$a, pval_correction_mode$a, data_rtr_sel)
        
        output$download_test_results <- downloadHandler(
          filename = function() {
            paste( Sys.Date(), "-data", ".txt", sep="")
          },
          content = function(file3) {
            write(output_str_f, file3)
          }
        )
      })
    }
  })

}

