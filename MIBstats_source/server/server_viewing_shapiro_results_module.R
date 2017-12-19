# _Module for viewing Shapiro-Wilk test results

viewing_shapiro_test_results_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Shapiro-Wilk test results"),
    uiOutput(ns("shapiro_results_selectinput")),
    fluidRow(
      column(12,
             dataTableOutput(ns("shapiro_results_table"))
      )
    ),
    helpText("Click on a row of the table to view test statistic details for one variable"),
    tags$hr(),
    two_groups_test_data_downloadUI(ns("two_groups_test_data_download_shapiro_test")),
    tableOutput(ns("shapiro_test_one_row_results_table")),
    two_groups_test_data_one_variable_statistics_downloadUI(ns("one_variable_statistics_download_shapiro_test")),
    tags$style(type="text/css", "#text_full_results {white-space: pre-wrap;}"),
    help_box_moduleUI(ns("help_shapiro_test"))
  )
}

viewing_shapiro_test_results_module <- function(input, output, session, rtr_reactive, results_decimal_places_reactive) {
  # _Module for viewing Shapiro-Wilk test results
  ns <- session$ns
  
  callModule(help_box_module, "help_shapiro_test")
  shapiro_two_groups_test_data_download_called <- reactiveValues(a = FALSE)
  shapiro_result_r <- reactiveValues(a = NULL)
  shapiro_result_r_a_reactive <- reactive({shapiro_result_r$a})
  shapiro_selected_test <- reactiveValues(a = NULL)
  shapiro_selected_test_reactive <- reactive({shapiro_selected_test$a})
  shapiro_pval_correction_mode <- reactiveValues(a = TRUE)
  shapiro_pval_correction_mode_reactive <- reactive({shapiro_pval_correction_mode$a})
  
  rtr <- reactiveValues(a = NULL)
  results_decimal_places <- reactiveValues(a = NULL)
  
  shapiro_full_results_a <- reactiveValues(a = NULL)
  shapiro_full_results_b <- reactiveValues(a = NULL)
  shapiro_full_results_pairs <- reactiveValues(a = NULL)
  
  selectinput_choices <- reactiveValues(a = NULL)
  
  
  observe({
    rtr_new <- rtr_reactive()
    if(!is.null(rtr_new)) {
      isolate({
        rtr$a <- rtr_new
        update_shapiro_results_page()
        selectinput_choices$a <- rtr$a$shapiro_tracking@results_pages$l
                
      })
    }
  })
  
  observe({
    results_decimal_places_new <- results_decimal_places_reactive()
    if(!is.null(results_decimal_places_new)) {
      isolate({
        results_decimal_places$a <- results_decimal_places_reactive()
      })
    }
  })
  
  rtr_sel <- 1
  

  update_shapiro_results_page <- function() {
    
    if(is.null(input$shapiro_results_pages_selectinput)) {
      rtr$a$shapiro_tracking@curr_sel_index$a <<- 1
      print("IS NULL")
    } else {
      curr_selection_s <- input$shapiro_results_pages_selectinput
      rtr$a$shapiro_tracking@curr_sel_index$a <<- which(rtr$a$shapiro_tracking@results_pages$l == curr_selection_s)
    }
 
    shapiro_result_r$a <<- rtr$a$shapiro_tracking@results$l[[rtr$a$shapiro_tracking@curr_sel_index$a]]

    # _Proceeds if the t-test function does not report an error
    
    # _Gets the p value and full results from the t-test output
    
    rtr$a$shapiro_tracking@p_values_answer <<- shapiro_result_r$a@out_frame
    shapiro_full_results_a$a <<- shapiro_result_r$a@full_results_list$a
    shapiro_full_results_b$a <<- shapiro_result_r$a@full_results_list$b
    shapiro_full_results_pairs$a <<- shapiro_result_r$a@full_results_list$pairs
    shapiro_paired_boolean <- shapiro_result_r$a@full_results_list$pairs_boolean
    
    
    row_titles_f <- as.character(shapiro_result_r$a@out_frame$variable_index)
    row_titles_f2 <- row_titles_f
    
    row_titles_group1 <- NULL
    row_titles_group2 <- NULL
    row_titles_pairs <- NULL
    for (item_f in row_titles_f) {
      row_titles_group1 <- c(row_titles_group1, paste("group_1:_", item_f, sep=""))
      row_titles_group2 <- c(row_titles_group2, paste("group_2:_", item_f, sep=""))
      row_titles_pairs <- c(row_titles_pairs, paste("pairs:_", item_f, sep=""))
    }
    
    shapiro_combined_results_list = c(shapiro_full_results_a, shapiro_full_results_b)
    row_titles_f2 <- list.append(row_titles_group1, row_titles_group2)
    if(shapiro_paired_boolean) {
      shapiro_combined_results_list = c(shapiro_combined_results_list, shapiro_full_results_pairs)
      row_titles_f2 <- list.append(row_titles_f2, row_titles_pairs)
    }
    shapiro_combined_results_list <- list(shapiro_combined_results_list)
    
    shapiro_combined_results_list <- shapiro_combined_results_list[[1]]
    
    
    if(shapiro_result_r$a@test_performed) {
      shapiro_results_colnames <- c("Variable", "Group 1 p-value", "Group 2 p-value", "Pairs p-value", "Group 1 p-value significance", "Group 2 p-value significance", "Pairs p-value significance")
      js$enableTab("tab_view_shapiro_wilk_test_results")
      
      if(!is.null(rtr$a$shapiro_tracking@p_values_answer) && is.data.frame(rtr$a$shapiro_tracking@p_values_answer)) {
        output$shapiro_results_table <- renderDataTable({
          DT::datatable(rtr$a$shapiro_tracking@p_values_answer,
                        options = list(columnDefs = list(
                          list(targets = {c(3, 6)}, visible = shapiro_paired_boolean))
                          , rowCallback = JS('
                                             function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                             // Bold and green cells for conditions
                                             if (parseFloat(aData[1]) <= 0.05)
                                             $("td:eq(1)", nRow).css("font-weight", "bold");
                                             if (parseFloat(aData[1]) <= 0.05)
                                             $("td:eq(1)", nRow).css("background-color", "#FF0000");
                                             if (parseFloat(aData[2]) <= 0.05)
                                             $("td:eq(2)", nRow).css("font-weight", "bold");
                                             if (parseFloat(aData[2]) <= 0.05)
                                             $("td:eq(2)", nRow).css("background-color", "#FF0000");
                                             if (parseFloat(aData[3]) <= 0.05)
                                             $("td:eq(3)", nRow).css("font-weight", "bold");
                                             if (parseFloat(aData[3]) <= 0.05)
                                             $("td:eq(3)", nRow).css("background-color", "#FF0000");
                                             }')
                        ), colnames = shapiro_results_colnames, rownames = FALSE, selection = 'none')
          
        })
          
      } else {
        show_generic_error_message("The results table is faulty")
      }
      
    }

    shapiro_selected_test$a <<- toString(input$shapiro_results_pages_selectinput)
    
    if(!shapiro_two_groups_test_data_download_called$a) {

      callModule(two_groups_test_data_download, "two_groups_test_data_download_shapiro_test", shapiro_result_r_a_reactive, rtr$a, rtr_sel, shapiro_selected_test_reactive, shapiro_pval_correction_mode_reactive)
      shapiro_two_groups_test_data_download_called$a <<- TRUE
    }
  }
  
  observeEvent(input$shapiro_results_pages_selectinput, {
    isolate({
      update_shapiro_results_page()
    })
  })
  
  
  # _If the table is clicked
  observeEvent(input$shapiro_results_table_cell_clicked, {
    isolate({
      info_s = input$shapiro_results_table_cell_clicked
      
      if (is.null(info_s$value)) return()
      
      shapiro_statistics_frame <- ""
      shapiro_statistics_string_for_downloading <- ""
      if((info_s$col == 1) || (info_s$col == 4)) {
        shapiro_statistics_frame <- format_two_groups_statistics_for_output_as_table(shapiro_result_r$a, shapiro_full_results_a$a[[info_s$row]], results_decimal_places$a)
        shapiro_statistics_string_for_downloading <- format_two_groups_statistics_for_downloading(shapiro_result_r$a, shapiro_full_results_a$a[[info_s$row]], results_decimal_places$a)
      }
      if((info_s$col == 2) || (info_s$col == 5)) {
        shapiro_statistics_frame <- format_two_groups_statistics_for_output_as_table(shapiro_result_r$a, shapiro_full_results_b$a[[info_s$row]], results_decimal_places$a)
        shapiro_statistics_string_for_downloading <- format_two_groups_statistics_for_downloading(shapiro_result_r$a, shapiro_full_results_b$a[[info_s$row]], results_decimal_places$a)
      }
      if((info_s$col == 3) || (info_s$col == 6)) {
        shapiro_statistics_frame <- format_two_groups_statistics_for_output_as_table(shapiro_result_r$a, shapiro_full_results_pairs$a[[info_s$row]], results_decimal_places$a)
        shapiro_statistics_string_for_downloading <- format_two_groups_statistics_for_downloading(shapiro_result_r$a, shapiro_full_results_pairs$a[[info_s$row]], results_decimal_places$a)
      }
      
      output_str_f2 <- paste(shapiro_selected_test$a, "\n\n", shapiro_statistics_string_for_downloading, sep="")
      
      output$shapiro_test_one_row_results_table <- renderTable(shapiro_statistics_frame, rownames = TRUE, selection = "none")
      
      callModule(two_groups_test_data_one_variable_statistics_download, "one_variable_statistics_download_shapiro_test", output_str_f2)
    })
    
    
  })
  
  output$shapiro_results_selectinput = renderUI({
    selectInput(ns("shapiro_results_pages_selectinput"), "Shapiro-Wilk test results", selectinput_choices$a)
  })
  
}

