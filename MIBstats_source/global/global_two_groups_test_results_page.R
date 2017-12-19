# _Module for displaying the test results of the t-test, F-test and Mann-Whitney U test

two_groups_test_results_pageUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("two_groups_test_title")),
    
    uiOutput(ns("two_groups_test_results_selectinput")),
    fluidRow(
      column(12,
             dataTableOutput(ns("two_groups_test_results_table"))
      )
    ),
    helpText("Click on a row of the table to view test statistic details for one variable"),
    tags$hr(),
    radioButtons(ns("p_value_adjustment_radiobutton"), "p-values adjustment",
                 c("Adjusted for multiple testing" = "adjusted",
                   "Unadjusted for multiple testing" = "unadjusted")),
    actionButton(ns("change_p_values_adjustment_mode_button"), "Change p-values adjustment type"),
    bsTooltip(id = ns("change_p_values_adjustment_mode_button"), title = "Click here to change p-values adjustment type", 
              placement = "left", trigger = "hover"),
    tags$hr(),
    two_groups_test_data_downloadUI(ns("two_groups_test_data_download_t_test")),
    
    uiOutput(ns("two_groups_test_results_one_variable_statistics")),
    uiOutput(ns("two_groups_test_help_container"))
  )
}

two_groups_test_results_page <- function(input, output, session, data_rtr, data_rtr_sel, results_decimal_places_f) {
  
  ns <- session$ns
  
  plot_file_format <- reactiveValues(a = "svg")
  
  plot_file_format_reactive <- callModule(image_format_selection_module, "two_groups_test_image_format")
  
  observe({
    plot_file_format_observed <- plot_file_format_reactive()
    isolate({
      if(!is.null(plot_file_format_observed)) {
        plot_file_format$a <- plot_file_format_observed
      }
    })
  })
  
  two_group_test_results_title <- function(data_rtr_sel_f) {
    # _Gets the title of the test based on the results tracker (rtr) index
    two_group_test_results_title_f <- ""
    if(data_rtr_sel_f == 2) {
      two_group_test_results_title_f <- "t-test"
    } else if (data_rtr_sel_f == 3) {
      two_group_test_results_title_f <- "F-test"
    } else if (data_rtr_sel_f == 9) {
      two_group_test_results_title_f <- "Mann-Whitney U test"
    }
    return(two_group_test_results_title_f)
  }
  
  get_help_module_name <- function(data_rtr_sel_f) {
    # _Gets the name of the module for displaying context-specific help, based on the results tracker (rtr) index
    help_module_name_f <- ""
    if(data_rtr_sel_f == 2) {
      help_module_name_f <- "t_test"
    } else if (data_rtr_sel_f == 3) {
      help_module_name_f <- "f_test"
    } else if (data_rtr_sel_f == 9) {
      help_module_name_f <- "mann_whitney_u_test"
    }
    help_module_name_f <- paste("help_", help_module_name_f, sep="")
    return(help_module_name_f)
  }
  
  
  help_module_name <- get_help_module_name(data_rtr_sel)
  output$two_groups_test_help_container <- renderUI({
    help_box_moduleUI(ns(help_module_name))
  })
  
  callModule(help_box_module, help_module_name)
  
  plf <- reactiveValues(l = NULL)
  pl_axis_t <- reactiveValues(x = NULL, y = NULL)
  show_adjusted_p_values <- reactiveValues(a = TRUE) # _Boolean that toggles whether the p-values shown are adjusted for multiple testing or not
  
  two_groups_test_result_r <- reactiveValues(a = NULL)
  two_groups_test_result_r_a_reactive <- reactive({two_groups_test_result_r$a})
  
  two_groups_test_full_results <- reactiveValues(a = NULL)
  pval_adjustment_mode_reactive <- reactive({show_adjusted_p_values$a})
  
  row_titles_f <- reactiveValues(a = NULL)
  
  selected_test <- reactiveValues(a = NULL)
  selected_test_reactive <- reactive({selected_test$a})
  
  two_groups_test_data_download_called <- reactiveValues(a = FALSE)
    
  plotInput = function(plotting_frame_f, x_title_f, y_title_f) {
    ggplot(na.omit(stack(plotting_frame_f)), aes(x = ind, y = values)) +
      geom_boxplot() + labs(y=y_title_f, x = x_title_f)+ theme_bw() 
  }
  
  redraw_boxplot <- function(plotting_frame_f, x_title_f, y_title_f) {
    output$two_groups_test_data_boxplot <- renderPlot({
      plotInput(plotting_frame_f, x_title_f, y_title_f)
    })
  }
  
  get_p_val_table <- function(data_rtr_sel_f, data_rtr_f, p_adjustment_choice_f) {
    # _Produces the datatable with p-values that will be displayed on the screen
    
    p_values_data <- NULL
    
    p_values_answer1 <- two_groups_test_result_r$a@out_frame
    
    unadjusted_p_values_answer1 <- two_groups_test_result_r$a@unadjusted_out_frame
    
    if(p_adjustment_choice_f) {
      p_values_data <- p_values_answer1
    } else if(!p_adjustment_choice_f) {
      p_values_data <- unadjusted_p_values_answer1
    }
    
    
    
    if(data_rtr_sel_f == 2 | data_rtr_sel_f == 9) {
      # _If displaying results for a t-test or Mann-Whitney U test
      p_val_table <- DT::datatable(p_values_data, 
                                   options = list(rowCallback = JS('
                                                    function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                    // Bold and green cells for conditions
                                                    if (parseFloat(aData[1]) <= 0.05)
                                                    $("td:eq(1)", nRow).css("font-weight", "bold");
                                                    if (parseFloat(aData[1]) <= 0.05)
                                                    $("td:eq(1)", nRow).css("background-color", "#9BF59B");
                                                    }')
                                   ), colnames = c("Variable", "p-value", "p-value significance"), rownames = FALSE, selection = 'none'
      )
    } else if (data_rtr_sel_f == 3) {
      # _If displaying results for an F-test
      p_val_table <- DT::datatable(p_values_data, 
                                   options = list(rowCallback = JS('
                                                    function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                    // Bold and green cells for conditions
                                                    if (parseFloat(aData[1]) <= 0.05)
                                                    $("td:eq(1)", nRow).css("font-weight", "bold");
                                                    if (parseFloat(aData[1]) <= 0.05)
                                                    $("td:eq(1)", nRow).css("background-color", "#FF0000");
                                                    }')
                                   ), colnames = c("Variable", "p-value", "p-value significance"), rownames = FALSE, selection = 'none'
      )
    }
    return(p_val_table)
  }
  
  
  update_results_page_selection <- function(curr_selection_f, rtr_results_pages_f) {
    new_curr_sel_index_a <- which(rtr_results_pages_f == curr_selection_f)
    return(new_curr_sel_index_a)
  }
  
  update_two_groups_test_results_page <- function(data_rtr, data_rtr_sel, show_adjusted_p_values_a) {
    # _Updates the results page depending on the choice of test result that is selected for viewing
    
    
    
    if(is.null(data_rtr[[data_rtr_sel]]@curr_sel_index$a)) {
      data_rtr[[data_rtr_sel]]@curr_sel_index$a <- 1
    }
    
    result_index <- data_rtr[[data_rtr_sel]]@curr_sel_index$a
    results_list <- data_rtr[[data_rtr_sel]]@results$l
    
    if(is.null(result_index) || length(result_index) == 0) {
      result_index <- 1
    }
    
    two_groups_test_result_r$a <<- results_list[[result_index]]
    
    # _this belongs to the two groups test result class
    
    
    # _Gets the p value and full results from the two_groups_test output
    data_rtr[[data_rtr_sel]]@p_values_answer <<- two_groups_test_result_r$a@out_frame
    
    data_rtr[[data_rtr_sel]]@unadjusted_p_values_answer <<- two_groups_test_result_r$a@unadjusted_out_frame
    
    
    
    row_titles_f$a <<- two_groups_test_result_r$a@out_frame$variable_index
    
    # _Proceeds if the two_groups_test function does not report an error
    two_groups_test_full_results$a <<- two_groups_test_result_r$a@full_results_list
    
    if(two_groups_test_result_r$a@test_performed) {
      p_val_table <- get_p_val_table(data_rtr_sel, data_rtr, show_adjusted_p_values_a)
      output$two_groups_test_results_table <<- renderDataTable({p_val_table})
      selected_test$a <- toString(input$two_groups_test_results_pages_selectinput)
      
      if(!two_groups_test_data_download_called$a) {
        
        callModule(two_groups_test_data_download, "two_groups_test_data_download_t_test", two_groups_test_result_r_a_reactive, data_rtr, data_rtr_sel, selected_test_reactive, pval_adjustment_mode_reactive)
        two_groups_test_data_download_called$a <- TRUE
      }
    }
  }
  
  update_two_groups_test_results_page(data_rtr, data_rtr_sel, show_adjusted_p_values$a)
  
  observeEvent(input$two_groups_test_results_pages_selectinput, {
    data_rtr[[data_rtr_sel]]@curr_sel_index$a <- update_results_page_selection(input$two_groups_test_results_pages_selectinput, data_rtr[[data_rtr_sel]]@results_pages$l)
    update_two_groups_test_results_page(data_rtr, data_rtr_sel, show_adjusted_p_values$a)
    
  })
  
  observeEvent(input$change_p_values_adjustment_mode_button, {
    isolate({
      old_show_adjusted_p_values <- show_adjusted_p_values$a
      if(input$p_value_adjustment_radiobutton == "adjusted") {
        show_adjusted_p_values$a <- TRUE
      } else if(input$p_value_adjustment_radiobutton == "unadjusted") {
        show_adjusted_p_values$a <- FALSE
      }
      if(old_show_adjusted_p_values != show_adjusted_p_values$a) {
        
        
        new_p_val_table <- get_p_val_table(data_rtr_sel, data_rtr, show_adjusted_p_values$a)
        
        
        output$two_groups_test_results_table <- renderDataTable({new_p_val_table})
      }
    })
  })
  
  # _If the table is clicked
  observeEvent(input$two_groups_test_results_table_cell_clicked, {
    isolate({
      info = input$two_groups_test_results_table_cell_clicked
      
      if (is.null(info$value)) return()
      
      
      
      full_results_for_screen_table <- NULL
      if(show_adjusted_p_values$a) {
        full_results_for_screen_table <- two_groups_test_result_r$a@full_results_list
      } else if (!show_adjusted_p_values$a) {
        full_results_for_screen_table <- two_groups_test_result_r$a@unadjusted_full_results_list
      }
      
      two_groups_test_statistics_frame <- format_two_groups_statistics_for_output_as_table(two_groups_test_result_r$a, full_results_for_screen_table[[info$row]], results_decimal_places_f)
      
      
      
      output$two_groups_test_text_full_results_table <- renderTable(two_groups_test_statistics_frame, rownames = TRUE, selection = "none")
      
      
      plotting_frame <- extract_two_groups_test_data_for_box_plot(two_groups_test_result_r$a, info$row)
      
      plf$l <- plotting_frame
      
      selected_test$a <- toString(input$two_groups_test_results_pages_selectinput)
      output_str_f2 <- paste(selected_test$a, "\n\n", format_two_groups_statistics_for_downloading(two_groups_test_result_r$a, full_results_for_screen_table[[info$row]], 100), sep="")
      
      output$two_groups_test_results_one_variable_statistics = renderUI({
        tags$div(
          tags$h4(paste(row_titles_f$a[info$row], ": test statistics", sep="")) 
        )
        uiOutput(ns("two_groups_test_results_one_variable"))
        fluidRow(
          
          column(10,
                 tags$h4("Box plot of the two groups"), 
                 plotOutput(ns("two_groups_test_data_boxplot")),
                 uiOutput(ns("two_groups_test_x_axis_title_field")),
                 uiOutput(ns("two_groups_test_y_axis_title_field")),
                 uiOutput(ns("two_groups_test_axis_titles_button")),
                 downloadButton(ns("download_one_variable_plot"), "Download the plot"),
                 bsTooltip(id = ns("download_one_variable_plot"), title = "Click here to download the plot as an image", 
                           placement = "left", trigger = "hover"),
                 image_format_selection_moduleUI(ns("two_groups_test_image_format"))
          ),
          
          column(2,
                 tags$h4("List of test statistics"), 
                 tableOutput(ns("two_groups_test_text_full_results_table")),
                 two_groups_test_data_one_variable_statistics_downloadUI(ns("one_variable_statistics_download_t_test")),
                 tags$style(type="text/css", "#text_full_results {white-space: pre-wrap;}")
          )
        )
        
      })
      
      callModule(two_groups_test_data_one_variable_statistics_download, "one_variable_statistics_download_t_test", output_str_f2)
      
      
      
      output$download_one_variable_plot <- downloadHandler(
        filename = function() { paste(input$dataset, ".", plot_file_format$a, sep='') },
        content = function(file) {
          ggsave(file, plotInput(plf$l, pl_axis_t$x, pl_axis_t$y), device = plot_file_format$a)
        }
      )
      
      redraw_boxplot(plf$l, pl_axis_t$x, pl_axis_t$y)
      
      # _Displays the text fields and the button for adjusting plot axis titles
      output$two_groups_test_x_axis_title_field <- renderUI({
        textInput(ns("two_groups_test_plot_x_axis_title"), "x axis title:", "")
      })
      
      output$two_groups_test_y_axis_title_field <- renderUI({
        textInput(ns("two_groups_test_plot_y_axis_title"), "y axis title:", "")
      })
      
      output$two_groups_test_axis_titles_button <- renderUI({
        tagList(
          actionButton(ns("two_groups_test_update_axis_titles_button"), "Update axis titles"),
          bsTooltip(id = ns("two_groups_test_update_axis_titles_button"), title = "Click here to update the axis titles of the box plot", 
                    placement = "left", trigger = "hover")
        )
      })
      
    })

  })
  
  # _If the button to update axis titles was clicked
  observeEvent(input$two_groups_test_update_axis_titles_button, {
    isolate({
      pl_axis_t$x <- input$two_groups_test_plot_x_axis_title
      pl_axis_t$y <- input$two_groups_test_plot_y_axis_title
      if(!is.null(plf$l)) {
        redraw_boxplot(plf$l, pl_axis_t$x, pl_axis_t$y)  
      }
      
    })
  })
  
  test_title <- paste(two_group_test_results_title(data_rtr_sel), " results", sep="")
  
  output$two_groups_test_title = renderUI({
    tags$h3(test_title)
  })
  
  output$two_groups_test_results_selectinput = renderUI({
    selectInput(ns("two_groups_test_results_pages_selectinput"), "Pages of results:", data_rtr[[data_rtr_sel]]@results_pages$l)
  })
  
}