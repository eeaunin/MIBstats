# _This is the server part that deals with viewing Rank Product results

library(ggplot2)

rank_product_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Table of p-values"),
    fluidRow(
      column(12,
             dataTableOutput(ns("two_groups_test_results_table"))
      )
    ),
    downloadButton(ns("download_test_results"), "Download the p-values table"),
    bsTooltip(id = ns("download_test_results"), title = "Click here to download the list of test results", 
              placement = "left", trigger = "hover"),
    actionButton(ns("p_value_adjustment_panel_button"), "p-value adjustment...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("p_value_adjustment_panel_button"), title = "Click here to choose the type of p-value adjustment", placement = "left", trigger = "hover"),
    uiOutput(ns("p_value_adjustment_container")),
    tags$hr(),
    actionButton(ns("pfp_plots_panel_button"), "Show PFP plots", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("pfp_plots_panel_button"), title = "Click here to view PFP plots", placement = "left", trigger = "hover"),
    uiOutput(ns("pfp_plots_container")),
    uiOutput(ns("rank_product_help_container"))
  )
}

rank_product_module <- function(input, output, session, data_rtr, data_rtr_sel, results_decimal_places_a, selectinput_result_f, calculateP_option) {
  # _Module for rank product

  ns <- session$ns
  
  plot_file_format <- reactiveValues(a = "svg")
  
  plot_file_format_reactive <- callModule(image_format_selection_module, "rank_product_image_format")
  
  observe({
    plot_file_format_observed <- plot_file_format_reactive()
    isolate({
      if(!is.null(plot_file_format_observed)) {
        plot_file_format$a <- plot_file_format_observed
      }
    })
  })
  
  help_module_name <- NULL
  if(calculateP_option) {
    help_module_name <- "help_rank_product"
  } else {
    help_module_name <- "help_rank_sum"
  }
  
  output$rank_product_help_container <- renderUI({
    help_box_moduleUI(ns(help_module_name))
  })
  
  callModule(help_box_module, help_module_name)
  
  padjust_panel_visible <- reactiveValues(a = FALSE)
  pfp_plots_visible <- reactiveValues(a = FALSE)
  padjust_choice <- reactiveValues(a = TRUE)
  current_result <- reactiveValues(a = NULL)
  data_for_downloading <- reactiveValues(a = NULL, plot1 = NULL, plot2 = NULL)
  
  update_toggle <- function(state_f, button_name_f) {
    # _Updates the buttons with plus and minus icons
    if(state_f) {
      updateActionButton(session, button_name_f, icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
    } else {
      updateActionButton(session, button_name_f, icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    }
  }
  
  observeEvent(input$p_value_adjustment_panel_button, {
    isolate({
      padjust_panel_visible$a <- !padjust_panel_visible$a
      update_toggle(padjust_panel_visible$a, "p_value_adjustment_panel_button")
    })
  })
  
  observeEvent(input$pfp_plots_panel_button, {
    isolate({
      pfp_plots_visible$a <- !pfp_plots_visible$a
      update_toggle(pfp_plots_visible$a, "pfp_plots_panel_button")
    })
  })
  
  observe({
    if(padjust_panel_visible$a) {
      isolate({
        output$p_value_adjustment_container <- renderUI ({
          wellPanel(
            htmlOutput(ns("currently_selected_padjust_type")),
            radioButtons(ns("multiple_testing_correction_of_p_values"), "Choose the type of the displayed p-values:",
                         c("Adjusted for multiple testing" = "adjusted", "Unadjusted" = "unadjusted")),
            actionButton(ns("change_p_value_adjustment_button"), "Apply"),
            bsTooltip(id = ns("change_p_value_adjustment_button"), title = "Click here to change the setting of p-value adjustment", placement = "left", trigger = "hover")
          )
          
        })
        update_padjust_textbox(padjust_choice$a)
      })
    } else {
      output$p_value_adjustment_container <- renderUI ({})
    }
  })
  
  update_padjust_textbox <- function(padjust_choice_f) {
    # _Updates the text box that says if p-values are currently adjusted for multiple testing or not
    padjust_text <- NULL
    if(padjust_choice_f) {
      padjust_text <- "adjusted p-values"
    } else {
      padjust_text <- "unadjusted p-values"
    }
    output$currently_selected_padjust_type <- renderText({
    })
  }
  
  observeEvent(input$change_p_value_adjustment_button, {
    isolate({
      old_padjust_choice <- padjust_choice$a
      if(input$multiple_testing_correction_of_p_values == "adjusted") {
        padjust_choice$a <- TRUE
      } else if (input$multiple_testing_correction_of_p_values == "unadjusted") {
        padjust_choice$a <- FALSE
      }
      if(padjust_choice$a != old_padjust_choice) {
        update_padjust_textbox(padjust_choice$a)
        update_pval_table(current_result$a, padjust_choice$a)
      }
    })
  })
  
  observe({
    current_result$a <- selectinput_result_f()
     isolate({
       if(!is.null(current_result$a)) {
         update_pval_table(current_result$a, padjust_choice$a)
       }
     })
  })
  
  
  update_pval_table <- function(current_result_f, p_adjustment_choice_f) {
    # _Function to update the datatable with p-values that is displayed on the screen
    p_values_data <- NULL 
    
    if(p_adjustment_choice_f) {
      p_values_data <- current_result_f@out_frame
    } else if(!p_adjustment_choice_f) {
      p_values_data <- current_result_f@unadjusted_out_frame
    }
    
    p_val_table <- DT::datatable(p_values_data, 
                                 options = list(rowCallback = JS('
                                                    function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                    // Bold and green cells for conditions
                                                    if (parseFloat(aData[1]) <= 0.05)
                                                    $("td:eq(1)", nRow).css("font-weight", "bold");
                                                    if (parseFloat(aData[1]) <= 0.05)
                                                    $("td:eq(1)", nRow).css("background-color", "#9BF59B");
                                                    if (parseFloat(aData[2]) <= 0.05)
                                                    $("td:eq(2)", nRow).css("font-weight", "bold");
                                                    if (parseFloat(aData[2]) <= 0.05)
                                                    $("td:eq(2)", nRow).css("background-color", "#9BF59B");
                                                    if (parseFloat(aData[3]) <= 0.05)
                                                    $("td:eq(3)", nRow).css("font-weight", "bold");
                                                    if (parseFloat(aData[3]) <= 0.05)
                                                    $("td:eq(3)", nRow).css("background-color", "#9BF59B");
                                                    }')
                                 ), colnames = c("Minimum", "Group 1 vs Group 2", "Group 2 vs group 1"), rownames = FALSE, selection = 'none'
    )
    output$two_groups_test_results_table <- renderDataTable({p_val_table})
  }

  render_ggplot2_pfp_plot <- function(plot_nr_f, title_f, RP.out, calculateP_option) {
    # _Function that renders the ggplot2 scatter plots of PFP data
    
    ind <- NULL
    if(calculateP_option) {
      ind <- order(RP.out$RPs[,plot_nr_f], decreasing = F)
    } else {
      ind <- order(RP.out$RSs[,plot_nr_f], decreasing = F)
    }
    
    
    pfp_frame <- data.frame(1:nrow(RP.out$pfp), RP.out$pfp[ind,plot_nr_f])
    
    colnames(pfp_frame) <- c("number_of_genes", "pfp_value")
    new_plot <- ggplot(pfp_frame, aes(x=number_of_genes, y=pfp_value, group=1)) + geom_line() + geom_point() +
      xlab("Number of identified genes") + ylab("Estimated PFP") + 
      ggtitle(title_f)
    if(plot_nr_f == 1) {
      data_for_downloading$plot1 <- new_plot
      output$plot1_ggplot2 <- renderPlot({
        new_plot
      })
    } else if (plot_nr_f == 2) {
      data_for_downloading$plot2 <- new_plot
      output$plot2_ggplot2 <- renderPlot({
        new_plot
      })
    }
  }
  
  observe({
    if(!is.null(current_result$a) & padjust_choice$a) {
      isolate({
        
        rp_full <- current_result$a@full_results_list[[6]]
        
          
        render_ggplot2_pfp_plot(1, "PFP1", rp_full, calculateP_option)
        render_ggplot2_pfp_plot(2, "PFP2", rp_full, calculateP_option)
        
        data_for_downloading$a <- current_result$a@out_frame
        
      })
    } else if (!is.null(current_result$a) & !padjust_choice$a) {
      isolate({
        
        rp_full <- current_result$a@unadjusted_full_results_list[[6]]
        
        render_ggplot2_pfp_plot(1, "PFP1", rp_full, calculateP_option)
        render_ggplot2_pfp_plot(2, "PFP2", rp_full, calculateP_option)
        
        
        data_for_downloading$a <- current_result$a@unadjusted_out_frame
      })
    }
  })
  
  observe({
    if(pfp_plots_visible$a) {
      isolate({
        output$pfp_plots_container <- renderUI ({
          wellPanel(
            tags$h4("Percentage of False Prediction (PFP) plots"),
            plotOutput(ns("plot1_ggplot2")),
            downloadButton(ns("download_pfp1_plot"), "Download the plot"),
            bsTooltip(id = ns("download_pfp1_plot"), title = "Click here to the plot", 
                      placement = "left", trigger = "hover"),
            tags$br(),
            tags$br(),
            plotOutput(ns("plot2_ggplot2")),
            downloadButton(ns("download_pfp2_plot"), "Download the plot"),
            bsTooltip(id = ns("download_pfp2_plot"), title = "Click here to the plot", 
                      placement = "left", trigger = "hover"),
            tags$hr(),
            image_format_selection_moduleUI(ns("rank_product_image_format"))
            
          )
          
        })
      })
    } else {
      output$pfp_plots_container <- renderUI ({})
    }
  })
  
  output$download_test_results <- downloadHandler(
    filename = function() {
      paste( Sys.Date(), "-data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_for_downloading$a, file)
    }
  )
  
  output$download_pfp1_plot <- downloadHandler(
    filename = function() { paste( Sys.Date(), "-data", ".", plot_file_format$a, sep="") },
    content = function(file) {
      ggsave(file, data_for_downloading$plot1, device = plot_file_format$a)
    }
  )
  
  output$download_pfp2_plot <- downloadHandler(
    filename = function() { paste( Sys.Date(), "-data", ".", plot_file_format$a, sep="") },
    content = function(file) {
      ggsave(file, data_for_downloading$plot2, device = plot_file_format$a)
    }
  )

}