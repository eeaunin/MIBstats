# _Module for displaying the results of fold changes calculation

library(gplots)

two_groups_fold_changes_results_pageUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("two_groups_test_title")),
    
    uiOutput(ns("two_groups_test_results_selectinput")),
    tags$h4("Fold changes of group averages"),
    fluidRow(
      column(12,
             dataTableOutput(ns("two_groups_test_results_table"))
      )
    ),
    uiOutput(ns("test_results_download_container")),
    tags$h4("Settings"),
    actionButton(ns("two_groups_fold_changes_direction_chooser_button"), "Choose the direction of fold change calculation...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("two_groups_fold_changes_direction_chooser_button"), title = "Click here to choose the direction of fold change calculation", placement = "left", trigger = "hover"),
    uiOutput(ns("two_groups_fold_changes_direction_chooser_container")),
    actionButton(ns("two_groups_fold_changes_show_scale_chooser_button"), "Change the scale of fold changes...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("two_groups_fold_changes_show_scale_chooser_button"), title = "Click here to change the scale of fold changes", placement = "left", trigger = "hover"),
    uiOutput(ns("two_groups_fold_changes_scale_chooser_container")),
    actionButton(ns("two_groups_fold_changes_show_heat_map_button"), "Show heat map", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("two_groups_fold_changes_show_heat_map_button"), title = "Click here to view the heat map of fold changes of paired samples", placement = "left", trigger = "hover"),
    uiOutput(ns("two_groups_fold_changes_heat_map_container")),
    tags$br(),
    help_box_moduleUI(ns("help_fold_changes"))
    
  )
}

two_groups_fold_changes_results_page <- function(input, output, session, data_rtr, data_rtr_sel, results_decimal_places_f){

  ns <- session$ns
  
  callModule(help_box_module, "help_fold_changes")
  
  plot_file_format <- reactiveValues(a = "svg")
  
  plot_file_format_reactive <- callModule(image_format_selection_module, "fold_changes_image_format")
  
  observe({
    plot_file_format_observed <- plot_file_format_reactive()
    isolate({
      if(!is.null(plot_file_format_observed)) {
        plot_file_format$a <- plot_file_format_observed
      }
    })
  })
  
  two_group_test_results_title <- function(data_rtr_sel_f) {
    two_group_test_results_title_f <- ""
    if(data_rtr_sel_f == 4) {
      two_group_test_results_title_f <- "Fold changes"
    }
    
    return(two_group_test_results_title_f)
  }
  
  two_groups_test_result_r <- reactiveValues(a = NULL)
  two_groups_test_result_r_a_reactive <- reactive({two_groups_test_result_r$a})
  
  two_groups_test_full_results <- reactiveValues(a = NULL)
  
  row_titles_f <- reactiveValues(a = NULL)
  
  selected_test_index <- reactiveValues(a = NULL)
  selected_test <- reactiveValues(a = NULL)
  selected_test_reactive <- reactive({selected_test$a})
  
  two_groups_test_data_download_called <- reactiveValues(a = FALSE)
  
  heat_map_visible <- reactiveValues(a = FALSE)
  scale_chooser_visible <- reactiveValues(a = FALSE)
  direction_chooser_visible <- reactiveValues(a = FALSE)
  paired_option <- reactiveValues(a = FALSE)
  fc_matrix <- reactiveValues(a = NULL)
  heatmap_r <- reactiveValues(a = NULL)
  scale_selection <- reactiveValues(a = "linear")
  direction_selection <- reactiveValues(a = "group1_vs_group2")
  
  table_hidden_columns <- reactiveValues(a = c(2))

  get_fc_matrix_data <- function(two_groups_test_full_results_a, group1_vs_group2_bool) {
    # _Extracts the data for matrix of fold changes for paired samples from the module's input.
    # _group1_vs_group2_bool toggles if the fold changes are group1/group2 or group2/group1. (group1/group2 if TRUE, group2/group1 if FALSE)
    fc_matrix_data <- NULL
    for(item_f in two_groups_test_full_results_a) {
      fc_row <- NULL
      if(group1_vs_group2_bool) {
        fc_row <- item_f$group1_vs_group2_paired_f
      } else {
        fc_row <- item_f$group2_vs_group1_paired_f
      }
      fc_matrix_data <- rbind(fc_matrix_data, fc_row)
    }
    return(fc_matrix_data)
  }
  
  get_colnames_for_heatmap <- function(group1_input_data, group2_input_data, group1_vs_group2_bool) {
    # _Extracts data for column names of the paired samples fold changes heat map from the input data of the fold changes calculation. 
    # _group1_vs_group2_bool toggles if the fold changes are group1/group2 or group2/group1. (group1/group2 if TRUE, group2/group1 if FALSE)
    group1_names <- NULL
    group2_names <- NULL
    if(group1_vs_group2_bool) {
      group1_names <- names(group1_input_data[[1]])
      group2_names <- names(group2_input_data[[1]])
    } else {
      group2_names <- names(group1_input_data[[1]])
      group1_names <- names(group2_input_data[[1]])
    }
    counter_f <- 1
    concat_name <- NULL
    for(item_f in group1_names) {
      current_name <- paste(item_f, "_vs_", group2_names[counter_f], sep="")
      concat_name <- c(concat_name, current_name )
      counter_f <- counter_f + 1
    }
    return(concat_name)
  }
  
  get_fc_matrix <- function(two_groups_test_full_results_a, data_row_a_f, data_row_b_f, direction_selection_f, scale_selection_f) {
    # _Extracts the matrix of fold changes for paired samples from the module's input.
    # _group1_vs_group2_bool toggles if the fold changes are group1/group2 or group2/group1. (group1/group2 if TRUE, group2/group1 if FALSE)
    group1_vs_group2_bool <- NULL
    if(direction_selection_f == "group1_vs_group2") {
      group1_vs_group2_bool <- TRUE
    } else if (direction_selection_f == "group2_vs_group1") {
      group1_vs_group2_bool <- FALSE
    }
    fc_matrix_f <- get_fc_matrix_data(two_groups_test_full_results_a, group1_vs_group2_bool)
    matrix_col_names <- get_colnames_for_heatmap(data_row_a_f, data_row_b_f, group1_vs_group2_bool)
    colnames(fc_matrix_f) <- matrix_col_names
    rownames(fc_matrix_f) <- row_titles_f$a
    if(scale_selection_f == "log2") {
      fc_matrix_f <- log2(fc_matrix_f)
    } else if (scale_selection_f == "log10") {
      fc_matrix_f <- log10(fc_matrix_f)
    }
    fc_matrix_f[is.infinite(fc_matrix_f)] <- 0
    fc_matrix_f[is.nan(fc_matrix_f)] <- 0
    fc_matrix_f[is.na(fc_matrix_f)] <- 0
    return(fc_matrix_f)
  }
  
  observeEvent(input$two_groups_fold_changes_show_scale_chooser_button, {
    isolate({
      scale_chooser_visible$a <- !scale_chooser_visible$a
      if(scale_chooser_visible$a) {
        updateActionButton(session, "two_groups_fold_changes_show_scale_chooser_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "two_groups_fold_changes_show_scale_chooser_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$two_groups_fold_changes_show_heat_map_button, {
    isolate({
      heat_map_visible$a <- !heat_map_visible$a
      if(heat_map_visible$a) {
        updateActionButton(session, "two_groups_fold_changes_show_heat_map_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "two_groups_fold_changes_show_heat_map_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$two_groups_fold_changes_direction_chooser_button, {
    isolate({
      direction_chooser_visible$a <- !direction_chooser_visible$a
      if(direction_chooser_visible$a) {
        updateActionButton(session, "two_groups_fold_changes_direction_chooser_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "two_groups_fold_changes_direction_chooser_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  log_adjust_fc_data_table <- function(fc_data_f, scale_selection_f) {
    # _Adjusts the scale of the fold change numbers in the fold changes table to log2 or log10 if needed
    
    
    if(scale_selection_f == "log2") {
      fc_data_f[, 2:3] <- log(fc_data_f[2:3], 2)
    } else if (scale_selection_f == "log10") {
      fc_data_f[, 2:3] <- log(fc_data_f[2:3], 10)
    }
    
    return(fc_data_f)
  }

  get_fc_table <- function(data_rtr_sel_f, data_rtr_f, scale_selection_f) {
    # _Produces the datatable with fold changes that will be displayed on the screen
    
    fc_data <- NULL
 
    fc_data <- two_groups_test_result_r$a@out_frame
    fc_data <- log_adjust_fc_data_table(fc_data, scale_selection_f)

    
    if(data_rtr_sel_f == 4) {
      # _If displaying results for a t-test
      
      fc_table <- DT::datatable(fc_data, 
                                options = list(columnDefs = list(list(visible=FALSE, targets=table_hidden_columns$a)), rowCallback = JS('
                                                  function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                  // Bold and green cells for conditions
                                                  //if (parseFloat(aData[1]) > 0)
                                                  //$("td:eq(1)", nRow).css("font-weight", "bold");
                                                  if (parseFloat(aData[1]) > 0)
                                                  $("td:eq(1)", nRow).css("background-color", "#9BF59B");
                                                  if (parseFloat(aData[1]) < 0)
                                                  $("td:eq(1)", nRow).css("background-color", "#4682b4");
                                                  if (parseFloat(aData[2]) > 0)
                                                  $("td:eq(2)", nRow).css("background-color", "#9BF59B");
                                                  if (parseFloat(aData[2]) < 0)
                                                  $("td:eq(2)", nRow).css("background-color", "#4682b4");
                                                  }')
                                ), colnames = c("Variable", "group1/group2 FC", "group2/group1 FC"), rownames = FALSE, selection = 'none'
      )
    }
    
    return(fc_table)
  }
  
  update_table_hidden_columns <- function() {
    # _Toggles which direction of fold changes is displayed in the table
    if(direction_selection$a == "group1_vs_group2") {
      table_hidden_columns$a <- c(2)
    } else if (direction_selection$a == "group2_vs_group1") {
      table_hidden_columns$a <- c(1)
    }
  }
  
  update_two_groups_test_results_page <- function(data_rtr, data_rtr_sel) {
    # _updates the page based on which test result is selected for viewing

    update_table_hidden_columns()
    
    update_results_page_selection <- function(curr_selection_f, rtr_results_pages_f) {
      new_curr_sel_index_a <- which(rtr_results_pages_f == curr_selection_f)
      if(is.null(new_curr_sel_index_a) | length(new_curr_sel_index_a) == 0) {
        new_curr_sel_index_a <- 1
      }
      return(new_curr_sel_index_a)
    }
    
    if(is.null(selected_test_index$a)) {
      selected_test_index$a <- 1
    }
    
    value_from_selectinput <- NULL
    if(is.null(input$two_groups_test_results_pages_selectinput)) {
      value_from_selectinput <- data_rtr[[data_rtr_sel]]@results_pages$l[[1]]
    } else {
      value_from_selectinput <- input$two_groups_test_results_pages_selectinput
    }
    
    selected_test_index$a <<- update_results_page_selection(value_from_selectinput, data_rtr[[data_rtr_sel]]@results_pages$l)
    
    two_groups_test_result_r$a <<- data_rtr[[data_rtr_sel]]@results$l[[selected_test_index$a]]
    data_rtr[[data_rtr_sel]]@fc_out_frame <<- two_groups_test_result_r$a@out_frame

 
    row_titles_f$a <<- two_groups_test_result_r$a@out_frame$variable_index
    
    # _Proceeds if the two_groups_test function does not report an error
    two_groups_test_full_results$a <<- two_groups_test_result_r$a@full_results_list
    
    if(two_groups_test_result_r$a@test_performed) {
      fc_table <- get_fc_table(data_rtr_sel, data_rtr, scale_selection$a)
      output$two_groups_test_results_table <<- renderDataTable({fc_table})
      selected_test$a <- toString(input$two_groups_test_results_pages_selectinput)
      
    }
    
    paired_option$a <<- two_groups_test_result_r$a@paired_option
    
    if(paired_option$a) {
      shinyjs::enable("two_groups_fold_changes_show_heat_map_button")
    } else {
      output$two_groups_fold_changes_heat_map <<- renderPlot({})
      output$two_groups_fold_changes_heat_map_container <<- renderUI({})
      heat_map_visible$a <<- FALSE
      shinyjs::disable("two_groups_fold_changes_show_heat_map_button")
      updateActionButton(session, "two_groups_fold_changes_show_heat_map_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    }
    
    if(paired_option$a) {
      fc_matrix$a <<- get_fc_matrix(two_groups_test_full_results$a, two_groups_test_result_r$a@data_row_a, two_groups_test_result_r$a@data_row_b, direction_selection$a, scale_selection$a)
    } else {
      fc_matrix$a <<- NULL
    }
    
  }
  
  update_two_groups_test_results_page(data_rtr, data_rtr_sel)
  
  observeEvent(input$two_groups_test_results_pages_selectinput, {
    isolate({
      update_two_groups_test_results_page(data_rtr, data_rtr_sel)
    })
  })

  # _If the table is clicked
  observeEvent(input$two_groups_test_results_table_cell_clicked, {
    isolate({
      info = input$two_groups_test_results_table_cell_clicked
      
      if (is.null(info$value)) return()
    })
  })
    
  test_title <- paste(two_group_test_results_title(data_rtr_sel), " results", sep="")

  output$two_groups_test_title = renderUI({
    tags$h3(test_title)
  })
  
  output$two_groups_test_results_selectinput = renderUI({
    selectInput(ns("two_groups_test_results_pages_selectinput"), "Pages of results:", data_rtr[[data_rtr_sel]]@results_pages$l)
  })
  
  output$test_results_download_container <- renderUI({
    tagList(
      downloadButton(ns("download_test_results"), "Download the table of fold changes of group averages"),
      bsTooltip(id = ns("download_test_results"), title = "Click here to download the list of test results", 
                placement = "left", trigger = "hover")
    )
  })
  
  
  get_fc_table_data_for_downloading <- function(two_groups_test_result_r_a, scale_selection_f, direction_selection_f) {
    # _Gets the data table of fold changes of group averages for and formats it for downloading
    fc_data <- two_groups_test_result_r_a@out_frame
    colnames(fc_data) <- c("Variable", "Group1_vs_Group2_FC", "Group2_vs_Group1_FC")
    fc_data <- log_adjust_fc_data_table(fc_data, scale_selection_f)
    if(direction_selection_f == "group1_vs_group2") {
      fc_data <- fc_data[, -2]
    } else if (direction_selection_f == "group2_vs_group1") {
      fc_data <- fc_data[, -1]
    }
    
    return(fc_data)
  }
  
  
  table_data_for_downloading <- reactiveValues(a = NULL)
  table_data_for_downloading$a <- get_fc_table_data_for_downloading(two_groups_test_result_r$a, scale_selection$a, direction_selection$a)
  
  output$download_test_results <- downloadHandler(
    filename = function() {
      paste( Sys.Date(), "-data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_data_for_downloading$a, file)
    }
  )
  
  output$download_paired_test_results <- downloadHandler(
    filename = function() {
      paste( Sys.Date(), "-data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(fc_matrix$a, file)
    }
  )
  
  observe({
    if(plot_file_format$a == "svg") {
      isolate({
        output$download_heat_map <- downloadHandler(
          filename = function() { paste(Sys.Date(), "-fold_changes_heat_map", ".", plot_file_format$a, sep="") },
          content <- function(file) {
            svg(file, width = 20, height = 20)
            heatmap_plot <- generate_paired_samples_fold_changes_heat_map()
            heatmap_plot()
            dev.off()},
          contentType = 'image/png'
        )
      })
    } else if (plot_file_format$a == "png") {
      isolate({
        output$download_heat_map <- downloadHandler(
          filename = function() { paste(Sys.Date(), "-fold_changes_heat_map", ".", plot_file_format$a, sep="") },
          content <- function(file) {
            png(file, width = 1500, height = 1500, units = "px", pointsize = 12,
                bg = "white", res = NA)
            heatmap_plot <- generate_paired_samples_fold_changes_heat_map()
            heatmap_plot()
            dev.off()},
          contentType = 'image/png'
        )
      })
    }
  })
  
  
  
  observe({
    if(scale_chooser_visible$a) {
      isolate({
        output$two_groups_fold_changes_scale_chooser_container = renderUI({
          wellPanel(
            radioButtons(ns("fc_scale_chooser_radiobutton"), "Scale of fold changes:",
                         c("Linear" = "linear",
                           "log2" = "log2",
                           "log10" = "log10")),
            actionButton(ns("fc_change_scale_button"), "Change the scale"),
            bsTooltip(id = ns("fc_change_scale_button"), title = "Click here to change the scale of fold changes", placement = "left", trigger = "hover")
          )
        })
      })
      
    } else {
      isolate({
        output$two_groups_fold_changes_scale_chooser_container = renderUI({})
      })
    }
  })
  
  observe({
    if(direction_chooser_visible$a) {
      isolate({
        output$two_groups_fold_changes_direction_chooser_container = renderUI({
          wellPanel(
            radioButtons(ns("fold_changes_direction_of_comparison_radiobutton"), "Direction of comparison:",
                         c("Group 1 vs Group 2" = "group1_vs_group2",
                           "Group 2 vs Group 1" = "group2_vs_group1")),
            actionButton(ns("fc_change_direction_of_comparison_button"), "Change the direction of comparison"),
            bsTooltip(id = ns("fc_change_direction_of_comparison_button"), title = "Click here to change the direction of comparison of fold changes", placement = "left", trigger = "hover")
          )
        })
      })
      
    } else {
      isolate({
        output$two_groups_fold_changes_direction_chooser_container = renderUI({})
      })
    }
  })
  
  observeEvent(input$fc_change_direction_of_comparison_button, {
    isolate({
      old_direction <- direction_selection$a
      if(old_direction != input$fold_changes_direction_of_comparison_radiobutton) {
        direction_selection$a <- input$fold_changes_direction_of_comparison_radiobutton
        update_two_groups_test_results_page(data_rtr, data_rtr_sel)
      }
    })
  })
  
  observeEvent(input$fc_change_scale_button, {
    isolate({
      old_scale_selection <- scale_selection$a
      if(old_scale_selection != input$fc_scale_chooser_radiobutton) {
        scale_selection$a <- input$fc_scale_chooser_radiobutton
        
        neg_values <- sum(two_groups_test_result_r$a@out_frame[[2]] < 0) + sum(two_groups_test_result_r$a@out_frame[[3]] < 0, na.rm = TRUE)
        if(!is.na(neg_values)) {
          if((scale_selection$a == "log2" | scale_selection$a == "log10") & neg_values > 0) {
            message_out(paste("Warning: ", neg_values, " negative values were found in the fold changes data. The values were replaced with NA when log transforming the numbers", sep=""), "Fold changes")
          }
        }
        
        update_two_groups_test_results_page(data_rtr, data_rtr_sel)
      }
    })
    
  })
  
  generate_paired_samples_fold_changes_heat_map <- function() {
    # _Function for generating the heat map of fold changes of paired samples
    new_heat_map <- reactive({heatmap.2( fc_matrix$a,
                         col = colorpanel(100,"blue","yellow","green"),
                         keysize = 0.5,
                         margins = c(22, 22),
                         trace = "none",
                         lhei = c(2, 8),
                         scale = c("none"),
                         symbreaks = min(fc_matrix$a, na.rm=TRUE),
                         na.rm = TRUE,
                         cexRow = 1, cexCol = 1.5,
                         dendrogram = "both")})
    return(new_heat_map)
  }

  observe({
    if(paired_option$a) {
      if(heat_map_visible$a) {
        if(is.matrix(fc_matrix$a) & is.numeric(fc_matrix$a)) {
          isolate({
            heatmap_r$a <- generate_paired_samples_fold_changes_heat_map()
            output$two_groups_fold_changes_heat_map_container <- renderUI({
              tagList(
                tags$h4("Heat map: fold changes of paired samples"),
                downloadButton(ns("download_paired_test_results"), "Download the table of fold changes of paired samples"),
                bsTooltip(id = ns("download_paired_test_results"), title = "Click here to download the table of fold changes of paired samples", 
                          placement = "left", trigger = "hover"),
                downloadButton(ns("download_heat_map"), "Download the heat map"),
                bsTooltip(id = ns("download_heat_map"), title = "Click here to download the the heat map of fold changes of paired samples", 
                          placement = "left", trigger = "hover"),
                image_format_selection_moduleUI(ns("fold_changes_image_format")),
                plotOutput(ns("two_groups_fold_changes_heat_map"), inline = TRUE),
                tags$br()
              )
            })
          })
          
        } else {
          isolate({
            output$two_groups_fold_changes_heat_map_container <- renderUI({})
          })
        }
        
      } else {
        isolate({
          output$two_groups_fold_changes_heat_map_container <- renderUI({})
        })
      }
    }
  })
  
  observe({
    if(paired_option$a & heat_map_visible$a) {
      isolate({
        output$two_groups_fold_changes_heat_map <- renderPlot({
          heatmap_r$a()
        }, width = 1500, height = 1500)
      })
    }
  })
  
}