# _The server part of volcano plot
library(plotly)

volcano_plot_moduleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$h3("Volcano plot"),
    actionButton(ns("get_volcano_plot_inputs_button"), "Get data for volcano plot"),
    bsTooltip(id = ns("get_volcano_plot_inputs_button"), title = "Click here to retrieve p-values and fold changes data from the current session of this app", placement = "left", trigger = "hover"),
    uiOutput(ns("test_choice_selectinputs_container1")),
    uiOutput(ns("test_choice_selectinputs_container2")),
    uiOutput(ns("test_choice_button_container")),
    uiOutput(ns("volcano_plot_settings_container")),
    uiOutput(ns("volcano_plot_container")),
    uiOutput(ns("volcano_plot_selectizeinput_container")),
    help_box_moduleUI(ns("help_volcano_plot"))
  )
}

volcano_plot_module <- function(input, output, session, rtr_reactive_f) {
  
  ns <- session$ns
  
  callModule(help_box_module, "help_volcano_plot")
  current_session_name <- session$ns("name")
  current_session_name <- as.character(strsplit(current_session_name, "-name"))
  rtr_f <- reactiveValues(a = NULL)
  test_settings <- reactiveValues(l = list())
  test_names_tree <- reactiveValues(a = NULL)
  test_names_tree_selection <- reactiveValues(a = 1)
  test_names_tree_selection_reactive <- reactive({test_names_tree_selection$a})
  selected_test <- reactiveValues(fc_name = NULL, fc_index = NULL, pval_name = NULL, pval_index = NULL)
  matching_tests <- reactiveValues(a = NULL)
  rtr_copy <- reactiveValues(a = NULL)
  results_set <- reactiveValues(fc = NULL, pval = NULL)
  plot_data_frame <- reactiveValues(a = NULL)
  variables_for_arrows <- reactiveValues(l = NULL)
  volcano_plot_selection <- reactiveValues(a = NULL)
  
  plot_settings <- reactiveValues(pval_threshold = 0.05, log2_fc_threshold = 2, fold_change_direction = "group1_vs_group2", p_adjust = TRUE)
  
  
  
  settings_panel_visible <- reactiveValues(a = FALSE)
  settings_panel_visible_reactive <- reactive({settings_panel_visible$a})
  
  ignored_indices_setting <- c(5, 6, 7, 8)
  
  observe({
    rtr_f$a <- rtr_reactive_f()
  })
  
  observeEvent(input$choose_plot_selection_for_marking_with_arrows, {
    isolate({
      updateSelectizeInput(session, "volcano_plot_selectizeinput", choices = plot_data_frame$a$variables, selected = volcano_plot_selection$a)
    })
  })
  
  find_matching_pval_tests_for_fc_tests <- function(test_settings_l, ignored_indices_setting) {
    # _From the list of statistical tests that have been performed, finds p-value yielding tests (e.g. t-tests) whose test parameters match those that were used in fold changes calculations.
    # _This is used for volcano plot
    matches_list <- list()
    fc_counter <- 1
    for(fc_item in test_settings_l$fc_tests) {
      pval_counter <- 1
      detected_matches <- NULL
      for(pval_item in test_settings_l$pval_tests) {
        
        if(rtr_check_if_two_lists_have_identical_items(fc_item$test_queue_item_details, pval_item$test_queue_item_details, ignored_indices_setting)) {
          detected_matches <- c(detected_matches, pval_counter)
        }
        pval_counter <- pval_counter + 1
      }
      matches_list <- append(matches_list, list(detected_matches))
      fc_counter <- fc_counter + 1
    }
    return(matches_list)
  }
  
  get_the_names_of_pval_tests_matching_an_fc_test <- function(test_settings_l, fc_test_index, matches_list_f) {
    # _Retrieves the names of p-value yielding tests have been performed that match a specific fold changes test that has been performed
    pval_test_indices <- matches_list_f[[fc_test_index]]
    pval_test_names <- NULL
    for(index_f in pval_test_indices) {
      pval_test_names <- c(pval_test_names, rtr_test_parameters_to_string(test_settings_l$pval_tests[[index_f]]))
    }
    return(pval_test_names)
  }
  
  get_the_fc_test_and_pval_test_selector_tree <- function(test_settings_l, matches_list_f) {
    # _Gets the data for the fold changes and p-values test selection selectinput, formats it as text strings contained in a nested list
    test_names_tree_f <- list()
    for(fc_item_index in 1:length(matches_list_f)) {
      fc_settings_item <- test_settings_l$fc_tests[[fc_item_index]]
      fc_test_name <- rtr_test_parameters_to_string(fc_settings_item)
      pval_test_names <- get_the_names_of_pval_tests_matching_an_fc_test(test_settings_l, fc_item_index, matches_list_f)
      test_names_tree_f[[fc_test_name]] <- pval_test_names
    }
    return(test_names_tree_f)
  }
  
  get_fold_changes_data <- function(test_settings_l, fc_index_f, rtr_copy_f) {
    # _Loads fold changes data from results tracker (rtr)
    fc_settings_item <- test_settings_l$fc_tests[[fc_index_f]]
    fc_rtr_index <- fc_settings_item$rtr_location
    rtr_result <- get_test_result_from_rtr(fc_rtr_index, rtr_copy_f)
    return(rtr_result)
  }
  
  get_pval_data <- function(test_settings_l, fc_index_f, pval_index_f, matches_list_f, rtr_copy_f) {
    # _Loads p-values data from results tracker (rtr)
    matches_list_row <- matches_list_f[[fc_index_f]]
    pval_settings_item_index <- matches_list_row[pval_index_f]
    pval_settings_item <- test_settings_l$pval_tests[[pval_settings_item_index]]
    pval_rtr_index <- pval_settings_item$rtr_location
    rtr_result <- get_test_result_from_rtr(pval_rtr_index, rtr_copy_f)
    return(rtr_result)
  }
  
  
  get_plot_data_frame <- function(results_set_fc, results_set_pval, fold_change_direction_f, p_adjust_f) {
    # _Produces the data frame with fold changes and p-values that is used to draw the volcano plot
    pval_input_frame <- NULL
    if(p_adjust_f) {
      pval_input_frame <- results_set_pval@out_frame
    } else {
      pval_input_frame <- results_set_pval@unadjusted_out_frame
    }
    
    variable_names <- as.character(results_set_fc@out_frame[,1])
    fold_data <- NULL
    if(fold_change_direction_f == "group1_vs_group2") {
      fold_data <- as.numeric(log2(results_set_fc@out_frame[,2]))
    } else if(fold_change_direction_f == "group2_vs_group1") {
      fold_data <- as.numeric(log2(results_set_fc@out_frame[,3]))
    }
    pval_data <- as.numeric(pval_input_frame[,2])
    plot_data_frame_f <- data.frame(variables = variable_names, folds = fold_data, pvals = pval_data, stringsAsFactors = FALSE)
    return(plot_data_frame_f)
  }
  
  generate_plotly_volcano_plot <- function(diff_df, pval_significance_threshold_a, log2_fc_threshold_a) {
    # _Function to draw a volcano plot
    # _Adapted from the code at https://www.biostars.org/p/214100/
    
    change_data_frame_groupings <- function(diff_df_f) {
      # _Function to group data by p values and fold changes for volcano plot
      # _Adapted from the code at https://www.biostars.org/p/214100/
      
      # __add a grouping column; default value is "not significant"
      diff_df_f["group"] <- "NotSignificant"
      
      # __for the plot, the highlighted values will be
      # __FDR < 0.05 (significance level)
      # __Fold Change > 1.5
      
      # __change the grouping for the entries with significance but not a large enough Fold change
      diff_df_f[which(diff_df_f['FDR'] < pval_significance_threshold_a & abs(diff_df_f['Fold']) < log2_fc_threshold_a ),"group"] <- "Significant"
      
      # __change the grouping for the entries a large enough Fold change but not a low enough p value
      diff_df_f[which(diff_df_f['FDR'] > pval_significance_threshold_a & abs(diff_df_f['Fold']) > log2_fc_threshold_a ),"group"] <- "FoldChange"
      
      # __change the grouping for the entries with both significance and large enough fold change
      diff_df_f[which(diff_df_f['FDR'] < pval_significance_threshold_a & abs(diff_df_f['Fold']) > log2_fc_threshold_a ),"group"] <- "Significant&FoldChange"
      return(diff_df_f)
    }
    
    find_and_label_top_peaks <- function(diff_df_f, ind = NULL) {
      # _Function copied from https://www.biostars.org/p/214100/
      # __Find and label the top peaks..
      a <- NULL
      if(!is.null(ind)) {
        

        top_peaks <- diff_df_f[ind,]
        colnames(top_peaks) <- colnames(diff_df_f)
        
        a <- list()
        for (i in seq_len(nrow(top_peaks))) {
          m <- top_peaks[i, ]
          a[[i]] <- list(
            x = m[["Fold"]],
            y = -log10(m[["FDR"]]),
            text = m[["external_variable_name"]],
            xref = "x",
            yref = "y",
            showarrow = TRUE,
            arrowhead = 0.5,
            ax = 20,
            ay = -40
          )
        }
      }
      
      return(a)
    }
    
    get_min_and_max_values_for_volcano_plot_dotted_lines <- function(fold_f, fdr_f, pval_significance_threshold_f, log2_fc_threshold_a) {
      # _Finds the minimum and maximum values for the dotted lines on the volcano plot
      margins_multiplier <- 1.15
      out_list <- list()
      
      h_line_min <- min(diff_df$Fold, na.rm = TRUE)
      if(h_line_min > 0){
        h_line_min <- h_line_min*margins_multiplier
      }
      if(h_line_min>(-log2_fc_threshold_a)) {
        h_line_min <- -log2_fc_threshold_a
      }
      h_line_max <- max(diff_df$Fold, na.rm = TRUE)
      if(h_line_max > 0){
        h_line_max <- h_line_max*margins_multiplier
      }
      if(h_line_max<(log2_fc_threshold_a)) {
        h_line_max <- log2_fc_threshold_a
      }
      
      v_line_max <- max(-log10(diff_df$FDR), na.rm = TRUE)
      if(v_line_max > 0){
        v_line_max <- v_line_max*margins_multiplier
      }
      if(v_line_max<(-log10(pval_significance_threshold_f))) {
        v_line_max <- (-log10(pval_significance_threshold_f))
      }
      out_list$h_line_min <- h_line_min
      out_list$h_line_max <- h_line_max
      out_list$v_line_max <- v_line_max
      return(out_list)
    }
    
    get_dotted_lines_for_plot <- function(fold_f, fdr_f, pval_significance_threshold_f, log2_fc_threshold_a) {
      # _Generates the dotted lines for volcano plot
      min_max_vals <- get_min_and_max_values_for_volcano_plot_dotted_lines(fold_f, fdr_f, pval_significance_threshold_f, log2_fc_threshold_a)
      
      lines1 <- list()
      line1 <- list(type='line', x0= log2_fc_threshold_a, x1= log2_fc_threshold_a, y0=0, y1=min_max_vals$v_line_max, line=list(dash='dot', width=2, color='#b3b3b3'))
      lines1 <- append(lines1, list(line1))
      line2 <- list(type='line', x0= -log2_fc_threshold_a, x1= -log2_fc_threshold_a, y0=0, y1=min_max_vals$v_line_max, line=list(dash='dot', width=2, color='#b3b3b3'))
      lines1 <- append(lines1, list(line2))
      line3 <- list(type='line', x0= min_max_vals$h_line_min, x1= min_max_vals$h_line_max, y0=-log10(pval_significance_threshold_a), y1=-log10(pval_significance_threshold_a), line=list(dash='dot', width=2, color='#b3b3b3'))
      lines1 <- append(lines1, list(line3))
      return(lines1)
    }
    
    colnames(diff_df) <- c("external_variable_name", "Fold", "FDR")
    diff_df <- change_data_frame_groupings(diff_df)
    variable_ind <- which(diff_df$external_variable_name %in% input$volcano_plot_selectizeinput)
    
    a <- find_and_label_top_peaks(diff_df, ind=variable_ind) #,ind=input$tablex_rows_selected)
    if(is.null(variables_for_arrows$l)) {
      a <- NULL
    }
    lines1 <- get_dotted_lines_for_plot(diff_df$Fold, diff_df$FDR, pval_significance_threshold_a, log2_fc_threshold_a)
    
    p <- plot_ly(data = diff_df, type = "scatter", x = diff_df$Fold, y = -log10(diff_df$FDR), key = ~diff_df$external_variable_name, source = "volcano_plot1", text = diff_df$external_variable_name, mode = "markers", color = diff_df$group) %>%
      layout(shapes=lines1,
             title = "Volcano plot",
             xaxis = list(title = "log2(fold change)", showgrid = TRUE),
             yaxis = list(title = "-log10(p)", showgrid = TRUE), dragmode =  "select") %>%
      layout(annotations = a)
    return(p)
  }
  
  observeEvent(input$get_volcano_plot_inputs_button, {
    isolate({
      rtr_copy$a <- rtr_f$a
      test_settings$l <- get_test_queue_details_of_test_results(rtr_copy$a, excluded_indices = 7)
      matching_tests$a <- find_matching_pval_tests_for_fc_tests(test_settings$l, ignored_indices_setting)
      fc_test_index1 <- 1
      test1 <- get_the_names_of_pval_tests_matching_an_fc_test(test_settings$l, fc_test_index1, matching_tests$a)
      test_names_tree$a <- get_the_fc_test_and_pval_test_selector_tree(test_settings$l, matching_tests$a)
      
      output$test_choice_selectinputs_container1 = renderUI({
        tagList(
          selectInput(ns("fc_selectinput"), label = "Fold changes test:", choices = names(test_names_tree$a))
        )
      })
      
      output$test_choice_selectinputs_container2 = renderUI({
        tagList(
          selectInput(ns("pval_selectinput"), label = "p-value yielding test:", choices = test_names_tree$a[[test_names_tree_selection_reactive()]])
        )
      })
      
      output$test_choice_button_container = renderUI({
        tagList(
          actionButton(ns("generate_volcano_plot_button"), "Generate volcano plot"),
          bsTooltip(id = ns("generate_volcano_plot_button"), title = "Click here to generate a volcano plot", placement = "left", trigger = "hover"),
          tags$hr(),
          actionButton(ns("change_plot_settings_button"), "Volcano plot settings...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
          bsTooltip(id = ns("change_plot_settings_button"), title = "Click here to modify volcano plot settings", placement = "left", trigger = "hover")
        )
      })
      
    })
  })
  
  observe({
    if(settings_panel_visible_reactive() == TRUE) {
      isolate({
        output$volcano_plot_settings_container = renderUI({
          wellPanel(
            numericInput(ns("volcano_plot_pvalue_significance_numericinput"), "Maximum p-value that is considered statistically significant: ", value = 0.05, min = 0, max = 1, step = 0.001),
            htmlOutput(ns("pvalue_significance_current_value_htmloutput")),
            actionButton(ns("set_significance_p_value_button"), "Apply the p-value threshold"),
            bsTooltip(id = ns("set_significance_p_value_button"), title = "Click here to change the threshold of statistical significance of p-values", placement = "left", trigger = "hover"),
            tags$hr(),
            numericInput(ns("volcano_plot_fold_change_significance_numericinput"), "Minimum log2 fold change that is considered as differential expression: ", value = 2, min = 0),
            htmlOutput(ns("fold_change_significance_current_value_htmloutput")),
            actionButton(ns("set_fold_change_threshold_button"), "Apply the log2 fold change threshold"),
            bsTooltip(id = ns("set_fold_change_threshold_button"), title = "Click here to change the threshold of log2 fold change that is considered as differential expression", placement = "left", trigger = "hover"),
            tags$hr(),
            radioButtons(ns("fold_change_calculation_direction_radiobuttons"), "Fold change calculation direction:",
                         c("Group 1 / Group 2" = "group1_vs_group2",
                           "Group 2 / Group 1" = "group2_vs_group1")),
            htmlOutput(ns("fold_change_direction_current_value_htmloutput")),
            actionButton(ns("apply_fold_change_direction_button"), "Apply the fold change direction"),
            bsTooltip(id = ns("apply_fold_change_direction_button"), title = "Click here to change the direction of calculation of fold changes", placement = "left", trigger = "hover"),
            tags$hr(),
            radioButtons(ns("p_values_multiple_testing_radiobuttons"), "Multiple testing correction of p-values:",
                         c("Use adjusted p-values" = "adjusted",
                           "Use unadjusted p-values" = "unadjusted")),
            htmlOutput(ns("multiple_testing_correction_current_value_htmloutput")),
            actionButton(ns("apply_p_values_adjustment_setting_button"), "Apply the p-values adjustment setting"),
            bsTooltip(id = ns("apply_p_values_adjustment_setting_button"), title = "Click here to change the mode of adjustment of p-values", placement = "left", trigger = "hover"),
            tags$hr(),
            actionButton(ns("refresh_the_volcano_plot"), "Refresh the volcano plot", icon("refresh", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("refresh_the_volcano_plot"), title = "Click here to update the volcano plot", placement = "left", trigger = "hover")
            
          )
        })
      })
    } else {
      isolate({
        output$volcano_plot_settings_container = renderUI({})
      })
    }
    
  })
  
  
  
  output$pvalue_significance_current_value_htmloutput <- renderText({ 
    paste("Current value: ", plot_settings$pval_threshold)
  })
  
  output$fold_change_significance_current_value_htmloutput <- renderText({ 
    paste("Current value: ", plot_settings$log2_fc_threshold)
  })
  
  observe({
    if(plot_settings$fold_change_direction == "group1_vs_group2") {
      isolate({
        output$fold_change_direction_current_value_htmloutput <- renderText({ 
          paste("Current setting: group 1 / group 2")
        })
      })
    } else {
      isolate({
        output$fold_change_direction_current_value_htmloutput <- renderText({ 
          paste("Current setting: group 2 / group 1")
        })
      })
    }
  })
  
  observe({
    if(plot_settings$p_adjust == TRUE) {
      isolate({
        output$multiple_testing_correction_current_value_htmloutput <- renderText({ 
          paste("Current setting: adjusted p-values")
        })
      })
    } else {
      isolate({
        output$multiple_testing_correction_current_value_htmloutput <- renderText({ 
          paste("Current setting: unadjusted p-values")
        })
      })
    }
  })
  
  observeEvent(input$set_significance_p_value_button, {
    isolate({
      x <- input$volcano_plot_pvalue_significance_numericinput
      if(is.numeric(x) & x > 0 & x < 1) {
        plot_settings$pval_threshold <- x
      } else {
        show_generic_error_message("This is not a valid value for this setting")
      }
    })
  })
  
  observeEvent(input$set_fold_change_threshold_button, {
    isolate({
      x <- input$volcano_plot_fold_change_significance_numericinput
      if(is.numeric(x) & x > 0) {
        plot_settings$log2_fc_threshold <- x
      } else {
        show_generic_error_message("This is not a valid value for this setting")
      }
    })
  })
  
  
  observeEvent(input$apply_fold_change_direction_button, {
    isolate({
      plot_settings$fold_change_direction <- input$fold_change_calculation_direction_radiobuttons
    })
  })
  
  observeEvent(input$apply_p_values_adjustment_setting_button, {
    isolate({
      if(input$p_values_multiple_testing_radiobuttons == "adjusted") {
        plot_settings$p_adjust <- TRUE
      } else {
        plot_settings$p_adjust <- FALSE
      }
    })
  })
  
  
  observeEvent(input$change_plot_settings_button, {
    isolate({
      settings_panel_visible$a <- !settings_panel_visible$a
      if(settings_panel_visible$a) {
        updateActionButton(session, "change_plot_settings_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome")) 
      } else {
        updateActionButton(session, "change_plot_settings_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$fc_selectinput, {
    isolate({
      sel_index <- which(names(test_names_tree$a) == input$fc_selectinput)
      test_names_tree_selection$a <- sel_index
    })
  })
  
  generate_new_volcano_plot <- function() {
    # _Function that loads the data for the volcano plot and then calls the functions to generate the plot
    
    selected_test$fc_name <<- input$fc_selectinput
    selected_test$fc_index <<- which(names(test_names_tree$a) == input$fc_selectinput)
    selected_test$pval_name <<- input$pval_selectinput
    selected_test$pval_index <<- which(test_names_tree$a[[selected_test$fc_index]] == selected_test$pval_name)
    
    results_set$fc <<- get_fold_changes_data(test_settings$l, selected_test$fc_index, rtr_copy$a)
    results_set$pval <<- get_pval_data(test_settings$l, selected_test$fc_index, selected_test$pval_index, matching_tests$a, rtr_copy$a)
    
    plot_data_frame$a <<- get_plot_data_frame(results_set$fc, results_set$pval, plot_settings$fold_change_direction, plot_settings$p_adjust)
    p_volcano <- generate_plotly_volcano_plot(plot_data_frame$a, plot_settings$pval_threshold, plot_settings$log2_fc_threshold)
    
    output$volcano_plot_container = renderUI({
      tagList(
        plotlyOutput(ns("volcano_plot")),
        wellPanel(
          htmlOutput(ns("volcano_plot_selected_items_summary")),
          actionButton(ns("choose_plot_selection_for_marking_with_arrows"), "Choose the variables that are selected on the plot for marking with arrows"),
          bsTooltip(id = ns("choose_plot_selection_for_marking_with_arrows"), title = "Click here to choose the variables that are selected on the plot for marking with arrows", placement = "left", trigger = "hover")
        )
      )
    })
    
    
    output$volcano_plot_selectizeinput_container = renderUI({
      wellPanel(
        selectizeInput(ns("volcano_plot_selectizeinput"), "Variables to highlight with arrows on the plot:", choices = plot_data_frame$a$variables, selected = variables_for_arrows$l, multiple = TRUE,
                       options = NULL),
        actionButton(ns("mark_variables_with_arrows_button"), "Mark selected variables"),
        bsTooltip(id = ns("mark_variables_with_arrows_button"), title = "Click here to mark the selected variables with arrows", placement = "left", trigger = "hover")
      )
    })
    
    
    output$volcano_plot <- renderPlotly({
      p_volcano
    })
    
  }
  
  update_volcano_plot_selected_items_summary <- function() {
    # _Function to update the list of selected variables displayed under the plotly PCA scores plot
    summary_f <- "<b>Selected variables </b><br>"
    volcano_plot_d <- event_data(event = "plotly_selected", source = "volcano_plot1")
    if (is.null(volcano_plot_d)) {
      summary_f <- paste(summary_f, "None", sep="")
      volcano_plot_selection$a <- NULL
    }
    else {
      selected_items_string <- paste(toString(volcano_plot_d$key), sep="")
      volcano_plot_selection$a <- volcano_plot_d$key
      summary_f <- paste(summary_f, selected_items_string, sep="")
    }
    return(summary_f)
  }
  
  output$volcano_plot_selected_items_summary <- renderText({
    update_volcano_plot_selected_items_summary()
  })
  
  
  
  observeEvent(input$generate_volcano_plot_button, {
    isolate({
      generate_new_volcano_plot()
    })
  })
  
  observeEvent(input$refresh_the_volcano_plot, {
    isolate({
      generate_new_volcano_plot()
    })
  })
  
  observeEvent(input$mark_variables_with_arrows_button, {
    isolate({
      variables_for_arrows$l <- input$volcano_plot_selectizeinput
      generate_new_volcano_plot()
    })
  })
  
}

