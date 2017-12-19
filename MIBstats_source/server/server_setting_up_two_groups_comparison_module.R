# _Module for setting up statistical tests that compare two groups of samples. Used for all statistical methods in the app except iGA and db-iGA

setting_up_two_groups_comparison_moduleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    tags$h3("Choose data for comparing two groups"),
    tags$h4("Data summary"),
    actionButton(ns("two_groups_test_page_view_data_summary_button"), "View input data summary", icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("two_groups_test_page_view_data_summary_button"), title = "Click here to view input data summary",
              placement = "left", trigger = "hover"),
    uiOutput(ns("two_groups_test_page_data_summary_panel")),
    tags$hr(),
    tags$h4("Samples in two groups"),
    helpText("New test to compare two groups: choose the samples to compare"),
    
    
    fluidRow(
      column(6, div(style = "height:150px;", uiOutput(ns("two_groups_test_choose_sample_class_selectinput")))
      ),
      column(6, div(style = "height:150px;", uiOutput(ns("two_groups_test_choose_sample_subset_selectinput")))
      )
    ),
    fluidRow(column(12, div(uiOutput(ns("make_subsets_of_input_data_panel"))))),

    
    actionButton(ns("add_to_two_groups_test_group_1"), "Add to test group 1", icon("plus", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("add_to_two_groups_test_group_1"), title = "Click here to add the selected sample class to group nr. 1 of the two groups comparison",
              placement = "left", trigger = "hover"),
    actionButton(ns("add_to_two_groups_test_group_2"), "Add to test group 2", icon("plus", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("add_to_two_groups_test_group_2"), title = "Click here to add the selected sample class to group nr. 2 of the two groups comparison",
              placement = "left", trigger = "hover"),
    
    
           
    helpText("Sample classes selected for the new test:"),
    fluidRow(
      column(12, style = "background-color:lightsteelblue;",
             tableOutput(ns("new_two_groups_samples_table_from_dataframe"))
      )
    ),
    fluidRow(
      actionButton(ns("add_to_queued_two_group_tests"), "Add to queued tests", icon("plus", class = NULL, lib = "font-awesome")),
      bsTooltip(id = ns("add_to_queued_two_group_tests"), title = "Click here to add the test with the selected sample classes and parameters to the list of queued statistical tests",
                placement = "left", trigger = "hover"),
      actionButton(ns("clear_new_two_groups_test_list"), "Clear the list", icon("remove", class = NULL, lib = "font-awesome")),
      bsTooltip(id = ns("clear_new_two_groups_test_list"), title = "Click here if you wish to reset the list of sample classes that have been selected for the statistical test",
                placement = "left", trigger = "hover")
    ),
    
    actionButton(ns("configure_two_groups_test_item_button"), label = "Test parameters...", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("configure_two_groups_test_item_button"), title = "Click here to change test parameters",
              placement = "left", trigger = "hover"),
    uiOutput(ns("configure_two_groups_test_item_panel")),
    
    uiOutput(ns("test_setup_step2_container")),
    

    help_box_moduleUI(ns("help_setting_up_test"))
  )
}

setting_up_two_groups_comparison_module <- function(input, output, session, t1_f_reactive) {
  
  ns <- session$ns
  
  callModule(help_box_module, "help_setting_up_test")
  callModule(help_box_module, "help_pvalue_correction")
  
  t1_f <- reactiveValues(a = NULL)
  t1_f$a <- input_data_table()
  t1_fa_reactive <- reactive({t1_f$a})
  t1_f$a@subsets <- list(l = list())
  
  updated_summary <- reactiveValues(a = NULL)
  

  initialise_two_groups_test_item_table <- function() {
    # _Initialises the table of choosing the sample groups for comparing
    nodata <- data.frame(x = "(none)", y = "(none)")
    colnames(nodata) <- c("Group #1", "Group #2")
    output$new_two_groups_samples_table_from_dataframe <- renderTable(nodata, rownames = FALSE, selection = "none")
  }
  
  initialise_two_groups_test_item_table()
  
  observe({
    t1_f_observed <- t1_f_reactive()
    if(!is.null(t1_f_observed)) {
      isolate({
        if(!is.null(t1_f_observed@df)) {
          t1_f$a <- t1_f_observed
          t1_f$a@subsets <- list(l = list())
          updated_summary$a <- update_input_data_summary(t1_f$a)
          callModule(class_labels_summary_box, "class_labels_summary_box_two_groups_test", reactive({updated_summary$a$data_summary_frame_class_label_statistics}))
          
        }
      })
    }
  })
  
  
  
  two_groups_test_sample_class_selectinput_choices <- reactiveValues(l = NULL)
  
  multiple_testing_correction_panel_visible <- reactiveValues(l = FALSE)
  multiple_testing_correction_select <- reactiveValues(l = "BH")
  
  input_data_subsets_panel_visible <- reactiveValues(l = FALSE)
  
  input_data_subsets_counter <- reactiveValues(a = 1)
  
  sample_pairs_setup <- reactiveValues(g1 = NULL, g2 = NULL, combined = NULL)
  
  configure_two_groups_test_item_panel_visible <- reactiveValues(a = FALSE)
  two_groups_test_page_data_summary_panel_visible <- reactiveValues(a = FALSE)
  two_groups_test_page_data_summary_panel_visible_reactive <- reactive({two_groups_test_page_data_summary_panel_visible$a})
  
  two_groups_test_item_settings <- reactiveValues(welch = TRUE, paired = FALSE, groups_descriptive_label = "")
  
  
  q1 <- two_groups_test_queue(l = list())
  
  q1@l$two_sample_test_group <- reactiveValues(a = NULL, b = NULL, a_subset = NULL, b_subset = NULL)
  q1@l$queued_two_sample_tests <- reactiveValues(l = NULL)
  q1@l$queued_two_sample_tests_frame <- reactiveValues(df = NULL)
  q1@l$queued_two_sample_tests_pairings <- reactiveValues(l = NULL)
  q1@l$editing_queued_two_sample_tests_row_index <- reactiveValues(a = NULL)
  
  q1@l$queued_two_sample_tests$l <- list()
  
  output_button <- reactiveValues(a = NULL, counter = 0)
  output_list <- reactiveValues(a = NULL)
  output_reactive <- reactive({output_list$a})
  
  stat_test_selectinput_choices <- reactiveValues(l = c("Shapiro-Wilk test" = "shapiro_test", "F-test" = "f_test", "t-test" = "t_test", "Mann-Whitney U test" = "mann_whitney_u_test", "Fold changes" = "fold_changes", "Rank product" = "rank_product", "Rank sum" = "rank_sum"))
  

  output$multiple_comparisons_correction_selection <- renderText({get_full_name_of_p_val_adjustment_method(multiple_testing_correction_select$l)})
  
  observeEvent(input$change_multiple_comparisons_correction, {
    isolate({
      multiple_testing_correction_panel_visible$l <- !multiple_testing_correction_panel_visible$l
    })
    
  })
  
  observe({
    if (configure_two_groups_test_item_panel_visible$a == TRUE) {
      isolate({
        output$configure_two_groups_test_item_panel <- renderUI({
          wellPanel(
            textInput(ns("groups_descriptive_label"), "Descriptive label for the groups (optional):", value = two_groups_test_item_settings$groups_descriptive_label),
            helpText("Pairing of samples:"),
            checkboxInput(ns("paired_test_checkbox"), "The samples are paired", value = two_groups_test_item_settings$paired),
            helpText("Multiple comparisons correction"),
            helpText("Currently selected:"),
            verbatimTextOutput(ns("multiple_comparisons_correction_selection")),
            actionButton(ns("change_multiple_comparisons_correction"), "Change multiple comparisons correction type...", icon = icon("wrench", class = NULL, lib = "font-awesome"), placement = "left", trigger = "hover"),
            bsTooltip(id = ns("change_multiple_comparisons_correction"), title = "Optional: click here to change the type of multiple comparisons correction"),
            uiOutput(ns("multiple_testing_correction_radiobuttons_panel")),
            actionButton(ns("apply_changes_to_test_settings_button"), "Apply the changes to test settings"),
            bsTooltip(id = ns("apply_changes_to_test_settings_button"), title = "Click here to apply the changes to test settings")
          )
        })
      })
    } else {
      isolate({
        output$configure_two_groups_test_item_panel <- renderUI({})
      })
    }
  })
  
  
  observeEvent(input$configure_two_groups_test_item_button, {
    isolate({
      configure_two_groups_test_item_panel_visible$a <- !configure_two_groups_test_item_panel_visible$a
      multiple_testing_correction_panel_visible$l <- FALSE
      if(configure_two_groups_test_item_panel_visible$a) {
        updateActionButton(session, "configure_two_groups_test_item_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "configure_two_groups_test_item_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
    
  })
  
  observeEvent(input$two_groups_test_page_view_data_summary_button, {
    isolate({
      
      two_groups_test_page_data_summary_panel_visible$a <- !two_groups_test_page_data_summary_panel_visible$a
      if(two_groups_test_page_data_summary_panel_visible$a) {
        updateActionButton(session, "two_groups_test_page_view_data_summary_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else if (!two_groups_test_page_data_summary_panel_visible$a) {
        input_data_subsets_panel_visible$l <- FALSE
        updateActionButton(session, "two_groups_test_page_view_data_summary_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
      
    })
    
  })
  
  
  
  observe({
    if (multiple_testing_correction_panel_visible$l == TRUE) {
      isolate({
        output$multiple_testing_correction_radiobuttons_panel <- renderUI({
          tagList(
            radioButtons(ns("t_test_multiple_comparisons_radiobutton"), "Multiple comparisons correction:",
                         multiple_testing_correction_radiobutton_names(), selected = multiple_testing_correction_select$l),
            help_box_moduleUI(ns("help_pvalue_correction"))
          )
        })
      })
      
    } else {
      isolate({
        output$multiple_testing_correction_radiobuttons_panel <- renderUI({})
      })
    }
  })
  
  
  observeEvent(input$apply_changes_to_test_settings_button, {
    isolate({
      if(!is.null(input$t_test_multiple_comparisons_radiobutton)) {
        multiple_testing_correction_select$l <- input$t_test_multiple_comparisons_radiobutton
      }
      if(!is.null(input$paired_test_checkbox)) {
        two_groups_test_item_settings$paired <- input$paired_test_checkbox
      }
      if(!is.null(input$groups_descriptive_label)) {
        two_groups_test_item_settings$groups_descriptive_label <- input$groups_descriptive_label
      }
    })
  })
  
  
  observe({
    summary_panel_visible <- two_groups_test_page_data_summary_panel_visible_reactive()
    isolate({
      if (summary_panel_visible == TRUE) {
        output$two_groups_test_page_data_summary_panel <- renderUI({
          tagList(
            class_labels_summary_boxUI(ns("class_labels_summary_box_two_groups_test"))
          )
        })
      } else {
        output$two_groups_test_page_data_summary_panel <- renderUI({})
      }
    })
    
  })
  
  # _Outputs data table statistics in a text field on the two_groups page
  output$data_loading_summary_two_groups_test_page <- renderText({
    summary_for_printing <- prepare_table_statistics_list_for_printing_html(idc1)
  })
  
  
  observe({
    t1_observed <- t1_fa_reactive()
    if(!is.null(t1_observed)) {
      isolate({
        two_groups_test_sample_class_selectinput_choices$l <- unique_class_labels_df_to_vector(two_groups_test_queue(), t1_observed@class_labels) 
      })
    }
  })
  
  output$two_groups_test_choose_sample_class_selectinput <- renderUI({
    tagList(
      selectInput(ns("sample_class_from_selectinput"), "Sample class:", two_groups_test_sample_class_selectinput_choices$l)
    )
  })
  
  initialise_queued_two_sample_tests_frame <- function() {
    empty_frame_f <- data.frame(label= character(0), groups1 = character(0), subset_g1 = character(0), groups2 = character(0), subset_g2 = character(0), welch = logical(0), paired = logical(0), p_adjustment = character(0))
    return(empty_frame_f)
  }
  
  
  q1@l$queued_two_sample_tests_frame$df <- initialise_queued_two_sample_tests_frame()
  
  
  refresh_new_two_groups_test_class_labels_box <- function(class_labels_a, subsets_a, class_labels_b, subsets_b) {
    # _Refreshes the table that contains the names of the sample classes selected for a new test
    html_string <- class_label_vectors_to_html_report(q1, class_labels_a, subsets_a, class_labels_b, subsets_b)
    groups_report_frame_f <- two_groups_html_report_to_data_frame(q1, html_string)
    
    output$new_two_groups_samples_table_from_dataframe <- renderTable(groups_report_frame_f, rownames = FALSE, selection = "none")
  }
  
  display_error_message_when_adding_sample_class_to_multiple_groups <- function() {
    # _A sample class cannot be added to the same test group twice, so an error message is displayed if the user tries to do this
    show_error_message_two_groups_test_preparation(q1, "This sample class has already been added to a group.")
  }
  
  check_if_element_already_added_to_test_group <- function(new_element, new_element_subset) {
    # _Function for checking if a sample class has already been added to a test group
    element_already_added_f <- FALSE
    if(is.element(new_element, q1@l$two_sample_test_group$a) && is.element(new_element_subset, q1@l$two_sample_test_group$a_subset)) {
      element_already_added_f <- TRUE
    }
    if(is.element(new_element, q1@l$two_sample_test_group$b) && is.element(new_element_subset, q1@l$two_sample_test_group$b_subset)) {
      element_already_added_f <- TRUE
    }
    return(element_already_added_f)
  }
  
  
  get_label_by_subset_name <- function(subset_selection_name_f, t1_f) {
    subset_selection_label_f <- ""
    
    subsets_list <- t1_f@subsets
    
    if(subset_selection_name_f != "None") {
      for(subset_item in subsets_list[[1]]) {
        
        
        if(subset_item@item_name == subset_selection_name_f) {
          subset_selection_label_f <- subset_item@item_label
          
          break
        }
      }
    }
    
    return(subset_selection_label_f)
  }
  
  add_item_to_two_groups_test_group <- function(group_nr) {
    # _Adds a sample class to one of the two test groups for a new test
    
    element_already_added <- check_if_element_already_added_to_test_group(toString(input$sample_class_from_selectinput), toString(input$sample_subset_from_selectinput))
    if(!element_already_added) {
      if(group_nr == 1) {
        q1@l$two_sample_test_group$a <- append(q1@l$two_sample_test_group$a, toString(input$sample_class_from_selectinput))
      } else if (group_nr == 2) {
        q1@l$two_sample_test_group$b <- append(q1@l$two_sample_test_group$b, toString(input$sample_class_from_selectinput))
      }
      subset_selection_name <- toString(input$sample_subset_from_selectinput)
      subset_selection_label <- get_label_by_subset_name(subset_selection_name, t1_f$a)
      names(subset_selection_name) <- subset_selection_label
      
      
      if(group_nr == 1) {
        q1@l$two_sample_test_group$a_subset <- append(q1@l$two_sample_test_group$a_subset, subset_selection_name)
      } else if (group_nr == 2) {
        q1@l$two_sample_test_group$b_subset <- append(q1@l$two_sample_test_group$b_subset, subset_selection_name)
      }
    } else {
      display_error_message_when_adding_sample_class_to_multiple_groups()
    }
    
    
    if(length(q1@l$two_sample_test_group$a) > 0 | length(q1@l$two_sample_test_group$b) > 0) {
      shinyjs::enable("clear_new_two_groups_test_list")
    }
    if(length(q1@l$two_sample_test_group$a) > 0 & length(q1@l$two_sample_test_group$b) > 0) {
      shinyjs::enable("add_to_queued_two_group_tests")
    }
    refresh_new_two_groups_test_class_labels_box(q1@l$two_sample_test_group$a, q1@l$two_sample_test_group$a_subset, q1@l$two_sample_test_group$b, q1@l$two_sample_test_group$b_subset)
  }
  
  observeEvent(input$add_to_two_groups_test_group_1, {
    isolate({
      add_item_to_two_groups_test_group(1)
    })
  })
  
  observeEvent(input$add_to_two_groups_test_group_2, {
    isolate({
      add_item_to_two_groups_test_group(2)
    })
  })
  
  observeEvent(input$clear_new_two_groups_test_list, {
    isolate({
      q1@l$two_sample_test_group$a <- NULL
      q1@l$two_sample_test_group$b <- NULL
      q1@l$two_sample_test_group$a_subset <- NULL
      names(q1@l$two_sample_test_group$a_subset) <- NULL
      q1@l$two_sample_test_group$b_subset <- NULL
      names(q1@l$two_sample_test_group$b_subset) <- NULL
      shinyjs::disable("clear_new_two_groups_test_list")
      shinyjs::disable("add_to_queued_two_group_tests")
    })
    initialise_two_groups_test_item_table()
  })
  
  
  update_queued_two_groups_test_variables <- function(new_df, new_l) {
    q1@l$queued_two_sample_tests_frame$df <- new_df
    q1@l$queued_two_sample_tests$l <- new_l
    q1@l$queued_two_sample_tests_pairings$l <- append(q1@l$queued_two_sample_tests_pairings$l, list("<Empty>"))
    update_queued_two_groups_test_table()
    
  }
  
  
  
  observe({
    if(!is.null(q1@l$queued_two_sample_tests$l) && length(q1@l$queued_two_sample_tests$l)>0) {
      isolate({
        output$test_setup_step2_container <- renderUI({
          tagList(
            tags$hr(),
            tags$h4("Test queue"),
            helpText("Queued tests"),
            column(12,
                   dataTableOutput(ns("queued_two_groups_test_samples_table"))),
            uiOutput(ns("paired_samples_help_message")),
            actionButton(ns("fill_two_groups_test_queue_with_all_possible_pairs"), "Fill the queue with all possible pairs of sample classes", icon("plus", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("fill_two_groups_test_queue_with_all_possible_pairs"), title = "Optional: click here if you wish to fill the test queue with all possible pairs of sample classes from the loaded dataset",
                      placement = "left", trigger = "hover"),
            actionButton(ns("clear_new_two_groups_test_table"), "Clear the table", icon("remove", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("clear_new_two_groups_test_table"), title = "Optional: click here if you wish to empty the list of queued tests",
                      placement = "left", trigger = "hover"),
            uiOutput(ns("two_groups_test_paired_samples_preview")),
            uiOutput(ns("editing_the_pairing_of_samples_panel")),
            tags$hr(),
            tags$h4("Running the test"),
            uiOutput(ns("running_the_test_container"))
          )
        })
      })
      
    } else {
      isolate({
        output$test_setup_step2_container <- renderUI({})
      })
    }
  })
  
  
  observe({
    if(two_groups_test_item_settings$paired == TRUE) {
      isolate({
        output$paired_samples_help_message <- renderUI({
          tagList(
            helpText("Click on the test queue row with paired samples in order to view or edit the pairing of samples")
          )
        })
      })
    } else {
      isolate({
        output$paired_samples_help_message <- renderUI({})
      })
    }
  })
  
  update_queued_two_groups_test_table <- function() {
    # _Updates the test queue table that is displayed on the screen
    output_frame_f <- get_two_groups_test_table_for_output(q1)
    output$queued_two_groups_test_samples_table <- renderDataTable({
      DT::datatable(output_frame_f, rownames = TRUE, selection = 'none', 
                    options = list(columnDefs = list(list(visible=FALSE, targets=6)), rowCallback = JS('
            function(nCol, nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                      // Bold and green cells for conditions
                                      //if (parseFloat(aData[3]) >= 200)
                                      //$("td:eq(3)", nRow).css("font-weight", "bold");
                                      //if (parseFloat(aData[3]) >= 100)
                                      //$("td:eq(3)", nRow).css("background-color", "#9BF59B");
                                      $(nCol, nRow).css("background-color", "lightsteelblue");
                                      //$(nCol, nRow).css("background-color", "#9BF59B");
                                       }')
                    )
      )
    })
    
  }
  
  
  observeEvent(input$fill_two_groups_test_queue_with_all_possible_pairs, {
    isolate({
      class_labels_vector <- unique_class_labels_df_to_vector(q1, t1_f$a@class_labels)
      
      pairings_list <- prepare_all_possible_pairings_of_class_labels_for_two_groups_test(q1, t1_f$a@class_labels, two_groups_test_item_settings$welch, two_groups_test_item_settings$paired, two_groups_test_item_settings$groups_descriptive_label, multiple_testing_correction_select$l)
      
      error_message_pairings_list <- ""
      for (list_i in 1:length(pairings_list)) {
        pairings_list_item <- pairings_list[[list_i]]
        new_test_queue_entry2 <- create_new_test_queue_entry(q1, pairings_list_item$groups1, "None", pairings_list_item$groups2, "None", pairings_list_item$welch, pairings_list_item$paired, pairings_list_item$label, multiple_testing_correction_select$l)
        
        new_queue2 <- add_entry_to_two_groups_test_queue(q1, t1_f$a, new_test_queue_entry2)
        if(!new_queue2$error_occurred) {
          update_queued_two_groups_test_variables(new_queue2$queue_frame, new_queue2$queue_list)
        } else {
          
          if(new_queue2$error_message != "This element has already been added") {
            error_message_pairings_list <- new_queue2$error_message
          }
        }
      }
      if(error_message_pairings_list != "") {
        show_error_message_two_groups_test_preparation(q1, error_message_pairings_list)
      }
      
    })
    
  })
  
  observeEvent(input$add_to_queued_two_group_tests, {
    isolate({
      if(!is.null(q1@l$two_sample_test_group$a) && !is.null(q1@l$two_sample_test_group$b)) {
        new_test_queue_entry <- create_new_test_queue_entry(q1, q1@l$two_sample_test_group$a, q1@l$two_sample_test_group$a_subset, q1@l$two_sample_test_group$b, q1@l$two_sample_test_group$b_subset, two_groups_test_item_settings$welch, two_groups_test_item_settings$paired, two_groups_test_item_settings$groups_descriptive_label, multiple_testing_correction_select$l)
        
        
        new_queue <- list()
        isolate({
          new_queue <- add_entry_to_two_groups_test_queue(q1, t1_f$a, new_test_queue_entry)
          if(!new_queue$error_occurred) {
            update_queued_two_groups_test_variables(new_queue$queue_frame, new_queue$queue_list)
            output_list$a <- list(q1@l, "none", 0)
          } else {
            show_error_message_two_groups_test_preparation(q1, new_queue$error_message)
          }
        })
      } else {
        show_error_message_two_groups_test_preparation(q1, "Both groups need to contain samples before they can be added to the test queue")
      }
    })
  })
  
  observeEvent(input$clear_new_two_groups_test_table, {
    isolate({
      q1@l$queued_two_sample_tests_frame$df <- initialise_queued_two_sample_tests_frame()
      q1@l$queued_two_sample_tests$l <- list()
      
      q1@l$queued_two_sample_tests_pairings$l <- NULL
      q1@l$editing_queued_two_sample_tests_row_index$a <- NULL
      
      update_queued_two_groups_test_table()
    })
  })
  
  observeEvent(input$queued_two_groups_test_samples_table_cell_clicked, {
    isolate({
      info2 = input$queued_two_groups_test_samples_table_cell_clicked
      
      
      if (is.null(info2$value)) return()
      table_row_cl <- (q1@l$queued_two_sample_tests_frame$df[info2$row,])
      columns_to_be_deleted_based_on_subsets <- get_columns_to_be_deleted_based_on_subsets(t1_f$a, table_row_cl)
      filtered_merged_frame <- filter_merged_frame_based_on_subsets(t1_f$a, get_merged_frame(t1_f$a), columns_to_be_deleted_based_on_subsets)
      
      paired_samples_info <- find_paired_samples_in_two_groups(q1, table_row_cl, filtered_merged_frame, info2$row)
      if(!is.null(paired_samples_info)) {
        output$two_groups_test_paired_samples_preview <- renderUI({
          tagList(
            HTML(paired_samples_info$output_text_full),
            actionButton(ns("edit_the_pairing_of_samples_button"), "Edit the pairing of samples..."),
            bsTooltip(id = ns("edit_the_pairing_of_samples_button"), title = "Click here to edit the pairing scheme of the samples",
                      placement = "left", trigger = "hover")
          )
        })
        
      } else {
        output$two_groups_test_paired_samples_preview <- renderUI({})
      }
    })
  })
  
  observeEvent(input$edit_the_pairing_of_samples_button, {
    isolate({
      info2 = input$queued_two_groups_test_samples_table_cell_clicked
      
      
      if (is.null(info2$value)) return()
      q1@l$editing_queued_two_sample_tests_row_index$a <- info2$row
      table_row_cl <- (q1@l$queued_two_sample_tests_frame$df[info2$row,])
      
      columns_to_be_deleted_based_on_subsets <- get_columns_to_be_deleted_based_on_subsets(t1_f$a, table_row_cl)
      filtered_merged_frame <- filter_merged_frame_based_on_subsets(t1_f$a, get_merged_frame(t1_f$a), columns_to_be_deleted_based_on_subsets)
      
      paired_samples_info <- find_paired_samples_in_two_groups(q1, table_row_cl, filtered_merged_frame, info2$row)
      
      shinyjs::disable("edit_the_pairing_of_samples_button")
      
      output$editing_the_pairing_of_samples_panel <- renderUI({
        tagList(
          helpText("Editing the pairing of samples"),
          helpText("Please maximise the app window in order to display the tables properly."),
          fluidRow(
            column(4,
                   dataTableOutput(ns("editing_sample_pairs_g1_table")),
                   actionButton(ns("add_g1_item_to_pairs_button"), "Add group 1 item to pairs"),
                   bsTooltip(id = ns("add_g1_item_to_pairs_button"), title = "Click here to add the selected group 1 item to the list of sample pairs",
                             placement = "left", trigger = "hover")
            ),
            column(4,
                   dataTableOutput(ns("editing_sample_pairs_combined_table")),
                   actionButton(ns("remove_item_from_pairs_button"), "Remove from pairs"),
                   bsTooltip(id = ns("remove_item_from_pairs_button"), title = "Click here to remove the selected item from the list of sample pairs",
                             placement = "left", trigger = "hover")
            ),
            column(4,
                   dataTableOutput(ns("editing_sample_pairs_g2_table")),
                   actionButton(ns("add_g2_item_to_pairs_button"), "Add group 2 item to pairs"),
                   bsTooltip(id = ns("add_g2_item_to_pairs_button"), title = "Click here to add the selected group 2 item to the list of sample pairs",
                             placement = "left", trigger = "hover")
            )
          ),
          actionButton(ns("apply_the_new_pairing_of_samples_button"), "Apply the new pairing of samples"),
          bsTooltip(id = ns("apply_the_new_pairing_of_samples_button"), title = "Click here to apply this pairing scheme to the samples",
                    placement = "left", trigger = "hover")
        )
      })
      
      group1_frame <- NULL
      group2_frame <- NULL
      
      if(!is.null(group1_frame)) {
        output$editing_sample_pairs_g1_table <- renderDataTable(group1_frame, selection = "single")
      }
      if(!is.null(group2_frame)) {
        output$editing_sample_pairs_g2_table <- renderDataTable(group2_frame, selection = "single")
      }
      
      sample_pairs_frame <- data.frame(paired_samples_info$group1_sample_labels, paired_samples_info$group2_sample_labels, stringsAsFactors = FALSE)
      names(sample_pairs_frame) <- c("group1", "group2")
      
      output$editing_sample_pairs_combined_table <- renderDataTable(sample_pairs_frame, selection = "single")
      
      sample_pairs_setup$g1 <- c()
      sample_pairs_setup$g2 <- c()
      sample_pairs_setup$combined <- sample_pairs_frame
      
    })
    
  })
  
  add_item_to_pairs <- function(group_f) {
    # _Function for manually editing the pairing of samples.
    # _This function deals with the adding of samples to the pairs table
    selected_row <- NULL
    sample_pairs_setup_f <- NULL
    if(group_f == 1) {
      selected_row <- input$editing_sample_pairs_g1_table_rows_selected
      sample_pairs_setup_f <- sample_pairs_setup$g1
    } else if (group_f == 2) {
      selected_row <- input$editing_sample_pairs_g2_table_rows_selected
      sample_pairs_setup_f <- sample_pairs_setup$g2
    }
    
    selected_value <- sample_pairs_setup_f[selected_row]
    
    if(!is.null(sample_pairs_setup_f)) {
      
      if (is.null(selected_value)) return()
      combined_table_selected_row <- input$editing_sample_pairs_combined_table_rows_selected
      if (is.null(combined_table_selected_row)) return()
      
      if(sample_pairs_setup$combined[combined_table_selected_row, group_f] != ("-")) return()
      
      sample_pairs_setup$combined[combined_table_selected_row, group_f] <- as.character(selected_value)
      
      if(group_f == 1) {
        sample_pairs_setup$g1 <- sample_pairs_setup$g1[-selected_row]
        group1_frame <- as.data.frame(sample_pairs_setup$g1, stringsAsFactors = FALSE)
        names(group1_frame) <- c("group1")
        output$editing_sample_pairs_g1_table <- renderDataTable(group1_frame, selection = "single")
      } else if (group_f == 2) {
        sample_pairs_setup$g2 <- sample_pairs_setup$g2[-selected_row]
        group2_frame <- as.data.frame(sample_pairs_setup$g2, stringsAsFactors = FALSE)
        names(group2_frame) <- c("group2")
        output$editing_sample_pairs_g2_table <- renderDataTable(group2_frame, selection = "single")
      }
      new_combined_frame <- sample_pairs_setup$combined
      output$editing_sample_pairs_combined_table <- renderDataTable(new_combined_frame, selection = "single")
      
    }
    update_the_apply_new_pairings_button_activation(check_if_new_pairings_list_of_samples_is_complete())
  }
  
  observeEvent(input$add_g1_item_to_pairs_button, {
    isolate({
      add_item_to_pairs(1)
      
    })
  })
  
  observeEvent(input$add_g2_item_to_pairs_button, {
    isolate({
      add_item_to_pairs(2)
    })
  })
  
  observeEvent(input$remove_item_from_pairs_button, {
    isolate({
      selected_row <- input$editing_sample_pairs_combined_table_rows_selected
      
      if (is.null(selected_row)) return()
      if(sample_pairs_setup$combined[selected_row, 1] == ("-") && sample_pairs_setup$combined[selected_row, 2] == ("-")) return()
      
      
      if(sample_pairs_setup$combined[selected_row, 1] != ("-")) {
        sample_pairs_setup$g1 <- c(sample_pairs_setup$g1, sample_pairs_setup$combined[selected_row, 1])
        group1_frame <- as.data.frame(sample_pairs_setup$g1, stringsAsFactors = FALSE)
        names(group1_frame) <- c("group1")
        output$editing_sample_pairs_g1_table <- renderDataTable(group1_frame, selection = "single")
      }
      
      if(sample_pairs_setup$combined[selected_row, 2] != ("-")) {
        sample_pairs_setup$g2 <- c(sample_pairs_setup$g2, sample_pairs_setup$combined[selected_row, 2])
        group2_frame <- as.data.frame(sample_pairs_setup$g2, stringsAsFactors = FALSE)
        names(group2_frame) <- c("group2")
        output$editing_sample_pairs_g2_table <- renderDataTable(group2_frame, selection = "single")
      }
      
      sample_pairs_setup$combined[selected_row, 1] <- as.character("-")
      sample_pairs_setup$combined[selected_row, 2] <- as.character("-")
      new_combined_frame <- sample_pairs_setup$combined
      
      output$editing_sample_pairs_combined_table <- renderDataTable(new_combined_frame, selection = "single")
      update_the_apply_new_pairings_button_activation(check_if_new_pairings_list_of_samples_is_complete())
    })

  })
  
  check_if_new_pairings_list_of_samples_is_complete <- function() {
    # _Function for manually editing the pairing of samples.
    # _This function checks if the new pairings list made by the user is complete (i.e., all samples have been assigned pairings), so that the new pairings can be submitted
    is_complete_f <- TRUE
    if(length(sample_pairs_setup$g1) != 0 | length(sample_pairs_setup$g2) != 0) {
      is_complete_f <- FALSE
    }
    return(is_complete_f)
  }
  
  
  update_the_apply_new_pairings_button_activation <- function(is_complete_f) {
    # _Function for manually editing the pairing of samples.
    # _The button of applying the new pairings is enabled when the pairings list is complete and disabled when it is not
    if(is_complete_f) {
      shinyjs::enable("apply_the_new_pairing_of_samples_button")
    } else {
      shinyjs::disable("apply_the_new_pairing_of_samples_button")
    }
  }
  
  observeEvent(input$apply_the_new_pairing_of_samples_button, {
    isolate({
      q1@l$queued_two_sample_tests_pairings$l[[q1@l$editing_queued_two_sample_tests_row_index$a]] <- list(sample_pairs_setup$combined[,1], sample_pairs_setup$combined[,2])
      shinyjs::enable("edit_the_pairing_of_samples_button")
      
      output$two_groups_test_paired_samples_preview <- renderUI({})
      output$editing_the_pairing_of_samples_panel <- renderUI({})
      
    })
  })
  
  
  # _--------------------------------------------------------------------------------------------------------------------
  # _The following part of this module deals with making subsets of the data
  output$two_groups_test_choose_sample_subset_selectinput <- renderUI({
    tagList(
      selectInput(ns("sample_subset_from_selectinput"), "Sample class subset:", choices = c("None"), width = '100%'),
      actionButton(ns("view_list_of_samples_in_subset"), "View a list of samples in the subset"),
      bsTooltip(id = ns("view_list_of_samples_in_subset"), title = "Click here view a list of samples contained in this subset",
                placement = "left", trigger = "hover"),
      actionButton(ns("create_a_new_subset"), "Create a new subset from the selected class...", icon("plus-square-o", class = NULL, lib = "font-awesome")),
      bsTooltip(id = ns("create_a_new_subset"), title = "Optional: click here to create a subset of the sample class, with some samples removed",
                placement = "left", trigger = "hover")
    )
  })
  
  observeEvent(input$create_a_new_subset, {
    isolate({
      if(!input_data_subsets_panel_visible$l) {
        
        colnames_s <- extract_colnames_for_subset(t1_f$a, input$sample_class_from_selectinput)
        if(length(colnames_s) > 1) {
          input_data_subsets_panel_visible$l <- TRUE
          updateActionButton(session, "create_a_new_subset", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
        } else {
          show_generic_error_message("Cannot create a subset from a class that has only one sample")
        }
      } else if (input_data_subsets_panel_visible$l) {
        input_data_subsets_panel_visible$l <- FALSE
        updateActionButton(session, "create_a_new_subset", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
    
  })
  
  observeEvent(input$sample_subset_from_selectinput, {
    isolate({
      selected_subset_f <- input$sample_subset_from_selectinput
      if(selected_subset_f == "None") {
        shinyjs::disable("view_list_of_samples_in_subset")
      } else {
        shinyjs::enable("view_list_of_samples_in_subset")
      }
    })
  })
  
  observeEvent(input$view_list_of_samples_in_subset, {
    isolate({
      selected_subset_f <- input$sample_subset_from_selectinput
      if(selected_subset_f != "None") {
        subsets_list_f <- t1_f$a@subsets$l
        for(item_f in subsets_list_f) {
          if(item_f@item_name == selected_subset_f) {
            concatenated_name_and_label <- item_f@item_name
            if(item_f@item_label != "") {
              concatenated_name_and_label <- paste(concatenated_name_and_label, " (", item_f@item_label, ")")
            }
            output_message_title_f <- paste("List of samples in class ", item_f@parent_class, ", subset ", concatenated_name_and_label, sep="")
            
            output_message_f <- vector_to_comma_separated_character(item_f@colnames)
            
            showModal(modalDialog(
              title = output_message_title_f,
              output_message_f)
            )
            
          }
        }
        
      }
    })
    
  })
  
  observeEvent(input$sample_class_from_selectinput, {
    isolate({
      refresh_subsets_selectinput()
      names_and_labels_f <- get_names_and_labels_of_subsets_in_a_sample_class(t1_f$a, input$sample_class_from_selectinput)
      if((length(names_and_labels_f$names) == 1) && (length(names_and_labels_f$labels) == 1) && (names_and_labels_f$names[[1]] == "None")) {
        shinyjs::disable("sample_subset_from_selectinput")
        shinyjs::disable("view_list_of_samples_in_subset")
      } else {
        shinyjs::enable("sample_subset_from_selectinput")
        shinyjs::enable("view_list_of_samples_in_subset")
      }
    })
    
  })
  
  
  extract_colnames_for_subset <- function(t_f, sample_class_from_selectinput_f) {
    class_labels_vector_f <- (t_f@class_labels[,2])
    indexes_f <- get_sorted_indexes(two_groups_test(), sample_class_from_selectinput_f, class_labels_vector_f)
    dat_f <- t_f@df[,2:ncol(t_f@df)]
    data_selected_f <- dat_f[1,indexes_f]
    colnames_f <- names(data_selected_f)
    return(colnames_f)
  }
  
  observe({
    if (input_data_subsets_panel_visible$l == TRUE) {
      isolate({
        # _If missing input, return to avoid error later in function
        if(is.null(input$sample_class_from_selectinput))
          return()

        colnames_s <- extract_colnames_for_subset(t1_f$a, input$sample_class_from_selectinput)
        
        output$make_subsets_of_input_data_panel <- renderUI({
          tagList( 
            helpText("Please choose the samples that will remain in the data subset:"),
            checkboxGroupInput(ns("new_subset_selected_columns"), "Choose columns", choices = colnames_s, selected = colnames_s),
            helpText("Preview of the data subset (showing only the first 20 rows):"),
            fluidRow(
              column(
                tableOutput(ns("data_subset_table")), width = 12)
            ),
            textInput(ns("data_subset_label"), "Descriptive label for the data subset (optional):", ""),
            actionButton(ns("create_new_subsets_entry"), "Create a new subset", icon("plus", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("create_new_subsets_entry"), title = "Click here to make a new subset from the selected samples",
                      placement = "left", trigger = "hover"),
            tags$hr()
          )
        })
        

        output$data_subset_table <- renderTable({
          if (!input_data_subsets_panel_visible$l)
            return()
          dat <- t1_f$a@df

          if(is.null(dat))
            return()

          row.names(dat) <- t1_f$a@df[,1]

          if (is.null(input$new_subset_selected_columns) || !(input$new_subset_selected_columns %in% names(dat)))
            return()
          dat <- dat[, input$new_subset_selected_columns, drop = FALSE]

          head(dat, 20)

        }, width = "100%", include.rownames=TRUE)

      })
      
    } else {
      isolate({
        output$make_subsets_of_input_data_panel <- renderUI({})
      })
      
    }
  })
  
  observeEvent(input$create_new_subsets_entry, {
    
    isolate({
      if (!input_data_subsets_panel_visible$l)
        return()
      user_choice_for_new_subset <- input$new_subset_selected_columns
      
      new_subset <- collect_new_subset_settings(user_choice_for_new_subset, input$sample_class_from_selectinput, input$data_subset_label)
      new_sub_item_name <- new_subset@item_name
      t1_f$a@subsets$l <<- append(t1_f$a@subsets$l, list(new_subset))
      input_data_subsets_panel_visible$l <- FALSE
      input_data_subsets_counter$a <- input_data_subsets_counter$a + 1
      
      
      refresh_subsets_selectinput()
      shinyjs::enable("sample_subset_from_selectinput")
      shinyjs::enable("view_list_of_samples_in_subset")
    })
  })
  
  collect_new_subset_settings <- function(colnames_f, sample_class_f, data_subset_label_f) {
    # _Takes information on a new data subset that is stored in various separate variables and collects it in one variable that belongs to the class input_data_subset
    parent_class_f <- sample_class_f
    subset_name_f <- paste("Subset_", toString(input_data_subsets_counter$a), "_", parent_class_f, sep="")
    new_input_data_subset_f <- input_data_subset(parent_class = parent_class_f, colnames = colnames_f, item_name = subset_name_f, item_label = data_subset_label_f)
    return(new_input_data_subset_f)
  }
  
  get_names_and_labels_of_subsets_in_a_sample_class <- function(t1_f2_a, parent_class_select_f) {
    # _Gets the list of names and labels of subsets that belong to sample class
    names_list_f <- list()
    names_list_f$names <- list("None")
    names_list_f$labels <- list("None")
    for (subs_item in t1_f2_a@subsets$l) {
      if(subs_item@parent_class == parent_class_select_f) {
        if(subs_item@item_label == "" ) {
          names_list_f$labels <- append(names_list_f$labels, list(subs_item@item_name))
        } else {
          new_label_f <- paste(subs_item@item_name, " (", subs_item@item_label, ")", sep = "")
          names_list_f$labels <- append(names_list_f$labels, new_label_f)
        }
        names_list_f$names <- append(names_list_f$names, list(subs_item@item_name))
        
      }
    }
    return(names_list_f)
  }
  
  refresh_subsets_selectinput <- function() {
    # _Updates the choice of subsets in the UI page subsets selection selectinput after new subsets have been defined
    names_and_labels <- get_names_and_labels_of_subsets_in_a_sample_class(t1_f$a, input$sample_class_from_selectinput)
    names_for_selectinput_f <- unlist(names_and_labels$names)
    names(names_for_selectinput_f) <- unlist(names_and_labels$labels)
    
    last_name_f <- names_for_selectinput_f[length(names_for_selectinput_f)]
    updateSelectInput(session, "sample_subset_from_selectinput",
                      choices = names_for_selectinput_f,
                      selected = last_name_f
    )
  }
  
  
  
  # _end of the subsets part
  # _-----------------------------------------------------------------------------------------------------------------------
  
  
  output$running_the_test_container <- renderUI({
    tagList(
      selectInput(ns("stat_test_selectinput"), "Statistical test:", stat_test_selectinput_choices$l),
      actionButton(ns("proceed_to_selected_test_button"), "Proceed to the selected statistical test", icon("chevron-right", class = NULL, lib = "font-awesome")),
      bsTooltip(id = ns("proceed_to_selected_test_button"), title = "Click here to proceed to the selected statistical test", placement = "left", trigger = "hover")
    )
  })
  
  observeEvent(input$proceed_to_selected_test_button, {
    isolate({
      output_button$a <- input$stat_test_selectinput
      output_button$counter <- output_button$counter + 1
      output_list$a <- list(q1@l, output_button$a, output_button$counter)
    })
  })
  

  return(output_reactive)
}
  
