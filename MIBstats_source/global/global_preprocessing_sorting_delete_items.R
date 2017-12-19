# _Module for deleting samples or variables, used in preprocessing (sorting page)

preprocessing_sorting_delete_itemsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("sorting_remove_items_general_container"))
  )
}

preprocessing_sorting_delete_items <- function(input, output, session, pp_sorter1_a_reactive, outer_name1) {
  
  ns <- session$ns
  
  outer_name1 <- paste(outer_name1, "-", sep="")
  
  pp_sorter1m <- reactiveValues(a = NULL)
  
  deletion_counter <- reactiveValues(a = 0)
  deletion_counter_old <- reactiveValues(a = 0)
  output_reactivevalue <- reactiveValues(a = NULL)
  
  observe({
    pp_sorter1_a <- pp_sorter1_a_reactive()
    if(!is.null(pp_sorter1_a)) {
      isolate({
        pp_sorter1m$a <- pp_sorter1_a
      })
    }
  })
  
  
  get_item_names <- function(item_mode_f, pp_sorter1m_a) {
    # _Gets the names of samples or variables from the sorter object
    item_names_f <- NULL
    if(item_mode_f == "samples") {
      item_names_f <- sorter_get_sample_names(pp_sorter1m_a)
    } else if (item_mode_f == "variables") {
      item_names_f <- sorter_get_variable_names(pp_sorter1m_a)
    }
    return(item_names_f)
  }
  
  number_of_items_after_deletion <- function(item_mode_f, pp_sorter1m_a) {
    # _Counts the number of samples or variables remaining after deletion
    number_of_items_to_be_removed <- length(input$sorting_remove_items_groupinput)
    number_of_items_in_table <- length(get_item_names(item_mode_f, pp_sorter1m_a))
    items_remaining_f <- number_of_items_in_table-number_of_items_to_be_removed
    return(items_remaining_f)
  }
  
  item_mode <- NULL
  
  if(session$ns("name") == paste(outer_name1, "delete_samples-name", sep="")) {
    item_mode <- "samples"
    
    
  } else if(session$ns("name") == paste(outer_name1, "delete_variables-name", sep="")) {
    item_mode <- "variables"
    
  }
  
  
  observe({
    if(deletion_counter$a > deletion_counter_old$a) {
      isolate({
        output_reactivevalue$a <- list(pp_sorter1m$a)
        deletion_counter_old$a <- deletion_counter_old$a + 1
      })
    }
  })
  
  delete_items_out <- reactive({
    validate(need(output_reactivevalue$a, message = FALSE))
    output_reactivevalue$a
  })
  
  output$sorting_remove_items_general_container <- renderUI({
    wellPanel(
      uiOutput(ns("sorting_remove_items_groupinput_container")),
      actionButton(ns("sorting_remove_items_button"), "Preview: remove the selected items"),
      bsTooltip(id = ns("sorting_remove_items_button"), title = "Click here to remove the selected items from the previewed data", placement = "left", trigger = "hover"),
      actionButton(ns("sorting_remove_items_select_all_button"), "Select all"),
      bsTooltip(id = ns("sorting_remove_items_select_all_button"), title = "Click here to select all items", placement = "left", trigger = "hover"),
      actionButton(ns("sorting_remove_items_select_none_button"), "Select none"),
      bsTooltip(id = ns("sorting_remove_items_select_none_button"), title = "Click here to deselect all items", placement = "left", trigger = "hover")
    )
  })
  
  output$sorting_remove_items_groupinput_container <- renderUI({
    tagList(
      checkboxGroupInput(ns("sorting_remove_items_groupinput"), "Choose items", choices = get_item_names(item_mode, pp_sorter1m$a), selected = pp_sorter1m$a@panel_tracking$selected_items, inline=TRUE)
    )
  })

  observeEvent(input$sorting_remove_items_select_all_button, {
    isolate({
      updateCheckboxGroupInput(session, "sorting_remove_items_groupinput",
                               selected = get_item_names(item_mode, pp_sorter1m$a)
      )
    })
  })
  
  observeEvent(input$sorting_remove_items_select_none_button, {
    isolate({
      updateCheckboxGroupInput(session, "sorting_remove_items_groupinput",
                               selected = character(0)
      )
    })
  })
  
  observeEvent(input$sorting_remove_items_button, {
    isolate({
      
      number_of_items_to_be_removed <- length(input$sorting_remove_items_groupinput)
      if(!is.null(input$sorting_remove_items_groupinput) & length(number_of_items_to_be_removed)>0) {
        items_remaining <- number_of_items_after_deletion(item_mode, pp_sorter1m$a)
        if(items_remaining >= 2) {
          if(item_mode == "samples") {
            pp_sorter1m$a <- preproc_remove_samples(pp_sorter1m$a, input$sorting_remove_items_groupinput)
          } else if (item_mode == "variables") {
            pp_sorter1m$a@t@df <- preproc_remove_variables(pp_sorter1m$a, input$sorting_remove_items_groupinput)
          }
          deletion_counter$a <- deletion_counter$a + 1
        } else {
          show_generic_error_message(paste("The number of ", item_mode, " cannot be reduced to less than 2", sep=""))
        }
      }
      
    })
  })
  

  return(delete_items_out)
}