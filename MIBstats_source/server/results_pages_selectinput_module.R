# _The selectinput module for test results page
# _Used on rank sum and rank product results pages. The code of this app can be further optimised by using this module on other test results pages as well

results_pages_selectinput_moduleUI <- function(id) {
  
  ns <- NS(id)

  tagList(
    uiOutput(ns("two_groups_test_results_page_title")),
    uiOutput(ns("two_groups_test_results_selectinput"))
  )
}

results_pages_selectinput_module <- function(input, output, session, data_rtr2, data_rtr_sel2) {

  ns <- session$ns
  
  two_group_test_results_title <- function(data_rtr_sel_f) {
    # _Gets the name of the statistical test by the results tracker (rtr) index
    two_group_test_results_title_f <- ""
    data_rtr_names <- c("Shapiro-Wilk test", "t-test", "F-test", "Fold changes", "Rank product", "Rank sum")
    two_group_test_results_title_f <- data_rtr_names[data_rtr_sel_f]
    return(two_group_test_results_title_f)
  }

  test_title <- paste(two_group_test_results_title(data_rtr_sel2), " results", sep="")
  
  observed_answer_f <- reactiveValues(a = NULL)
  
  rtr_m <- reactiveValues(a = NULL)
  
  observe({
    rtr_m$a <- data_rtr2()
    isolate({
      if(!is.null(rtr_m$a[[data_rtr_sel2]])) {
        observed_answer_f$a <- rtr_m$a[[data_rtr_sel2]]@results_pages$l
      }
    })
  })
  
  rtr_selectin <- reactiveValues(a = NULL)
  
  two_groups_test_result_r <- reactiveValues(a = NULL)
  two_groups_test_result_r_a_reactive <- reactive({two_groups_test_result_r$a})

  selected_test_index <- reactiveValues(a = 1)
  selected_test <- reactiveValues(a = NULL)
  selected_test_reactive <- reactive({selected_test$a})

  reload_test_results_by_selectinput_choice <- function(data_rtr, data_rtr_sel) {
    # _Loads the set of test results that is selected by the user in two_groups_test_results_pages_selectinput

    update_results_page_selection <- function(curr_selection_f, rtr_results_pages_f) {
      new_curr_sel_index_a <- which(rtr_results_pages_f == curr_selection_f)
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
  }
  
  observeEvent(input$two_groups_test_results_pages_selectinput, {
    isolate({
      reload_test_results_by_selectinput_choice(rtr_m$a, data_rtr_sel2)
    })
  })

  output$two_groups_test_results_page_title = renderUI({
    tags$h3(test_title)
  })
  
  output$two_groups_test_results_selectinput = renderUI({
    selectInput(ns("two_groups_test_results_pages_selectinput"), "Pages of results:", choices = observed_answer_f$a)
  })
  
  return(two_groups_test_result_r_a_reactive)
}