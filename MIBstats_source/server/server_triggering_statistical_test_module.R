# _Statistical tests are triggered here

triggering_statistical_test_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("test_title_container")),
    uiOutput(ns("welch_checkbox_container")),
    uiOutput(ns("alternative_hypothesis_choice_container")),
    uiOutput(ns("max_allowed_na_percent_slider_container")),
    uiOutput(ns("statistical_test_button_container")),
    uiOutput(ns("help_button_container"))
  )
}

triggering_statistical_test_module <- function(input, output, session, server_queue_reactive_f, t1_reactive_f, rtr_reactive_f, server_queue_counter_reactive_f, mode_f) {
  ns <- session$ns
  
  help_module_name <- paste("tr_help_", gsub("-", "_", mode_f, fixed = TRUE), sep="")
  callModule(help_box_module, help_module_name)
  
  
  server_queue_f <- reactiveValues(a = NULL)
  t1_f <- reactiveValues(a = NULL)
  rtr_f <- reactiveValues(a = NULL, new = NULL)
  test_counter <- reactiveValues(a = 0)
  
  queue_counter <- reactiveValues(a = 0)
  
  test_name <- get_full_name_of_a_two_groups_test(mode_f)
  
  #server_queue_counter_reactive_f
  
  observe({
    server_queue_reactive_observed <- server_queue_reactive_f()
    if(!is.null(server_queue_reactive_observed)) {
      isolate({
        server_queue_f$a <- server_queue_reactive_observed
      })
    }
  })
  
  observe({
    queue_counter_observed <- server_queue_counter_reactive_f()
    if(!is.null(queue_counter_observed)) {
      isolate({
        queue_counter$a <- queue_counter_observed
        server_queue_f$a@test_queue_serial_nr <- queue_counter$a
      })
    }
  })
  
  observe({
    t1_reactive_observed <- t1_reactive_f()
    if(!is.null(t1_reactive_observed)) {
      isolate({
        t1_f$a <- t1_reactive_observed
      })
    }
  })
  
  observe({
    rtr_reactive_observed <- rtr_reactive_f()
    if(!is.null(rtr_reactive_observed)) {
      isolate({
        rtr_f$a <- rtr_reactive_observed
      })
    }
  })
  
  if(mode_f=="t-test") {
    output$welch_checkbox_container <- renderUI({
      tagList(
        checkboxInput(ns("var_equal_false_checkbox"), "Use Welch's correction", TRUE)
      )
    })
  }
  
  if(mode_f=="t-test" | mode_f == "mann-whitney_u_test") {
    output$alternative_hypothesis_choice_container <- renderUI({
      tagList(
        radioButtons(ns("alternative_hypothesis_radiobutton"), "Two-tailed or one-tailed test:",
                     c("Two-tailed" = "alternative_hypothesis_two_sided",
                       "One-tailed: alternative hypothesis = 'less'" = "alternative_hypothesis_less",
                       "One-tailed: alternative hypothesis = 'greater'" = "alternative_hypothesis_greater"))
      )
    })
    
    output$max_allowed_na_percent_slider_container <- renderUI({
      tagList(
        sliderInput(ns("max_allowed_na_percent_in_two_groups_test"), "Maximum % of allowed NA values in input data:",
                    min = 0, max = 100, value = 25
        )
      )
    })
  }

  
  run_test_button_name <- NULL
  run_test_button_tooltip_text <- NULL
  if(mode_f == "fold_changes" | mode_f == "rank_product" | mode_f == "rank_sum") {
    run_test_button_name <- paste("Calculate the ", tolower(test_name), sep="")
    run_test_button_tooltip_text <- paste("Click here to calculate the ", tolower(test_name), sep="")
  } else {
    run_test_button_name <- paste("Run the ", test_name, " with the selected groups", sep="")
    run_test_button_tooltip_text <- paste("Click here to run the ", test_name, sep="")
  }
  
  output$statistical_test_button_container <- renderUI({
    tagList(
      actionButton(ns("run_statistical_test"), run_test_button_name),
      bsTooltip(id = ns("run_statistical_test"), title = run_test_button_tooltip_text,
                placement = "left", trigger = "hover")
    )
  })
  
  output$test_title_container <- renderUI({
    tagList(
      tags$h3(test_name)
    )
  })
  
  output$help_button_container <- renderUI({
    tagList(
      help_box_moduleUI(ns(help_module_name))
    )
  })
  
  mode_to_rtr_sel <- function(mode_f2) {
    #_ Gets the results tracker (rtr) index by the name of a test
    # _shapiro: item 1
    # _t test: item 2
    # _f test: item 3
    # _fold changes: item 4
    # _rank product: item 5
    # _rank sum: item 6
    # _iga: item 7
    # _db_iga: item 8
    # _mann_whitney u test: item 9
    mode_names <- c("shapiro_test", "t-test", "f-test", "fold_changes", "rank_product", "rank_sum", "iga", "db_iga", "mann-whitney_u_test")
    current_rtr_sel <- which(mode_names == mode_f2)
    return(current_rtr_sel)
  }
  
  trigger_statistical_test <- function(mode_f2) {
    # _Function for triggering statistical tests (deals with the two_groups_test_queue class objects)
    additional <- list()
    max_na_allowed_percent <- 100
    if(mode_f2 == "t-test") {
      additional$welch <- input$var_equal_false_checkbox
    }
    if(mode_f2 == "t-test" | mode_f2 == "mann-whitney_u_test") {
      additional$alternative_hypothesis <- input$alternative_hypothesis_radiobutton
      max_na_allowed_percent <- input$max_allowed_na_percent_in_two_groups_test/100
    }
    
    new_rtr_item_main_f <- NULL
    
    new_rtr_item_main_f <- run_two_groups_test(server_queue_f$a, t1_f$a, mode_f2, additional, max_na_allowed_percent)
    return(new_rtr_item_main_f)
  }

  observeEvent(input$run_statistical_test, {
    isolate({
      new_rtr_item_main <- trigger_statistical_test(mode_f)
      if(!is.null(new_rtr_item_main)) {
        rtr_f$a[[mode_to_rtr_sel(mode_f)]] <- new_rtr_item_main
        rtr_f$new <- rtr_f$a
        test_counter$a <- test_counter$a + 1
      }
    })
  })

  out_reactive <- reactive({list(rtr_f$new, test_counter$a)})
  return(out_reactive)
}

