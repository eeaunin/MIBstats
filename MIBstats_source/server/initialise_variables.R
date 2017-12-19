# _Initialisation of variables in the shared namespace of the server that is done when the app is started

# _t1$a is the variable in which the input data table is stored in the shared namespace of the server

t1 <- reactiveValues(a = NULL)
t1$a <- input_data_table()

unpreprocessed_t1 <- reactiveValues(a = NULL)
unpreprocessed_t1$a <- input_data_table()

server_queue1 <- reactiveValues(a = NULL)
server_queue1$a <- two_groups_test_queue(l = list())


pca_tracking <- list()
pca_result_tracker <- reactiveValues(a = NULL)

initialise_pca_objects <- function() {
  # _Initialises PCA objects and some more variables for PCA
  pp_pca1 <<- reactiveValues(a = NULL)
  pp_pca1$a <<- preprocessing_pca()
  pp_pca2 <<- reactiveValues(a = NULL)
  pp_pca2$a <<- preprocessing_pca()

}

initialise_pca_tracking_variables <- function(select_f, extended_opt_f) {
  # _Initialises variables that are used for the input and output of PCA modules
  pca_tracking[[select_f]] <<- list()
  
  pca_tracking[[select_f]]$module_called <<- reactiveValues(a = FALSE)
  pca_tracking[[select_f]]$result <<- reactiveValues(l = NULL)
  
  pca_tracking[[select_f]]$input <<- reactiveValues(a = NULL)
  
  pca_tracking[[select_f]]$trigger_counter <<- reactiveValues(a = 1)
  pca_tracking[[select_f]]$trigger_counter_reactive <<- reactive({pca_tracking[[select_f]]$trigger_counter$a})
  
  pca_tracking[[select_f]]$reset_counter <<- reactiveValues(a = 1)
  pca_tracking[[select_f]]$reset_counter_reactive <<- reactive({pca_tracking[[select_f]]$reset_counter$a})
  
  pca_tracking[[select_f]]$output_counter_old <<- reactiveValues(a = 0)
  pca_tracking[[select_f]]$extended_options <<- reactiveValues(a = extended_opt_f)
}


initialise_pca_objects()

initialise_pca_tracking_variables("pca1", TRUE)
initialise_pca_tracking_variables("pca2", FALSE)

pca_tracking[["pca1"]]$input$a <- NULL
pca_tracking[["pca1"]]$input_reactive <- reactive({t1$a})
pca_tracking[["pca2"]]$input$a <- NULL
pca_tracking[["pca2"]]$input_reactive <- reactive({t1$a})







empty_tracking_t <- two_groups_test_results_tracker()
empty_tracking_t@test_type <- ""
empty_tracking_t@results <- list()
empty_tracking_t@results_pages <- list()
empty_tracking_t@curr_sel_index <- list()
empty_tracking_t@p_values_answer <- list()
empty_tracking_t@plot_val <- list()

rtr <- reactiveValues(a = list(shapiro_tracking = empty_tracking_t, t_test_tracking = empty_tracking_t, f_test_tracking = empty_tracking_t, fold_changes_tracking = empty_tracking_t, rank_product_tracking = empty_tracking_t, rank_sum_tracking = empty_tracking_t, iga_tracking = empty_tracking_t, db_iga_tracking = empty_tracking_t, mann_whitney_tracking = empty_tracking_t))
# _The order in which the rtr elements are defined is used to distinguish the results of different statistical tests from one another
# _shapiro: item 1
# _t test: item 2
# _f test: item 3
# _fold changes: item 4
# _rank product: item 5
# _rank sum: item 6
# _iga: item 7
# _db_iga: item 8
# _mann_whitney u test: item 9

callModule(volcano_plot_module, "two_groups_test_volcano_plot", reactive({rtr$a}))



rtr_sel <- 0



results_decimal_places <- reactiveValues(a = 2)





