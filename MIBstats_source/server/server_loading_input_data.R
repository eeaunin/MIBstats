# _This part of the server code is run when initialising the app
# _This is the server part of the app, located outside the modules, that calls the data loading module.
# _After the data has been loaded, the next modules are called for preprocessing and statistical tests.
# _The code in the shared variable space of the server here serves as a hub that deals with the input and output of modules in the app

data_loading_module_output <- callModule(data_loading_module, "loading_input_data")

file_loading_counter <- reactiveValues(a = 0)
viewing_loaded_data_module_output <- reactiveValues(a = NULL)
viewing_loaded_data_counter <- reactiveValues(preprocessing = 0, test_setup = 0)
pca2_visible <- reactiveValues(a = FALSE)
pca2_visible_reactive <- reactive({pca2_visible$a})

volcano_plot_pre <- reactiveValues(pval_tests_count = 0, fc_tests_count = 0)

na_zero_inf_module_output <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
sorting_module_output <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)

filtering_module_output <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
snv_module_output <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)
viewing_preprocessed_data_module_output <- reactiveValues(a = NULL, counter = 0, skip_counter = 0)

setting_up_two_groups_test_module_output <- reactiveValues(a = NULL)
output_button_2gt <- reactiveValues(a = NULL, counter = 0)

iga_module_output <- reactiveValues(a = NULL)
iga_module_output <- callModule(running_iga_module, "iga1", reactive({rtr$a}), "iga")
callModule(iga_results_module, "iga1_results", reactive({rtr$a}), "iga")

db_iga_module_output <- callModule(running_iga_module, "db_iga1", reactive({rtr$a}), "db-iga")
callModule(iga_results_module, "db_iga1_results", reactive({rtr$a}), "db-iga")

test_trigger_module_output <- reactiveValues(a = list(t_test = list(val = NULL, counter = 0), mann_whitney_u_test = list(val = NULL, counter = 0), f_test = list(val = NULL, counter = 0), shapiro_test = list(val = NULL, counter = 0), fold_changes = list(val = NULL, counter = 0), rank_product = list(val = NULL, counter = 0), rank_sum = list(val = NULL, counter = 0)))

server_queue_counter <- reactiveValues(a = 0) #_ This variable keeps track of how many times a new test queue has been set up
server_queue_counter_reactive <- reactive({server_queue_counter$a})

rtr_sel_to_module_output_tracker_name <- function(rtr_sel_f) {
  # _function for converting a results tracker (rtr) index to statistical test name
  local_dictionary <- c("shapiro_test" = 1, "t_test" = 2, "f_test" = 3, "fold_changes" = 4, "rank_product" = 5, "rank_sum" = 6, "mann_whitney_u_test" = 9)
  dict_name_index <- which(local_dictionary == rtr_sel_f)
  dict_name <- names(local_dictionary)[dict_name_index]
  return(dict_name)
}

process_test_triggering_module_output <- function(module_output_f, rtr_sel_f, results_module_name_f, tab_name_f) {
  # _Function that takes the output of the module of triggering statistical tests and sends it to the modules of viewing test results
  if(length(module_output_f)>0) {
    
    module_var_name <- rtr_sel_to_module_output_tracker_name(rtr_sel_f)
    module_var <- test_trigger_module_output$a[[module_var_name]]
    
    if(module_output_f[[2]] > module_var$counter) {
      
      new_rtr <<- module_output_f[[1]]
      
      test_trigger_module_output$a[[module_var_name]]$counter <<- test_trigger_module_output$a[[module_var_name]]$counter + 1
      
      server_queue_counter$a <- server_queue_counter$a + 1
      
      if(is.null(rtr$a[[rtr_sel_f]]@results$l)) {
        rtr$a[[rtr_sel_f]] <<- new_rtr[[rtr_sel_f]]

        
        if(rtr_sel_f == 1) {
          callModule(viewing_shapiro_test_results_module, results_module_name_f, reactive({rtr$a}), reactive({results_decimal_places$a}))
        }
      } else {
        list_length <- length(rtr$a[[rtr_sel_f]]@results$l)

        rtr_local <- rtr$a
        
        counter <- 1
        for(item in new_rtr[[rtr_sel_f]]@results$l) {
          rtr_local[[rtr_sel_f]]@results$l[list_length+counter] <- item
          
          item2 <- new_rtr[[rtr_sel_f]]@results_pages$l[counter]
          rtr_local[[rtr_sel_f]]@results_pages$l[list_length+counter] <- item2
          counter <- counter + 1
        }
        
        rtr$a <<- rtr_local
        
      }

      if(rtr_sel_f == 4) {
        callModule(two_groups_fold_changes_results_page, results_module_name_f, rtr$a, rtr_sel_f, results_decimal_places$a)
      } else if (rtr_sel_f == 5) {
        rank_product_selectinput_result <- callModule(results_pages_selectinput_module, "rank_product_selectinput", reactive({rtr$a}), rtr_sel_f)
        callModule(rank_product_module, results_module_name_f, reactive({rtr$a}), rtr_sel_f, results_decimal_places$a, rank_product_selectinput_result, TRUE)
      } else if (rtr_sel_f == 6) {
        rank_sum_selectinput_result <- callModule(results_pages_selectinput_module, "rank_sum_selectinput", reactive({rtr$a}), rtr_sel_f)
        callModule(rank_product_module, results_module_name_f, reactive({rtr$a}), rtr_sel_f, results_decimal_places$a, rank_sum_selectinput_result, FALSE)
      } else if (is.element(rtr_sel_f, c(2, 3, 9))) {
        callModule(two_groups_test_results_page, results_module_name_f, rtr$a, rtr_sel_f, results_decimal_places$a)
      }
      
      if(is.element(rtr_sel_f, c(1, 2, 3, 5, 6, 9))) {
        volcano_plot_pre$pval_tests_count <- volcano_plot_pre$pval_tests_count + 1
      }
      if(rtr_sel_f == 4) {
        volcano_plot_pre$fc_tests_count <- volcano_plot_pre$fc_tests_count + 1
      }
      
      updateTabsetPanel(session, "navbar", tab_name_f)
      js$enableTab(tab_name_f)
    }
  }
}

observe({
  # _Volcano plot tab is enabled if a fold changes test and a p-value yielding test have been performed
  if(volcano_plot_pre$pval_tests_count > 0 & volcano_plot_pre$fc_tests_count > 0) {
    isolate({
      js$enableTab("tab_volcano_plot")
    })
  }
})

observe({
  # _If a t-test was performed
  if(!is.null(test_trigger_module_output$a$t_test$val)) {
    t_test_module_output_observed <- test_trigger_module_output$a$t_test$val()
    isolate({
      process_test_triggering_module_output(t_test_module_output_observed, 2, "tgrp_t_test", "tab_view_t_test_results")
    })
  }
})

observe({
  # _If an f-test was performed
  if(!is.null(test_trigger_module_output$a$f_test$val)) {
    f_test_module_output_observed <- test_trigger_module_output$a$f_test$val()
    isolate({
      process_test_triggering_module_output(f_test_module_output_observed, 3, "tgrp_f_test", "tab_view_f_test_results")
    })
  }
})

observe({
  # _If a Shapiro-Wilk test was performed
  if(!is.null(test_trigger_module_output$a$shapiro_test$val)) {
    shapiro_test_module_output_observed <- test_trigger_module_output$a$shapiro_test$val()
    isolate({
      process_test_triggering_module_output(shapiro_test_module_output_observed, 1, "tgrp_shapiro_test", "tab_view_shapiro_wilk_test_results")
    })
  }
})

observe({
  # _If fold changes were calculated
  if(!is.null(test_trigger_module_output$a$fold_changes$val)) {
    fold_changes_module_output_observed <- test_trigger_module_output$a$fold_changes$val()
    isolate({
      process_test_triggering_module_output(fold_changes_module_output_observed, 4, "tgrp_fold_changes", "tab_view_fold_changes_results")
    })
  }
})

observe({
  # _If rank product was calculated
  if(!is.null(test_trigger_module_output$a$rank_product$val)) {
    rank_product_module_output_observed <- test_trigger_module_output$a$rank_product$val()
    isolate({
      process_test_triggering_module_output(rank_product_module_output_observed, 5, "rank_product_results1", "tab_view_rank_product_results")
    })
  }
})

observe({
  # _If rank sum was calculated
  if(!is.null(test_trigger_module_output$a$rank_sum$val)) {
    rank_sum_module_output_observed <- test_trigger_module_output$a$rank_sum$val()
    isolate({
      process_test_triggering_module_output(rank_sum_module_output_observed, 6, "rank_sum_results1", "tab_view_rank_sum_results")
    })
  }
})

observe({
  # _If Mann-Whitney U test was done
  if(!is.null(test_trigger_module_output$a$mann_whitney_u_test$val)) {
    mann_whitney_test_module_output_observed <- test_trigger_module_output$a$mann_whitney_u_test$val()
    isolate({
      process_test_triggering_module_output(mann_whitney_test_module_output_observed, 9, "tgrp_mann_whitney_test", "tab_view_mann_whitney_test_results")
    })
  }
})


observe({
  # _output from the iGA module
  iga_module_output_observed <- iga_module_output()
  isolate({
    if(!is.null(iga_module_output_observed)) {
      rtr$a[["iga_tracking"]]@results$l <- append(rtr$a[["iga_tracking"]]@results$l, list(iga_module_output_observed))
      
      js$enableTab("tab_view_iga_results")
      updateTabsetPanel(session, "navbar", "tab_view_iga_results")
    }
  })
})

observe({
  # _output from the db-iGA module
  db_iga_module_output_observed <- db_iga_module_output()
  isolate({
    if(!is.null(db_iga_module_output_observed)) {
      rtr$a[["db_iga_tracking"]]@results$l <- append(rtr$a[["db_iga_tracking"]]@results$l, list(db_iga_module_output_observed))
      
      js$enableTab("tab_view_db_iga_results")
      updateTabsetPanel(session, "navbar", "tab_view_db_iga_results")
    }
  })
})




observe({
  # _Observer that deals with the output of the data loading module. Preprocessing and statistical test modules are initialised here once data has been loaded to the app
  data_loading_list <- data_loading_module_output()
  if(!is.null(data_loading_list)) {
    isolate({
      idc1_a <- data_loading_list[[1]]
      if(!is.null(idc1_a@mydata_csv)) {
        t1$a@df <- idc1_a@mydata_csv
        if(!is.null(idc1_a@class_labels_csv)) {
          t1$a@class_labels <- idc1_a@class_labels_csv
        }
        if(!is.null(idc1_a@log_transformed)) {
          t1$a@log_transformed <- idc1_a@log_transformed
          t1$a@log_base <- idc1_a@log_base
        }
        updated_summary <- update_input_data_summary(t1$a)
        t1$a@table_statistics <- updated_summary$data_summary
        
        
        if(!is.null(data_loading_list[[2]]) & data_loading_list[[2]]>file_loading_counter$a) {
          
          
          
          test_module_names <- c("t_test", "mann_whitney_u_test", "f_test", "shapiro_test", "fold_changes", "rank_product", "rank_sum")
          test_type_vector <- c("t-test", "mann-whitney_u_test", "f-test", "shapiro_test", "fold_changes", "rank_product", "rank_sum")
          
          num_modules1 <- 8
          num_modules2 <- length(test_module_names)
          
          n <- num_modules1 + num_modules2
          
          withProgress(message = "Loading the data to app modules", value = 0, {
            
            
            for(module_i in (1:num_modules1)) {
              if(module_i == 1) {
                viewing_loaded_data_module_output$a <- callModule(viewing_loaded_data_module, "input_data", reactive({t1$a}))
              } else if(module_i == 2) {
                na_zero_inf_module_output$a <- callModule(handling_na_zero_inf_values_module, "handling_na_zero_inf", reactive({t1$a}))
              } else if(module_i == 3) {
                source(file.path("server", "server_calling_pca_modules.R"), local = TRUE)$value
              } else if(module_i == 4) {
                sorting_module_output$a <- callModule(preprocessing_sorting_module, "sorting1", reactive({t1$a}), "sorting1")
              } else if(module_i == 5) {
                filtering_module_output$a <- callModule(preprocessing_filtering_module, "filtering1", reactive({t1$a}))
              } else if(module_i == 6) {
                snv_module_output$a <- callModule(preprocessing_snv_module, "snv1", reactive({t1$a}))
              } else if(module_i == 7) {
                viewing_preprocessed_data_module_output$a <- callModule(viewing_preprocessed_data_module, "viewing_preprocessed_data1", reactive({t1$a}))
              } else if(module_i == 8) {
                setting_up_two_groups_test_module_output$a <- callModule(setting_up_two_groups_comparison_module, "setting_up_two_groups_test1", reactive({t1$a}))
              }
              incProgress(1/n, detail = paste("Doing part", module_i))
            }

            for(test_i in (1:length(test_module_names))) {
              current_test_module_name <- test_module_names[test_i]
              current_test_type <- test_type_vector[test_i]
              test_trigger_module_output$a[[current_test_module_name]]$val <- callModule(triggering_statistical_test_module, paste0(current_test_module_name, "1"), reactive({server_queue1$a}), reactive({t1$a}), reactive({rtr$a}), server_queue_counter_reactive, current_test_type)
              incProgress(1/n, detail = paste("Doing part", num_modules1 + test_i))
            }
          })
          
          js$enableTab("tab_view_loaded_data")
          updateTabsetPanel(session, "navbar", "tab_view_loaded_data")
          js$enableTab("tab_choose_data_for_comparing_two_groups")
          
          file_loading_counter$a < file_loading_counter$a + 1
        }
      }
    })
  }
})


observe({
  pca2_visible_state <- pca2_visible_reactive()
  isolate({
    if(!is.null(pca2_visible_state) && pca2_visible_state==TRUE) {
      output$viewing_preprocessed_data_pca2_uioutput <- renderUI ({
        tagList(
          preproc_pca_moduleUI("pca_module2")
        )
      })
    } else {
      output$viewing_preprocessed_data_pca2_uioutput <- renderUI ({})
    }
  })
})

observeEvent(input$view_preprocessed_data_pca_button, {
  isolate({
    pca2_visible$a <- !pca2_visible$a
    if(pca2_visible$a) {
      updateActionButton(session, "view_preprocessed_data_pca_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
    } else {
      updateActionButton(session, "view_preprocessed_data_pca_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    }
  })
})


observe({
  # _Output from the viewing of loaded data module
  if(!is.null(viewing_loaded_data_module_output$a)) {
    viewing_loaded_data_module_observed <- viewing_loaded_data_module_output$a()
    isolate({
      if(!is.null(viewing_loaded_data_module_observed) && (viewing_loaded_data_module_observed[[1]] > viewing_loaded_data_counter$preprocessing)) {
        js$enableTab("tab_handling_values")
        updateTabsetPanel(session, "navbar", "tab_handling_values")
        viewing_loaded_data_counter$preprocessing <- viewing_loaded_data_counter$preprocessing + 1
      } else if(!is.null(viewing_loaded_data_module_observed) && (viewing_loaded_data_module_observed[[2]] > viewing_loaded_data_counter$test_setup)) {
        js$enableTab("tab_choose_data_for_comparing_two_groups")
        updateTabsetPanel(session, "navbar", "tab_choose_data_for_comparing_two_groups")
        viewing_loaded_data_counter$test_setup <- viewing_loaded_data_counter$test_setup + 1
      }
    })
  }
})

observe({
  # _Output from the handling of NA, zero and Inf values module
  if(!is.null(na_zero_inf_module_output$a)) {
    na_zero_inf_observed <- na_zero_inf_module_output$a()
    isolate({
      if(!is.null(na_zero_inf_observed[[3]])) {
        # _skip_counter
        if(na_zero_inf_observed[[3]] > na_zero_inf_module_output$skip_counter) {
          js$enableTab("tab_pca")
          updateTabsetPanel(session, "navbar", "tab_pca")
          na_zero_inf_module_output$skip_counter <- na_zero_inf_module_output$skip_counter + 1
        }
      }
    })
    if(!is.null(na_zero_inf_observed[[2]]) & (na_zero_inf_observed[[1]] > na_zero_inf_module_output$counter)) {
      isolate({
        t1$a <- na_zero_inf_observed[[2]]
        js$enableTab("tab_pca")
        updateTabsetPanel(session, "navbar", "tab_pca")
        na_zero_inf_module_output$counter <- na_zero_inf_module_output$counter + 1
        
        pca_tracking[["pca2"]]$trigger_counter$a <- as.numeric(pca_tracking[["pca2"]]$trigger_counter$a) + 1
      })
    }
  }
})

observe({
  # _Output from the sorting module
  if(!is.null(sorting_module_output$a)) {
    sorter_observed <- sorting_module_output$a()
    isolate({
      if(!is.null(sorter_observed[[3]])) {
        # _skip_counter
        if(sorter_observed[[3]] > sorting_module_output$skip_counter) {
          js$enableTab("tab_filtering")
          updateTabsetPanel(session, "navbar", "tab_filtering")
          sorting_module_output$skip_counter <- sorting_module_output$skip_counter + 1
        }
      }
    })
    if(!is.null(sorter_observed[[2]]) & (sorter_observed[[1]] > sorting_module_output$counter)) {
      isolate({
        t1$a <- sorter_observed[[2]]
        js$enableTab("tab_filtering")
        updateTabsetPanel(session, "navbar", "tab_filtering")
        sorting_module_output$counter <- sorting_module_output$counter + 1
        pca_tracking[["pca2"]]$trigger_counter$a <- as.numeric(pca_tracking[["pca2"]]$trigger_counter$a) + 1
        
      })
    }
  }
})

observe({
  # _Output from the filtering module
  if(!is.null(filtering_module_output$a)) {
    filter_observed <- filtering_module_output$a()
    isolate({
      if(!is.null(filter_observed[[3]])) {
        # _skip_counter
        if(filter_observed[[3]] > filtering_module_output$skip_counter) {
          js$enableTab("tab_scaling_normalisation_etc")
          updateTabsetPanel(session, "navbar", "tab_scaling_normalisation_etc")
          filtering_module_output$skip_counter <- filtering_module_output$skip_counter + 1
        }
      }
    })
    if(!is.null(filter_observed[[2]]) & (filter_observed[[1]] > filtering_module_output$counter)) {
      isolate({
        t1$a <- filter_observed[[2]]
        js$enableTab("tab_scaling_normalisation_etc")
        updateTabsetPanel(session, "navbar", "tab_scaling_normalisation_etc")
        filtering_module_output$counter <- filtering_module_output$counter + 1
        pca_tracking[["pca2"]]$trigger_counter$a <- as.numeric(pca_tracking[["pca2"]]$trigger_counter$a) + 1
        
      })
    }
  }
})

observe({
  # _Output from the scaling, normalisation and variance stabilisation (snv) module
  if(!is.null(snv_module_output$a)) {
    snv_observed <- snv_module_output$a()
    isolate({
      if(!is.null(snv_observed[[3]])) {
        # _skip_counter
        if(snv_observed[[3]] > snv_module_output$skip_counter) {
          js$enableTab("tab_view_preprocessed_data")
          updateTabsetPanel(session, "navbar", "tab_view_preprocessed_data")
          snv_module_output$skip_counter <- snv_module_output$skip_counter + 1
          
        }
      }
    })
    if(!is.null(snv_observed[[2]]) & (snv_observed[[1]] > snv_module_output$counter)) {
      isolate({
        t1$a <- snv_observed[[2]]
        js$enableTab("tab_view_preprocessed_data")
        updateTabsetPanel(session, "navbar", "tab_view_preprocessed_data")
        snv_module_output$counter <- snv_module_output$counter + 1
        
        pca_tracking[["pca2"]]$trigger_counter$a <- as.numeric(pca_tracking[["pca2"]]$trigger_counter$a) + 1
        
      })
    }
  }
})

observe({
  # _Output from the module of viewing preprocessed data
  if(!is.null(viewing_preprocessed_data_module_output$a)) {
    vpd_observed <- viewing_preprocessed_data_module_output$a()
    isolate({
      if(!is.null(vpd_observed[[3]])) {
        # _skip_counter
        if(vpd_observed[[3]] > viewing_preprocessed_data_module_output$skip_counter) {
          t1$a <- unpreprocessed_t1$a
          message_out("Input data was reset to its initial state, as loaded from the input file (the changes to the data made in preprocessing were cancelled) ", "Preprocessing")
          updateTabsetPanel(session, "navbar", "tab_choose_data_for_comparing_two_groups")
          viewing_preprocessed_data_module_output$skip_counter <- viewing_preprocessed_data_module_output$skip_counter + 1
          pca_tracking[["pca2"]]$trigger_counter$a <- as.numeric(pca_tracking[["pca2"]]$trigger_counter$a) + 1
          
        }
      }
    })
    if(!is.null(vpd_observed[[2]]) & (vpd_observed[[1]] > viewing_preprocessed_data_module_output$counter)) {
      isolate({
        t1$a <- vpd_observed[[2]]
        updateTabsetPanel(session, "navbar", "tab_choose_data_for_comparing_two_groups")
        viewing_preprocessed_data_module_output$counter <- viewing_preprocessed_data_module_output$counter + 1
        
        pca_tracking[["pca2"]]$trigger_counter$a <- as.numeric(pca_tracking[["pca2"]]$trigger_counter$a) + 1
        
      })
    }
  }
})




observe({
  # _output from the module of setting up statistical tests
  if(!is.null(setting_up_two_groups_test_module_output$a)) {
    test_setup_output <- setting_up_two_groups_test_module_output$a()
    isolate({
      if(!is.null(test_setup_output)) {
        if(length(test_setup_output) >= 3) {
          server_queue1$a@l <- test_setup_output[[1]]
          if(test_setup_output[[2]] != "none") {
            
            if(test_setup_output[[3]] > output_button_2gt$counter) {
              
              output_button_2gt$a <- test_setup_output[[2]]
              
              output_button_2gt$counter <- output_button_2gt$counter + 1
              
              
              tabs_for_stat_tests <- c("shapiro_test" = "tab_shapiro_wilk_test", "f_test" = "tab_f_test", "t_test" = "tab_t_test", "mann_whitney_u_test" = "tab_mann_whitney_u_test", "fold_changes" = "tab_fold_changes", "rank_product" = "tab_rank_product", "rank_sum" = "tab_rank_sum")
              
              if(is.element(output_button_2gt$a, names(tabs_for_stat_tests))) {
                new_tab <- as.character(tabs_for_stat_tests[output_button_2gt$a])
                updateTabsetPanel(session, "navbar", new_tab)
              }

              
            }
          }
        }
      }
      
    })
  }
  
  
})







