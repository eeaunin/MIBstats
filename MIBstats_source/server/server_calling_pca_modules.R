# _The server part of code that calls PCA modules and gets output from the modules. This file is run after loading input data (from "server_loading_input_data.R")

call_pca_module <- function(select_f) {
  if(!pca_tracking[[select_f]]$module_called$a) {
    if(select_f=="pca1") {
      pca_tracking[[select_f]]$result$l <<- callModule(preproc_pca_module, "pca_module1", pca_tracking[[select_f]]$input_reactive, pca_tracking[[select_f]]$trigger_counter_reactive, pca_tracking[[select_f]]$extended_options$a, pca_tracking[[select_f]]$reset_counter_reactive, t1$a)
    } else if(select_f=="pca2") {
      pca_tracking[[select_f]]$result$l <<- callModule(preproc_pca_module, "pca_module2", pca_tracking[[select_f]]$input_reactive, pca_tracking[[select_f]]$trigger_counter_reactive, pca_tracking[[select_f]]$extended_options$a, pca_tracking[[select_f]]$reset_counter_reactive, t1$a)
    }
    pca_tracking[[select_f]]$module_called$a <- TRUE
  }
  
  pca_tracking[[select_f]]$output_counter_old <<- reactiveValues(a = 0)
}


call_pca_module("pca1")
call_pca_module("pca2")

unpreprocessed_t1$a <- t1$a

# _Setting up the updating of the "pca2" instance of the PCA plot module
observe({
  pca_tracking[["pca2"]]$input_reactive()
  isolate({
    pca_tracking[["pca2"]]$trigger_counter$a <- pca_tracking[["pca2"]]$trigger_counter$a + 1
  })
})

pca_go_to_next_tab <- function(select_f) {
  if(select_f == "pca1") {
    js$enableTab("tab_sorting")
    updateTabsetPanel(session, "navbar", "tab_sorting")
  }
}

observeEvent(input$skip_pca_button, {
  isolate({
    js$enableTab("tab_sorting")
    updateTabsetPanel(session, "navbar", "tab_sorting")
  })
})

process_pca_module_response <- function(pca1_response) {
  if(is.list(pca1_response) & length(pca1_response) > 0) {
    if(pca1_response$output_counter > pca_tracking$pca1$output_counter_old$a) {
      if (pca1_response$changes_applied) {
        t1$a@df <<- pca1_response$df
        t1$a@class_labels <<- pca1_response$class_labels_frame
        
        showModal(modalDialog(
          title = "PCA",
          "Changes from the PCA step of preprocessing were applied to the input data")
        )
        pca_go_to_next_tab("pca1")
      }
      else if (!pca1_response$changes_applied) {
        pca_go_to_next_tab("pca1")
      }
      pca_tracking$pca1$output_counter_old$a <<- pca1_response$output_counter
    }
  }
}

observe({
  pca1_response_observed <- pca_tracking$pca1$result$l()
  isolate({
    process_pca_module_response(pca1_response_observed)
  })
})