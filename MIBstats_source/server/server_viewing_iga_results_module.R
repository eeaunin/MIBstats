# _This is the server part that deals with viewing iGA results

library(reshape)
library(ggplot2)

iga_results_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("iga_results_page_title_container")),
    uiOutput(ns("test_results_selectinput_container")),
    uiOutput(ns("summary_table_container")),
    tags$hr(),
    actionButton(ns("view_metrics_table_button"), "View iGA metrics table", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("view_metrics_table_button"), title = "Click here to view iGA metrics table", placement = "left", trigger = "hover"),
    uiOutput(ns("metrics_table_container")),
    actionButton(ns("view_heatmap_button"), "View iGA heat map", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
    bsTooltip(id = ns("view_heatmap_button"), title = "Click here to view a heat map of iGA results", placement = "left", trigger = "hover"),
    uiOutput(ns("iga_heatmap_container")),
    uiOutput(ns("iga_results_help_container"))
  )
}

iga_results_module <- function(input, output, session, rtr_reactive_f, iga_mode_f) {
  ns <- session$ns
  
  plot_file_format <- reactiveValues(a = "svg")
  
  plot_file_format_reactive <- callModule(image_format_selection_module, "iga_image_format")
  
  rtr_f <- reactiveValues(a = NULL)
  iga_results_list <- reactiveValues(a = NULL)
  test_results_selectinput_choices <- reactiveValues(l = NULL)
  selected_test_ind <- reactiveValues(a = NULL)
  selected_test_ind_reactive <- reactive({selected_test_ind$a})
  iga_summary_data_frame <- reactiveValues(a = NULL)
  iga_summary_data_frame_reactive <- reactive({iga_summary_data_frame$a})
  metrics_frame <- reactiveValues(a = NULL)
  iga_metrics_frame_reactive <- reactive({metrics_frame$a})
  annot_classes_vector <- reactiveValues(a = NULL)
  table_hidden_columns <- reactiveValues(a = NULL)
  data_for_downloading <- reactiveValues(a = NULL)
  panel_tracking <- reactiveValues(metrics_table_visible = FALSE, heatmap_visible = FALSE)
  data_for_heatmap <- reactiveValues(a = NULL)
  data_for_heatmap_reactive <- reactive({data_for_heatmap$a})
  heat_map_plot <- reactiveValues(a = NULL)
  heat_map_plot_reactive <- reactive({heat_map_plot$a})
  
  observe({
    plot_file_format_observed <- plot_file_format_reactive()
    isolate({
      if(!is.null(plot_file_format_observed)) {
        plot_file_format$a <- plot_file_format_observed
      }
    })
  })
  
  iga_summary_colnames <- NULL
  if(iga_mode_f=="iga") {
    iga_summary_colnames <- c("Minimum PC value", "List position", "Number of variables selected", "Group size")
  } else if (iga_mode_f == "db-iga") {
    iga_summary_colnames <- c("Minimum PC value", "List start position", "List end position", "Number of variables selected", "Group size")
  }
  
  page_title <- "iGA results"
  if(iga_mode_f=="db-iga") {
    page_title <- "db-iGA results"
  }
  
  help_module_name <- "tr_help_iga"
  if(iga_mode_f=="db-iga") {
    help_module_name <- "tr_help_db_iga"
  }
  
  output$iga_results_help_container <- renderUI({
    help_box_moduleUI(ns(help_module_name))
  })
  
  callModule(help_box_module, help_module_name)
  
  output$iga_results_page_title_container <- renderUI ({
    tagList(
      tags$h3(page_title)
    )
  })
  
  observe({
    rtr_f$a <- rtr_reactive_f()
    isolate({
      if(iga_mode_f == "iga") {
        iga_results_list$a <- rtr_f$a[["iga_tracking"]]@results$l
      } else if (iga_mode_f == "db-iga") {
        iga_results_list$a <- rtr_f$a[["db_iga_tracking"]]@results$l
      }
    })
  })
  
  observeEvent(input$view_metrics_table_button, {
    isolate({
      panel_tracking$metrics_table_visible <- !panel_tracking$metrics_table_visible
      if(panel_tracking$metrics_table_visible) {
        updateActionButton(session, "view_metrics_table_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "view_metrics_table_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$view_heatmap_button, {
    isolate({
      panel_tracking$heatmap_visible <- !panel_tracking$heatmap_visible
      if(panel_tracking$heatmap_visible) {
        updateActionButton(session, "view_heatmap_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else {
        updateActionButton(session, "view_heatmap_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  generate_table_for_heat_map <- function(iga_input_data_annot, summary_frame) {
    # _0: does not belong to the class and is not in the region of interest
    # _1: belongs to the class and is not in the region of interest
    # _3: does not belong to the class and is in the region of interest
    # _4: belongs to the class and is in the region of interest
    heatmap_data <- iga_input_data_annot
    
    
    for(sf_row_ind in 1:nrow(summary_frame)) {
      start_pos <- NULL
      end_pos <- NULL
      
      if(iga_mode_f == "iga") {
        start_pos <- 1
        end_pos <- summary_frame[sf_row_ind, 2]
      } else if (iga_mode_f == "db-iga") {
        start_pos <- summary_frame[sf_row_ind, 2]
        end_pos <- summary_frame[sf_row_ind, 3]
      }
      
      for(binary_mat_row_ind in 1:nrow(iga_input_data_annot)) {
        if(binary_mat_row_ind >= start_pos & binary_mat_row_ind <= end_pos) {
          # _If the value is in the region of interest
          binary_data_value <- heatmap_data[binary_mat_row_ind, sf_row_ind]
          heatmap_data_value <- binary_data_value + 2
          heatmap_data[binary_mat_row_ind, sf_row_ind] <- heatmap_data_value
        }
        
      }
    }
    old_colnames <- colnames(heatmap_data)
    heatmap_data <- cbind(rownames(heatmap_data), heatmap_data)
    colnames(heatmap_data) <- c("Variable", old_colnames)
    heatmap_data <- as.data.frame(heatmap_data)
    
    heatmap_data$Variable <- as.character(heatmap_data$Variable)
    heatmap_data$Variable <- factor(heatmap_data$Variable, levels=unique(heatmap_data$Variable))
    
    return(heatmap_data)
  }
  
  observe({
    if(panel_tracking$heatmap_visible) {
      isolate({
        output$iga_heatmap_container <- renderUI ({
          tagList(
            tags$h4("Heat map of iGA results"),
            plotOutput(ns("iga_heatmap"), width="auto", height="auto"),
            downloadButton(ns("download_iga_plot"), "Download the iGA plot"),
            image_format_selection_moduleUI(ns("iga_image_format"))
          )
        })
      })
    } else {
      output$iga_heatmap_container <- renderUI ({})
    }
  })
  
  get_heat_map_dimensions <- function(heat_map_data_f) {
    # _plot dimensions:
    # _h 25 px per variable
    # _w 175 px per class
    
    heat_map_size_w <- (ncol(heat_map_data_f)-1)*175
    heat_map_size_h <- nrow(heat_map_data_f)*25
    return(c(heat_map_size_w, heat_map_size_h))
  }
  
  generate_iga_heat_map <- function(heat_map_data_f) {
    # _Function for generating the iGA plot
    melt.data<-melt(heat_map_data_f, id.vars="Variable", variable_name="Class")
    
    
    plot_colour_palette <- c("0"="gray98", "1"="skyblue2", "2"="gray65", "3"="navyblue")
    
    heat_map_plot <- reactive({qplot(data=melt.data,
                              x=Class,
                              y=Variable,
                              fill=factor(value),
                              geom="tile") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(name = "Properties of the variables", labels = c("0"="Not in the class. Not in iGA's selected variables", "1"="In the class. Not in iGA's selected variables", "2"="Not in the class. In iGA's selected variables", "3"="In the class. In iGA's selected variables"), values=plot_colour_palette) + 
      scale_y_discrete(limits = rev(levels(melt.data$Variable)))
      })
    
    return(heat_map_plot)
  }
  
  observe({
    data_for_heatmap_observed <- data_for_heatmap_reactive()
    isolate({
      if(!is.null(data_for_heatmap_observed)) {
        heat_map_reactive <- generate_iga_heat_map(data_for_heatmap_observed)
        hm_dimensions <- get_heat_map_dimensions(data_for_heatmap_observed)
        
        output$iga_heatmap <- renderPlot({
          heat_map_reactive()
        }, width = hm_dimensions[1], height = hm_dimensions[2])
        
        heat_map_plot$a <- heat_map_reactive()
      }
    })
  })

  observe({
    selected_test_ind_observed <- selected_test_ind_reactive()
    isolate({
      if(!is.null(selected_test_ind_observed)) {
        iga_summary_data_frame$a <- as.data.frame(iga_results_list$a[[selected_test_ind_observed]]@iga$results$summary)
        
        data_for_downloading$a <- iga_summary_data_frame$a
        
        colnames(data_for_downloading$a) = iga_summary_colnames
        
        
        iga_input_data_pvals <- iga_results_list$a[[selected_test_ind_observed]]@iga$input_data[[1]]
        iga_input_data_annot <- iga_results_list$a[[selected_test_ind_observed]]@iga$input_data[[2]]
        
        
        metrics <- cbind(iga_input_data_annot[,1], iga_input_data_pvals, iga_input_data_annot)
        rownames(metrics) <- rownames(iga_input_data_annot)
        annot_classes_vector$a <- colnames(iga_input_data_annot)
        colnames(metrics) <- c("selected_class","Metric", colnames(iga_input_data_annot))
        
        metrics_sorted <- metrics[order(metrics[,2], decreasing = FALSE),]
        metrics_sorted_cropped <- metrics_sorted[,-c(1,2)]
        data_for_heatmap$a <- generate_table_for_heat_map(metrics_sorted_cropped, iga_summary_data_frame$a)
        
        metrics_frame$a <- as.data.frame(metrics)
        
      }
    })
  })
  
  observe({
    if(!is.null(iga_results_list$a)) {
      isolate({
        list_for_selectinput <- list()
        for(results_item in iga_results_list$a) {
          list_for_selectinput <- append(list_for_selectinput, list(results_item@iga$test_name))
        }
        test_results_selectinput_choices$l <- list_for_selectinput
      })
    }
  })
  
  observe({
    if(!is.null(test_results_selectinput_choices$l)) {
      isolate({
        output$test_results_selectinput_container <- renderUI ({
          tagList(
            selectInput(ns("test_results_selectinput"), "Available test results:", choices = test_results_selectinput_choices$l)
          )
        })
      })
    } else {
      isolate({
        output$test_results_selectinput_container <- renderUI ({})
      })
    }
  })
  
  observeEvent(input$test_results_selectinput, {
    isolate({
      if(!is.null(test_results_selectinput_choices$l)) {
        selected_test_ind$a <- which(test_results_selectinput_choices$l == input$test_results_selectinput)
      }
    })
  })
  
  output$summary_table_container <- renderUI ({
    tagList(
      tags$h4("Summary table"),
      fluidRow(
        column(12,
               dataTableOutput(ns("iga_results_summary_table"))
        )
      ),
      downloadButton(ns("download_test_results"), "Download the summary table")
    )
  })
  
  observe({
    if(panel_tracking$metrics_table_visible) {
      isolate({
        output$metrics_table_container <- renderUI ({
          tagList(
            tags$h4("iGA metrics table"),
            fluidRow(
              column(12,
                     dataTableOutput(ns("iga_results_metrics_table"))
              )
            ),
            selectInput(ns("metrics_frame_selectinput"), "Class of annotations that is highlighted in the table:", choices = annot_classes_vector$a)
          )
        })
      })
    } else {
      isolate({
        output$metrics_table_container <- renderUI ({})
      })
    }
  })
  
  observeEvent(input$metrics_frame_selectinput, {
    isolate({
      selected_ind <- which(annot_classes_vector$a == input$metrics_frame_selectinput) + 2
      if(!is.null(selected_ind)) {
        metrics_frame$a[,1] <- metrics_frame$a[,selected_ind]
      }
    })
  })
  
  observe({
    iga_summary_data_frame_observed <- iga_summary_data_frame_reactive()
    isolate({
      if(!is.null(iga_summary_data_frame_observed)) {
        iga_results_table <- DT::datatable(iga_summary_data_frame_observed, colnames = iga_summary_colnames, rownames = FALSE, selection = 'none') 
        output$iga_results_summary_table <- renderDataTable({iga_results_table})
      }
    })
    
  })
  
  observe({
    iga_metrics_frame_observed <- iga_metrics_frame_reactive()
    isolate({
      if(!is.null(iga_metrics_frame_observed)) {
        
        iga_metrics_frame_sorted <- iga_metrics_frame_observed[order(iga_metrics_frame_observed$Metric),] 
        table_hidden_columns2 <- c(1, 3:ncol(iga_metrics_frame_sorted))
        iga_metrics_table <- DT::datatable(iga_metrics_frame_sorted, options = list(rowCallback = JS('
                                                  function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                                  if (parseFloat(aData[1]) > 0)
                                                  $("td:eq(1)", nRow).css("background-color", "#9BF59B");
                                                  }'), columnDefs = list(list(visible=FALSE, targets=table_hidden_columns2))), rownames = TRUE, selection = 'none') 
        output$iga_results_metrics_table <- renderDataTable({iga_metrics_table})
      }
    })
    
  })
  
  output$download_test_results <- downloadHandler(
    filename = function() {
      paste( Sys.Date(), "-data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_for_downloading$a, file)
    }
  )
  
  output$download_iga_plot <- downloadHandler(
    filename = function() { paste(Sys.Date(), "_iGA_heat_map", ".", plot_file_format$a, sep="") },
    content = function(file) {
      ggsave(file, heat_map_plot_reactive(), device = plot_file_format$a)
    }
  )

}