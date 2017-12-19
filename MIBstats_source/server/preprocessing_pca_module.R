# _Module for PCA

library(plotly)
library(cowplot)
library(ggplot2)

preproc_pca_moduleUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$h3("Principal component analysis"),
    textOutput(ns("PCA_text1")),
    textOutput(ns("pca_helptext")),
    uiOutput(ns("initial_run_pca_button_container")),
    uiOutput(ns("initial_skip_pca_button_container")),
    uiOutput(ns("pca_plotly_plots")),
    uiOutput(ns("pca_scores_selectinput_container")),
    uiOutput(ns("pca_extended_options_container")),
    help_box_moduleUI(ns("help_pca"))
    
  )
}


preproc_pca_module <- function(input, output, session, pp_pca1_input_reactive_f, trigger_reactive_f, extended_options_f, reset_counter_f, t1_initial_f) {
  # _The server part of the preprocessing step that uses PCA
  ns <- session$ns
  
  plot_file_format <- reactiveValues(a = "svg")
  
  initial_skip_pca_button_visible <- reactiveValues(a = TRUE)
  if(!extended_options_f) {
    initial_skip_pca_button_visible$a <- FALSE
  }
  
  observe({
    if(initial_skip_pca_button_visible$a) {
      isolate({
        output$initial_skip_pca_button_container <- renderUI({
          tagList(
            actionButton(ns("skip_pca_button"), "Skip PCA and go to the next step of preprocessing", style = paste0("color: ", skip_button_color)),
            bsTooltip(id = "skip_pca_button", title = "Click here to skip PCA and go to the next step of preprocessing", placement = "left", trigger = "hover")
          )
        })
      })
      
    } else {
      isolate({
        output$initial_skip_pca_button_container <- renderUI({})
      })
    }
  })
  
  
  
  plot_file_format_reactive <- callModule(image_format_selection_module, "pca_test_image_format")
  
  observe({
    plot_file_format_observed <- plot_file_format_reactive()
    isolate({
      if(!is.null(plot_file_format_observed)) {
        plot_file_format$a <- plot_file_format_observed
      }
    })
  })
  
  callModule(help_box_module, "help_pca")
  callModule(help_box_module, "help_kmeans")
  

  remove_inf_values_pca <- function(df_f_data) {
    # _Replaces infinite values with NA in the PCA preview data, because the PCA function does not accept infinite values
    replacement_f1 <- function(x){replace(x, x == -Inf, NA)}
    replacement_f2 <- function(x){replace(x, x == Inf, NA)}
    replacement_f3 <- function(x){replace(x, is.nan(x), NA)}
    df_f_data <- as.data.frame(lapply(df_f_data, replacement_f1))
    df_f_data <- as.data.frame(lapply(df_f_data, replacement_f2))
    df_f_data <- as.data.frame(lapply(df_f_data, replacement_f3))
    return(df_f_data)
  }
  
  t1_initial_f@df <- remove_inf_values_pca(t1_initial_f@df)
  t1_initial_f@df <- na.omit(t1_initial_f@df)
  
  extended_options <- reactiveValues(a = extended_options_f)
  current_session_name <- session$ns("name")
  current_session_name <- as.character(strsplit(current_session_name, "-name"))
  
  
  old_trigger_counter <- reactiveValues(a = 0)
  old_reset_counter <- reactiveValues(a = 0)


  t1_input <- reactiveValues(a = NULL)
  
  react <- reactiveValues(trigger_counter = NULL, reset_counter = NULL)
  
  pca_module_output <- reactiveValues(l = list())
  
  pca_output_counter <- reactiveValues(a = 0)
  

  
  
  initialise_pca_preview <- function(t1_f2) {
    # _Initialises the preprocessing_pca object
    pca1 <<- reactiveValues(a = NULL)
    pca1$a <<- preprocessing_pca()
    pca1$a@t <<- t1_f2
    pca_plot_pc_choice <<- reactiveValues(scores_pc1 = "PC1", scores_pc2 = "PC2", loadings_pc1 = "PC1", loadings_pc2 = "PC2")
    pca1$a@s <<- list()
    pca1$a <<- preproc_initialise_class_labels_vector(pca1$a, t1_f2)
  }
  
  initialise_pca_preview(t1_initial_f)
  
  observe({
    react$trigger_counter <- trigger_reactive_f()

    
    if(react$trigger_counter > old_trigger_counter$a) {
      isolate({
      
        if(!is.null(pp_pca1_input_reactive_f())) {
          t1_input$a <- pp_pca1_input_reactive_f()
          t1_input$a@df <- remove_inf_values_pca(t1_input$a@df)
          t1_input$a@df <- na.omit(t1_input$a@df)

          reset_pca_preview(t1_input$a)
          initialise_pca_reactive_values()

          respond_to_pca_click()

        }
      })
    }
  })
  
  
  
  observe({
    react$reset_counter <- reset_counter_f()
    isolate({
      if(react$reset_counter > old_reset_counter$a) {
        old_reset_counter$a <- react$reset_counter
        if(!is.null(pp_pca1_input_reactive_f())) {
          t1_input$a <- t1_initial_f

          reset_pca_reactive_values()
          reset_pca_preview(t1_input$a)
        }
      }
    })
    
  })
 
  source("downloaded_functions/multiplot.R")
  

  reset_pca_preview <- function(t1_f2) {
    # _Resets the preprocessing_pca object to its initial state
    pca1$a <<- preprocessing_pca()
    pca1$a@t <<- t1_f2
    pca_plot_pc_choice$scores_pc1 <<- "PC1"
    pca_plot_pc_choice$scores_pc2 <<- "PC2"
    pca_plot_pc_choice$loadings_pc1 <<- "PC1"
    pca_plot_pc_choice$loadings_pc2 <<- "PC2"
    pca1$a@s <<- list()
    pca1$a <<- preproc_initialise_class_labels_vector(pca1$a, t1_f2)
  }
  
  reset_pca_reactive_values <- function() {
    # _Initialises reactive values that are used to track the program state in the PCA section
    t1_f <<- t1_input$a
    
    samples_colour_mode$a <<- NULL
    samples_colour_mode$a <<- "class_labels"
    class_labels_on_clustering_plot$a <<- TRUE
    clustering_tracker$kmeans_number_of_clusters <- 3
    panel_plot_components_count$a <<- 2
    components_selected_for_clustering$l <<- NULL
    components_selected_for_clustering$l <<- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15", "PC16", "PC17", "PC18", "PC19", "PC20")
    initial_run_pca_button_visible$a <<- TRUE
    sample_colours_selection_button_visible$a <<- FALSE
    pca_extra_plots_visible$scree <<- FALSE
    pca_extra_plots_visible$clustering <<- FALSE
    pca_extra_plots_visible$panel_plot <<- FALSE
    pca_extra_plots_visible$panel_plot_selectinput <<- FALSE
    pca_extra_plots_visible$clustering_settings <<- FALSE
    
    close_pca_results_view()
  }
  
  
  initialise_pca_reactive_values <- function() {
    # _Initialises reactive values that are used to track the program state in the PCA section

    t1_f <<- t1_input$a

    samples_colour_mode <<- reactiveValues(a = NULL)
    samples_colour_mode$a <<- "class_labels"
    class_labels_on_clustering_plot <<- reactiveValues(a = TRUE)
    clustering_tracker <<- reactiveValues(kmeans_number_of_clusters = 3)
    panel_plot_components_count <<- reactiveValues(a = 2)
    components_selected_for_clustering <<- reactiveValues(l = NULL)
    components_selected_for_clustering$l <<- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15", "PC16", "PC17", "PC18", "PC19", "PC20")
    initial_run_pca_button_visible <<- reactiveValues(a = TRUE)
    sample_colours_selection_button_visible <<- reactiveValues(a = FALSE)
    pca_extra_plots_visible <<- reactiveValues(scree = FALSE, clustering = FALSE, panel_plot = FALSE, panel_plot_selectinput = FALSE, clustering_settings = FALSE)
  }
  
  render_initial_pca_button <- function() {
    if(current_session_name == "pca_module1") {
      # _For the PCA that is done as a preprocessing step
      output$initial_run_pca_button_container <- renderUI({
        tagList(
          actionButton(ns("run_pca_button"), "Run PCA", style = paste0("color: ", next_button_color)),
          bsTooltip(id = ns("run_pca_button"), title = "Click here to run principal component analysis (PCA) on the data in order to detect and remove outliers", placement = "left", trigger = "hover")
        )
      })
    } else if (current_session_name == "pca_module2") {
      # _For the PCA that is used for viewing the data after preprocessing
      output$initial_run_pca_button_container <- renderUI({
        tagList(
          actionButton(ns("run_pca_button"), "Run PCA"),
          bsTooltip(id = ns("run_pca_button"), title = "Click here to run principal component analysis (PCA) on the data in order to detect and outliers", placement = "left", trigger = "hover")
        )
      })
    }
  }
  render_initial_pca_button()
  
  
  get_new_scores_plot <- function(colour_mode_f) {
    # _Generates the plotly scores plot of PCA
    colouring_scheme_f <- NULL
    
    
    if(colour_mode_f == "clusters") {
      
      colouring_scheme_f <- pca1$a@s$pca_scores_frame$cluster_name
      
      
    } else if(colour_mode_f == "class_labels") {
      colouring_scheme_f <- pca1$a@class_labels_vector
    }
    
    current_pc1_f <- unlist(pca1$a@s$pca_scores_frame[[pca_plot_pc_choice$scores_pc1]])
    current_pc2_f <- unlist(pca1$a@s$pca_scores_frame[[pca_plot_pc_choice$scores_pc2]])
    
    p <- plot_ly(pca1$a@s$pca_scores_frame, type = "scatter", x = current_pc1_f , y = current_pc2_f, text = rownames(pca1$a@s$pca_scores_frame),
                 mode = "markers", key = ~pca1$a@s$scores_frame_key, color = colouring_scheme_f, marker = list(size = 11), source = "pca_scores1")
    
    p <- layout(p, title = "PCA Scores",
                xaxis = list(title = pca_plot_pc_choice$scores_pc1),
                yaxis = list(title = pca_plot_pc_choice$scores_pc2), dragmode =  "select")
    return(p)
  }
  
  
  update_pca_scores_plot <- function(colour_mode_f) {
    # _Updates the plotly plot of PCA scores
    
    p <- get_new_scores_plot(colour_mode_f)
    
    output$pca_plot <- renderPlotly({
      p
    })
  }
  
  update_pca_loadings_plot <- function() {
    # _Updates the plotly plot of PCA loadings
    current_pc1_f <- unlist(pca1$a@s$pca_loadings_frame[[pca_plot_pc_choice$loadings_pc1]])
    current_pc2_f <- unlist(pca1$a@s$pca_loadings_frame[[pca_plot_pc_choice$loadings_pc2]])
    
    
    p2 <- plot_ly(pca1$a@s$pca_loadings_frame, type = "scatter", x = current_pc1_f , y = current_pc2_f, text = rownames(pca1$a@s$pca_loadings_frame),
                  mode = "markers", key = ~pca1$a@s$loadings_frame_key, marker = list(size = 11), source = "pca_loadings1")
    
    
    p2 <- layout(p2, title = "PCA Loadings",
                 xaxis = list(title = pca_plot_pc_choice$loadings_pc1),
                 yaxis = list(title = pca_plot_pc_choice$loadings_pc2), dragmode =  "select")
    
    
    output$pca_plot2 <- renderPlotly({
      p2
    })
  }
  
  close_pca_results_view <- function() {
    # _Closes all UI panels on the PCA page
    pca_extra_plots_visible$panel_plot_selectinput <<- FALSE
    pca_extra_plots_visible$panel_plot <<- FALSE
    initial_run_pca_button_visible$a <<- TRUE
    if(extended_options_f) {
      initial_skip_pca_button_visible$a <<- TRUE
    }
    output$pca_plotly_plots <- renderUI({})
    output$pca_scores_selectinput_container <- renderUI({})
    output$pca_scree_button_container <- renderUI({})
    output$pca_clustering_button_container <- renderUI({})
    output$pca_panel_plot_button_container <- renderUI({})
    output$pca_extended_options_container <- renderUI({})
    output$pca_scree_plot_container <- renderUI({})
    output$pca_clustering_plot_container <- renderUI({})
    output$panel_plots_selectinput_container <- renderUI({})
    output$pca_panel_plot_container <- renderUI({})
    output$pca_loadings_selected_items_container <- renderUI({})
    output$pca_scores_selected_items_container <- renderUI({})
    output$pca_scores_selected_items_summary <- renderText({})
    output$rerun_pca_button_container <- renderUI({})
    output$pca_submit_button_container <- renderUI({})
    output$pca_change_sample_colours_container <- renderUI({})
    output$pca_clustering_settings_container <- renderUI({})
    output$pca_clustering_settings_container2 <- renderUI({})
    render_initial_pca_button()
  }
  

  respond_to_pca_click <- function() {
    # _Function that is triggered when the user clicks any of the buttons that serve to (re-)run PCA
    isolate({
      
      initial_skip_pca_button_visible$a <<- FALSE
      
      pc_names <- reactiveValues(a = NULL)
      for(i in 1:20) {
        pc_names$a <- c(pc_names$a, paste("PC", as.character(i), sep=""))
      }
      
      pca_extra_plots_visible$panel_plot_selectinput <<- FALSE
      pca_extra_plots_visible$panel_plot <<- FALSE
      initial_run_pca_button_visible$a <<- FALSE
      
      output$initial_run_pca_button_container <- renderUI({})

      pca_output <- run_pca(pca1$a, clustering_tracker$kmeans_number_of_clusters, components_selected_for_clustering$l)
      pca1$a@s <<- pca_output
      
      cumulative_result <- get_pca_cumulative_result(pca1$a)
      
      if(current_session_name == "pca_module1") {
        output$pca_helptext <- renderText({
          "In this preprocessing step, PCA can be used to detect and remove samples or variables that are outliers in the data."
        })
        output$pca_helptext2 <- renderText({
          "<small>&emsp;Samples and variables can be selected by clicking and dragging.</small>"
        })
      } else if (current_session_name == "pca_module2") {
        output$pca_helptext <- renderText({
          "PCA can be used here to examine if any outliers have remained in the data"
        })
      }
      
      output$pca_plotly_plots <- renderUI({
        tagList(
          fluidRow(
            tags$br(),
            htmlOutput(ns("pca_helptext2")),
            tags$br(),
            column(6,
                   tagList(
                     plotlyOutput(ns("pca_plot")),
                     uiOutput(ns("pca_scores_selected_items_container"))
                   )
            ),
            column(6,
                   tagList(
                     plotlyOutput(ns("pca_plot2")),
                     uiOutput(ns("pca_loadings_selected_items_container"))
                   )
            )
          )
        )
      })
      
      output$pca_scree_button_container <- renderUI({
        tagList(
          actionButton(ns("show_pca_scree_plot"), "Show PCA scree plot", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
          bsTooltip(id = ns("show_pca_scree_plot"), title = "Click here to view a cumulative scree plot of the PCA results", placement = "left", trigger = "hover")
        )
      })
      
      output$pca_clustering_button_container <- renderUI({
        tagList(
          actionButton(ns("show_pca_kmeans_clustering_plot"), "Show PCA k-means clustering plot", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
          bsTooltip(id = ns("show_pca_kmeans_clustering_plot"), title = "Click here to view the result of k-means clustering of the samples", placement = "left", trigger = "hover")
        )
      })
      
      output$pca_panel_plot_button_container <- renderUI({
        tagList(
          actionButton(ns("show_panel_plots_button"), "Show a plot panel of multiple combinations of PCs", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
          bsTooltip(id = ns("show_panel_plots_button"), title = "Click here to view the PCA results as a plot panel of multiple combinations of PCs", placement = "left", trigger = "hover"),
          uiOutput(ns("panel_plots_selectinput_container"))
        )
      })
      
      if(extended_options$a) {
        output$pca_extended_options_container <- renderUI({
          tagList(
            uiOutput(ns("rerun_pca_button_container")),
            tags$hr(),
            uiOutput(ns("pca_change_sample_colours_container")),
            tags$hr(),
            image_format_selection_moduleUI(ns("pca_test_image_format")),
            tags$hr(),
            uiOutput(ns("pca_scree_button_container")),
            uiOutput(ns("pca_scree_plot_container")),
            tags$hr(),
            uiOutput(ns("pca_clustering_button_container")),
            uiOutput(ns("pca_clustering_plot_container")),
            uiOutput(ns("pca_clustering_settings_container")),
            tags$hr(),
            uiOutput(ns("pca_panel_plot_button_container")),
            tags$hr(),
            uiOutput(ns("pca_submit_button_container"))
          )
        })
      }
      
      observe({
        if (pca_extra_plots_visible$scree) {
          isolate({
            output$pca_scree_plot_container <- renderUI({
              wellPanel(
                tags$h4("PCA scree plot"),
                plotOutput(ns("ggplot2_pca_scree_plot")),
                downloadButton(ns("download_pca_scree_plot_button"), "Download the PCA scree plot"),
                bsTooltip(id = ns("download_pca_scree_plot_button"), title = "Click here to download the PCA scree plot", 
                          placement = "left", trigger = "hover")
              )
            })
          })
          
        } else {
          isolate({
            output$pca_scree_plot_container <- renderUI({})
          })
          
        }
      })
      
      observe({
        if (pca_extra_plots_visible$clustering) {
          isolate({
            output$pca_clustering_plot_container <- renderUI({
              wellPanel(
                tags$h4("PCA k-means clustering plot"),
                plotOutput(ns("pca_clustering_plot")),
                checkboxInput(ns("pca_clustering_plot_show_labels"), "Show sample labels", FALSE),
                downloadButton(ns("download_pca_clustering_plot_button"), "Download the PCA clustering plot"),
                bsTooltip(id = ns("download_pca_clustering_plot_button"), title = "Click here to download the PCA clustering plot", 
                          placement = "left", trigger = "hover"),
                help_box_moduleUI(ns("help_kmeans"))
              )
            })
          })
          
        } else {
          isolate({
            output$pca_clustering_plot_container <- renderUI({})
          })
        }
      })
      
      observe({
        if (pca_extra_plots_visible$panel_plot_selectinput) {
          isolate({
            output$panel_plots_selectinput_container <- renderUI({
              tagList(
                selectInput(ns("pca_panel_plot_selectinput"), "Number of principal components in the plot panel:", c(2:20)),
                actionButton(ns("update_panel_plots_button"), "Update the PCA plot panel"),
                bsTooltip(id = ns("update_panel_plots_button"), title = "Click here to update the plot panel of the PCA results", placement = "left", trigger = "hover"),
                uiOutput(ns("pca_panel_plot_container"))
              )
            })
          })
          
        } else {
          isolate({
            output$panel_plots_selectinput_container <- renderUI({})
          })
        }
      })
      
      
      observe({
        if (pca_extra_plots_visible$panel_plot) {
          isolate({
            output$pca_panel_plot_container <- renderUI({
              tagList(
                tags$h4("Panel of PCA result plots"),
                plotOutput(ns("pca_panel_plot"), width="auto", height="auto"),
                plotOutput(ns("pca_panel_plot_legend"), width="auto", height="auto"),
                downloadButton(ns("download_pca_panel_plot_button"), "Download the plot"),
                bsTooltip(id = ns("download_pca_panel_plot_button"), title = "Click here to download the plot", 
                          placement = "left", trigger = "hover"),
                downloadButton(ns("download_pca_panel_plot_legend_button"), "Download the plot legend"),
                bsTooltip(id = ns("download_pca_panel_plot_legend_button"), title = "Click here to download the plot legend", 
                          placement = "left", trigger = "hover")
              )
            })
          })
          
        } else {
          isolate({
            output$pca_panel_plot_container <- renderUI({})
          })
        }
      })
      
      cumulative_result_frame <- data.frame(cbind(1:length(cumulative_result)), as.numeric(cumulative_result))
      colnames(cumulative_result_frame) <- c("principal_component", "cumulative_variance")
      cumulative_result_frame$principal_component <- as.factor(cumulative_result_frame$principal_component)
      rownames(cumulative_result_frame) <- names(cumulative_result)
      
      pca_scree_plot_reactive <- reactive({ggplot(cumulative_result_frame, aes(x=principal_component, y=cumulative_variance, group=1)) + geom_line() + geom_point() +
          xlab("Principal component") + ylab("Cumulative percentage of variance explained") + 
          ggtitle("PCA cumulative variance scree plot")})
      
      output$ggplot2_pca_scree_plot <- renderPlot({
        pca_scree_plot_reactive()
      })
      
      output$download_pca_scree_plot_button <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_PCA_scree_plot", ".", plot_file_format$a, sep="") },
        content = function(file) {
          ggsave(file, pca_scree_plot_reactive(), device = plot_file_format$a)
        }
      )

      
      output$download_pca_clustering_plot_button <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_PCA_clustering_plot", ".", plot_file_format$a, sep="") },
        content = function(file) {
          ggsave(file, pca_clustering_plot_reactive(), device = plot_file_format$a)
        }
      )
      
      panel_plot_reactive <- reactive({get_panel_plot(pca1$a, panel_plot_components_count$a, samples_colour_mode$a)})
      
      output$download_pca_panel_plot_button <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_PCA_plot_panel", ".", plot_file_format$a, sep="") },
        content = function(file) {
          ggsave(file, panel_plot_reactive(), device = plot_file_format$a)
        }
      )
      
      panel_plot_legend_reactive <- reactive({get_panel_plot_legend(pca1$a, panel_plot_components_count$a, samples_colour_mode$a)})
      
      output$download_pca_panel_plot_legend_button <- downloadHandler(
        filename = function() { paste(Sys.Date(), "_PCA_plot_panel_legend", ".", plot_file_format$a, sep="") },
        content = function(file) {
          ggsave(file, panel_plot_legend_reactive(), device = plot_file_format$a)
        }
      )
      
      pca_clustering_plot_reactive <- reactive({autoplot(pca1$a@s$pca_hc, data = pca1$a@s$pca_result$x,
                                                         label = class_labels_on_clustering_plot$a, label.size = 3, frame = TRUE)})
      
      output$pca_clustering_plot <- renderPlot({
        pca_clustering_plot_reactive()
      })
      
      
      update_pca_scores_plot(samples_colour_mode$a)
      update_pca_loadings_plot()
      
      update_pca_scores_selected_items_summary <- function() {
        # _Function to update the list of selected samples displayed under the plotly PCA scores plot
        summary_f <- "<b>Selected samples</b><br>"
        d <- event_data(event = "plotly_selected", source = "pca_scores1")
        if (is.null(d)) {
          summary_f <- paste(summary_f, "None", sep="")
        }
        else {
          selected_items_string <- paste(toString(d$key), sep="")
          summary_f <- paste(summary_f, selected_items_string, sep="")
        }
        return(summary_f)
      }
      
      update_pca_loadings_selected_items_summary <- function() {
        # _Function to update the list of selected variables displayed under the plotly PCA loadings plot
        summary_f <- "<b>Selected variables</b><br>"
        d2 <- event_data(event = "plotly_selected", source = "pca_loadings1")
        if (is.null(d2)) {
          summary_f <- paste(summary_f, "None", sep="")
        }
        else {
          selected_items_string <- paste(toString(d2$key), sep="")
          summary_f <- paste(summary_f, selected_items_string, sep="")
        }
        return(summary_f)
      }
      
      
      output$pca_loadings_selected_items_container <- renderUI({
        tagList(
          htmlOutput(ns("pca_loadings_selected_items_summary"))
        )
      })
      
      output$pca_scores_selected_items_container <- renderUI({
        tagList(
          htmlOutput(ns("pca_scores_selected_items_summary"))
        )
      })
      
      output$pca_scores_selected_items_summary <- renderText({
        update_pca_scores_selected_items_summary()
      })
      
      
      output$pca_loadings_selected_items_summary <- renderText({
        update_pca_loadings_selected_items_summary()
      })
      
      
      output$data_loading_summary_table_f2 <- renderTable(data_summary_frame_class_label_statistics_f, rownames = TRUE, selection = "none")
      
      
      output$rerun_pca_button_container <- renderUI({
        tagList(
          actionButton(ns("run_pca_button2"), "Preview: remove the samples/variables marked for deletion and rerun PCA"),
          bsTooltip(id = ns("run_pca_button2"), title = "Click here to remove the selected samples/variables from the previewed data and rerun PCA", placement = "left", trigger = "hover"),
          actionButton(ns("pca_reset_changes"), "Preview: reset the changes to samples and variables"),
          bsTooltip(id = ns("pca_reset_changes"), title = "Click here to undo the deletion of samples/variables from the preview data", placement = "left", trigger = "hover")
        )
      })
      
      output$pca_submit_button_container <- renderUI({
        tagList(
          actionButton(ns("pca_apply_changes"), "Apply the changes and move to the next step of preprocessing"),
          bsTooltip(id = ns("pca_apply_changes"), title = "Click here to apply the deletion of samples/variables to the data and proceed to the next step of preprocessing", placement = "left", trigger = "hover"),
          actionButton(ns("pca_move_to_next_step_without_changes"), "Go to the next step of preprocessing without making changes to the data"),
          bsTooltip(id = ns("pca_move_to_next_step_without_changes"), title = "Click here to go to the next step of preprocessing without making changes to the data", placement = "left", trigger = "hover")
        )
      })
      
      output$pca_change_sample_colours_container <- renderUI({
        wellPanel(
          actionButton(ns("show_sample_colours_panel_button"), "Change the mode of colouring samples...", icon("wrench", class = NULL, lib = "font-awesome"), placement = "left", trigger = "hover"),
          bsTooltip(id = ns("show_sample_colours_panel_button"), title = "Click here to change whether the colouring of samples is based on class labels or k-means clustering", placement = "left", trigger = "hover"),
          uiOutput(ns("pca_change_sample_colours_container2"))
        )
      })
      
      observe({
        if (sample_colours_selection_button_visible$a == TRUE) {
          isolate({
            output$pca_change_sample_colours_container2 <- renderUI({
              tagList(
                radioButtons(ns("pca_sample_colors_radiobuttons"), "Colour the samples based on:",
                             c("Class labels" = "class_labels",
                               "k-means clustering" = "clusters")),
                actionButton(ns("pca_update_sample_colours"), "Update sample colours", icon("refresh", class = NULL, lib = "font-awesome")),
                bsTooltip(id = ns("pca_update_sample_colours"), title = "Click here to apply the changes to the sample colouring mode", placement = "left", trigger = "hover")
              )
            })
          })
          
        } else {
          isolate({
            output$pca_change_sample_colours_container2 <- renderUI({})
          })
        }
      })
      
      output$pca_clustering_settings_container <- renderUI({
        tagList(
          actionButton(ns("pca_clustering_settings_button"), "Clustering settings...", icon("wrench", class = NULL, lib = "font-awesome")),
          bsTooltip(id = ns("pca_clustering_settings_button"), title = "Click here to edit the settings of k-means clustering", placement = "left", trigger = "hover"),
          uiOutput(ns("pca_clustering_settings_container2"))
        )
      })
      
      
      
      observe({
        if (pca_extra_plots_visible$clustering_settings == TRUE) {
          isolate({
            output$pca_clustering_settings_container2 <- renderUI({
              wellPanel(
                htmlOutput(ns("pca_current_number_of_clusters")),
                numericInput(ns("pca_new_number_of_clusters"), "New number of clusters:", clustering_tracker$kmeans_number_of_clusters, min = 2, max = length(pca1$a@class_labels_vector)),
                actionButton(ns("pca_set_number_of_clusters_button"), "Set the number of clusters"),
                bsTooltip(id = ns("pca_set_number_of_clusters_button"), title = "Click here to set the number of clusters to the specified value", placement = "left", trigger = "hover"),
                helpText("Please choose the principal components that will be used in the clustering:"),
                checkboxGroupInput(ns("pca_principal_components_for_clustering_groupinput"), "Choose components", choices = colnames(pca1$a@s$pca_result$x), selected = components_selected_for_clustering$l),
                actionButton(ns("pca_select_pcs_used_for_clustering"), "Set the k-means clustering to use the selected components"),
                bsTooltip(id = ns("pca_select_pcs_used_for_clustering"), title = "Click here to set the k-means clustering to use only the components selected in the checkboxes above", placement = "left", trigger = "hover"),
                actionButton(ns("pca_rerun_with_new_settings"), "Rerun PCA with new clustering settings", icon("refresh", class = NULL, lib = "font-awesome")),
                bsTooltip(id = ns("pca_rerun_with_new_settings"), title = "Click here to rerun PCA with new clustering settings", placement = "left", trigger = "hover")
              )
            })
          })
          
        } else {
          isolate({
            output$pca_clustering_settings_container2 <- renderUI({})
          })
        }
      })
      
      output$pca_scores_selectinput_container <- renderUI({
        tagList(
          selectInput(ns("pca_scores_selectinput_pc1"), "x axis component:", choices = pc_names$a, selected = "PC1"),
          selectInput(ns("pca_scores_selectinput_pc2"), "y axis component:", choices = pc_names$a, selected = "PC2")
        )
      })
    })
  }
  
  observeEvent(input$pca_select_pcs_used_for_clustering, {
    isolate({
      if(length(input$pca_principal_components_for_clustering_groupinput)<2) {
        show_generic_error_message("At least 2 principal components need to be selected for the clustering")
        return()
      }
      components_selected_for_clustering$l <- input$pca_principal_components_for_clustering_groupinput
      showModal(modalDialog(
        title = "Info",
        "The list of principal components that are used for k-means clustering was updated")
      )
    })
  })
  
  observeEvent(input$pca_clustering_plot_show_labels, {
    isolate({
      if(input$pca_clustering_plot_show_labels) {
        class_labels_on_clustering_plot$a <- TRUE
      } else {
        class_labels_on_clustering_plot$a <- FALSE
      }
    })
  })
  
  
  observeEvent(input$pca_clustering_settings_button, {
    isolate({
      pca_extra_plots_visible$clustering_settings <- !pca_extra_plots_visible$clustering_settings
      if(pca_extra_plots_visible$clustering_settings) {
        output$pca_current_number_of_clusters = renderText({
          paste("<b>Current number of clusters:</b> ", clustering_tracker$kmeans_number_of_clusters)
        })
      }
    })
  })
  
  
  observeEvent(input$show_sample_colours_panel_button, {
    isolate({
      sample_colours_selection_button_visible$a <- !sample_colours_selection_button_visible$a
    })
  })
  
  
  observeEvent(input$pca_set_number_of_clusters_button, {
    # _Sets the number of clusters in k-means clustering to a user-defined value
    isolate({
      new_clusters_nr <- input$pca_new_number_of_clusters
      new_clusters_nr_faulty <- FALSE
      if(is.numeric(new_clusters_nr)) {
        if(all.equal(new_clusters_nr, as.integer(new_clusters_nr))) {
          if(new_clusters_nr >1 & new_clusters_nr <= length(pca1$a@class_labels_vector)) {
            if(new_clusters_nr != clustering_tracker$kmeans_number_of_clusters) {
              clustering_tracker$kmeans_number_of_clusters <- new_clusters_nr
            } else {
              show_generic_error_message("The number of clusters is already set to the specified value")
            }
          } else {
            new_clusters_nr_faulty <- TRUE
          }
        } else {
          new_clusters_nr_faulty <- TRUE
        }
      } else {
        new_clusters_nr_faulty <- TRUE
      }
      if(new_clusters_nr_faulty) {
        show_generic_error_message("The specified value is not valid for the number of k-means clusters")
      }
    })
  })
  
  
  observeEvent(input$pca_update_sample_colours, {
    # _Toggles the colouring of samples in plots: it is either by sample class or by k-means clusters
    isolate({
      old_samples_colour_mode <- samples_colour_mode$a
      samples_colour_mode$a <- input$pca_sample_colors_radiobuttons
      
      if(samples_colour_mode$a != old_samples_colour_mode) {
        update_pca_scores_plot(samples_colour_mode$a)
      }
      
    })
  })
  
  
  observeEvent(input$run_pca_button, {
    # _Initial PCA page button to run PCA for the first time
    isolate({
      respond_to_pca_click()
    })
  })
  
  observeEvent(input$pca_rerun_with_new_settings, {
    # _Button for rerunning PCA after changing clustering settings
    isolate({
      respond_to_pca_click()
    })
  })
  
  get_panel_plot <- function(pca1_a, panel_plot_components_count_a, samples_colour_mode_a) {
    pca_plot_data_f <- generate_panel_plots(pca1_a, panel_plot_components_count_a, samples_colour_mode_a)
    number_of_components_f <- as.numeric(pca_plot_data_f$number_of_components)
    panel_plot_height <- 200*number_of_components_f
    panel_plot_width <- 200*number_of_components_f
    
    plot1 <- multiplot(plotlist = pca_plot_data_f$pca_plots_list, cols = number_of_components_f)
    return(plot1)
  }
  
  get_panel_plot_legend <- function(pca1_a, panel_plot_components_count_a, samples_colour_mode_a) {
    pca_plot_data_f <- generate_panel_plots(pca1_a, panel_plot_components_count_a, samples_colour_mode_a)
    plot_legend <- ggdraw(pca_plot_data_f$legend)
    return(plot_legend)
  }
  
  observeEvent(input$update_panel_plots_button, {
    # _Updates the panel of ggplot2 PCA plots
    isolate({
      pca_plot_data_f <- generate_panel_plots(pca1$a, panel_plot_components_count$a, samples_colour_mode$a)
      
      number_of_components_f <- as.numeric(pca_plot_data_f$number_of_components)
      panel_plot_height <- 200*number_of_components_f
      panel_plot_width <- 200*number_of_components_f
      
      output$pca_panel_plot <- renderPlot({
        multiplot(plotlist = pca_plot_data_f$pca_plots_list, cols = number_of_components_f)
      }, width = panel_plot_width, height = panel_plot_height)
      
      labels_count_f <- NULL
      if(samples_colour_mode$a == "class_labels") {
        labels_count_f <- length(unique(pca1$a@class_labels_vector))
      } else if (samples_colour_mode$a == "clusters") {
        labels_count_f <- length(unique(pca1$a@s$pca_scores_frame$cluster_name))
      }
      
      output$pca_panel_plot_legend <- renderPlot({
        ggdraw(pca_plot_data_f$legend)
      }, width = 100, height = 25*labels_count_f)
      
      pca_extra_plots_visible$panel_plot <- TRUE
    })
  })
  
  observeEvent(input$show_panel_plots_button, {
    isolate({
      pca_extra_plots_visible$panel_plot_selectinput <- !pca_extra_plots_visible$panel_plot_selectinput
      if(pca_extra_plots_visible$panel_plot_selectinput) {
        updateActionButton(session, "show_panel_plots_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else if (!pca_extra_plots_visible$panel_plot_selectinput) {
        updateActionButton(session, "show_panel_plots_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  
  
  generate_panel_plots <- function(pca1_f, panel_plot_number_of_components, colour_mode_f) {
    # Generates the plots for the ggplot2 PCA plots panel
    output_list_f <- list()
    
    colouring_scheme_f <- get_pca_scores_plot_colouring_scheme(pca1_f, colour_mode_f)
    
    pca_plots_list <- list()
    
    legend_plot_f <- generate_ggplot2_pca_scores_plot(pca1_f, colouring_scheme_f, 1, 2, TRUE)
    legend_f <- get_legend(legend_plot_f)
    
    withProgress(message = 'Calculating plot data', value = 0, {
      n <- as.numeric(panel_plot_number_of_components)
      
      for(y_index in 1:panel_plot_number_of_components) {
        for(x_index in 1:panel_plot_number_of_components) {
          new_plot_f <- NULL
          if(x_index == y_index) {
            new_plot_f <- generate_ggplot2_pca_stats_plot(pca1_f, x_index)
          }
          else if (x_index < y_index) {
            new_plot_f <- generate_ggplot2_pca_loadings_plot(pca1_f, x_index, y_index)
          }
          else if (x_index > y_index) {
            new_plot_f <- generate_ggplot2_pca_scores_plot(pca1_f, colouring_scheme_f, y_index, x_index, FALSE)
          }
          pca_plots_list <- append(pca_plots_list, list(new_plot_f))
        }
        incProgress(1/n, detail = paste("Doing part", y_index))
      }
    })
    
    output_list_f$number_of_components <- panel_plot_number_of_components
    output_list_f$pca_plots_list <- pca_plots_list
    output_list_f$legend <- legend_f
    
    return(output_list_f)
  }
  
  observeEvent(input$run_pca_button2, {
    # _Button for rerunning PCA after it has been run at least once
    isolate({
      d <- event_data(event = "plotly_selected", source = "pca_scores1")
      d2 <- event_data(event = "plotly_selected", source = "pca_loadings1")
      
      old_pca_df <- pca1$a@t@df
      if (!is.null(d)) {
        if(ncol(pca1$a@t@df)-length(d$key)<= 3) {
          show_generic_error_message("The number of samples cannot be reduced to be less than 3")
          pca1$a@t@df <- old_pca_df
        } else {
          pca1$a <- preproc_remove_samples(pca1$a, d$key)
        }
      }
      
      if (!is.null(d2)) {
        if(nrow(pca1$a@t@df)-length(d2$key)<= 2) {
          show_generic_error_message("The number of variables cannot be reduced to be less than 3")
          pca1$a@t@df <- old_pca_df
        } else {
          pca1$a@t@df <- preproc_remove_variables(pca1$a, d2$key)
        }
      }
      respond_to_pca_click()
    })
  })
  
  observeEvent(input$show_pca_scree_plot, {
    isolate({
      pca_extra_plots_visible$scree <- !pca_extra_plots_visible$scree
      if(pca_extra_plots_visible$scree) {
        updateActionButton(session, "show_pca_scree_plot", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else if (!pca_extra_plots_visible$scree) {
        updateActionButton(session, "show_pca_scree_plot", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  
  observeEvent(input$pca_panel_plot_selectinput, {
    isolate({
      panel_plot_components_count$a <- input$pca_panel_plot_selectinput
    })
  })
  
  
  observeEvent(input$show_pca_kmeans_clustering_plot, {
    isolate({
      pca_extra_plots_visible$clustering <- !pca_extra_plots_visible$clustering
      if(pca_extra_plots_visible$clustering) {
        updateActionButton(session, "show_pca_kmeans_clustering_plot", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
      } else if (!pca_extra_plots_visible$clustering) {
        updateActionButton(session, "show_pca_kmeans_clustering_plot", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
      }
    })
  })
  
  observeEvent(input$pca_scores_selectinput_pc1, {
    isolate({
      pca_plot_pc_choice$scores_pc1 <- input$pca_scores_selectinput_pc1
      pca_plot_pc_choice$loadings_pc1 <- input$pca_scores_selectinput_pc1
      update_pca_scores_plot(samples_colour_mode$a)
      update_pca_loadings_plot()
    })
  })
  
  observeEvent(input$pca_scores_selectinput_pc2, {
    isolate({
      pca_plot_pc_choice$scores_pc2 <- input$pca_scores_selectinput_pc2
      pca_plot_pc_choice$loadings_pc2 <- input$pca_scores_selectinput_pc2
      update_pca_scores_plot(samples_colour_mode$a)
      update_pca_loadings_plot()
    })
  })
  
  
  observeEvent(input$pca_reset_changes, {
    # _Button to reset the PCA page to its initial state (to undo changes to the data preview)
    isolate({
      reset_pca_preview(t1_input$a)
      respond_to_pca_click()
    })
  })
  
  observeEvent(input$pca_apply_changes, {
    # _Button to apply the previewed changes to the data)
    isolate({
      
      pca_output_counter$a <- pca_output_counter$a +1
      
      pca_module_output$l$df <-  pca1$a@t@df
      new_class_labels_frame <- class_labels_vector_to_class_labels_frame(pca1$a@class_labels_vector)
      pca_module_output$l$class_labels_frame <- new_class_labels_frame
      pca_module_output$l$changes_applied <- TRUE
      pca_module_output$l$output_counter <- pca_output_counter$a
      
    })
  })
  
  set_pca_output_to_cancelled <- function() {
    pca_output_counter$a <- pca_output_counter$a +1
    pca_module_output$l$df <<-  NULL
    pca_module_output$l$class_labels_frame <<- NULL
    pca_module_output$l$changes_applied <<- FALSE
    pca_module_output$l$output_counter <- pca_output_counter$a
  }
  
  observeEvent(input$pca_move_to_next_step_without_changes, {
    isolate({
      set_pca_output_to_cancelled()
    })
  })
  
  pca_output_reactive <- reactive({
    pca_module_output$l
  })
  
  observeEvent(input$skip_pca_button, {
    isolate({
      set_pca_output_to_cancelled()
      js$enableTab("tab_sorting")
      updateTabsetPanel(session, "navbar", "tab_sorting")
    })
  })
  
  return(pca_output_reactive)
}

