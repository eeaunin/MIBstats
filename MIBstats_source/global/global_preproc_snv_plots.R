# _Module for drawing box plots on the scaling, normalisation and variance stabilisation page of preprocessing

preproc_snv_plotsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("snv_boxplot_button_uioutput")),
    
    uiOutput(ns("snv_boxplot_uioutput"))
  )
}

preproc_snv_plots <- function(input, output, session, df_pre_f, df_post_f, processing_done_f, plot_title_f, snv_closebuttons_reactive_f) {

  ns <- session$ns
  
  plot_file_format <- reactiveValues(a = "svg")
  
  plot_file_format_reactive <- callModule(image_format_selection_module, "preproc_snvplots_image_format")
  
  observe({
    plot_file_format_observed <- plot_file_format_reactive()
    isolate({
      if(!is.null(plot_file_format_observed)) {
        plot_file_format$a <- plot_file_format_observed
      }
    })
  })
  
  current_session_name <- session$ns("name")
  current_session_name <- as.character(strsplit(current_session_name, "-name"))
  splitname <- unlist(strsplit(current_session_name, "-"))
  current_session_name <- splitname[2]
  
  old_close_switch <- reactiveValues(a = 1)
  
  plot_visible <- reactiveValues(a = FALSE)
  processing_done_bool <- reactiveValues(a = processing_done_f)
  
  react <- reactiveValues(df_pre = data.frame(), df_post = data.frame(), processing_done = FALSE, plot_title = "", close_switch = NULL)
  
  zoom_f = 3
  
  plots_for_downloading <- reactiveValues(pre = NULL, post = NULL)
  pre_plot_reactive <- reactive({plots_for_downloading$pre})
  post_plot_reactive <- reactive({plots_for_downloading$post})
  
  observeEvent(input$snv_view_boxplots_button, {
    
    isolate({
      plot_visible$a <- !plot_visible$a
      update_button_icon(plot_visible$a)
      
      if(plot_visible$a & react$processing_done) {
        snv_ggplot_pre <- generate_snv_boxplot(react$df_pre)
        snv_ggplot_post <- generate_snv_boxplot(react$df_post)
        
        plots_for_downloading$pre <- snv_ggplot_pre
        plots_for_downloading$post <- snv_ggplot_post
        
        output$snv_plot_pre <- renderPlot({
          snv_ggplot_pre
        }, height = 200*zoom_f, width = 400*zoom_f)
        
        output$snv_plot_post <- renderPlot({
          snv_ggplot_post
        }, height = 200*zoom_f, width = 400*zoom_f)
      }
    })
  })
  
  observe({
    # _react$processing_done keeps track of whether the processing to make the data for the plot has been done
    processing_done_list <- processing_done_f()
    isolate({
      if(!is.null(processing_done_list)) {
        react$processing_done <- processing_done_list[[current_session_name]]
      }
    })
  })
  
  observe({
    # _If the processing to make the data for the plot has been done, gets the data frame for the "before" plot
    df_pre_list <- df_pre_f()
    isolate({
      if(!is.null(df_pre_list[[current_session_name]])) {
        react$df_pre <- df_pre_list[[current_session_name]]
      }
    })
  })
  
  observe({
    # _If the processing to make the data for the plot has been done, gets the data frame for the "after" plot
    df_post_list <- df_post_f()
    isolate({
      if(!is.null(df_post_list[[current_session_name]])) {
        react$df_post <- df_post_list[[current_session_name]]
      }
    })
  })
  
  react$plot_title <- plot_title_f
  
  observe({
    # _Keeps track of the trigger that closes all plots on the page at once
    react$close_switch <- snv_closebuttons_reactive_f()
    if(react$close_switch$a > old_close_switch$a) {
      isolate({
        old_close_switch$a <- react$close_switch$a
        plot_visible$a <- FALSE
        update_button_icon(plot_visible$a)
      })
    }
  })
  
  observe({
    if((!is.null(react)) && (!is.null(react$processing_done)) && (react$processing_done==TRUE)) {
      isolate({
        output$snv_boxplot_button_uioutput <- renderUI({
          tagList(
            actionButton(ns("snv_view_boxplots_button"), "View box plots", icon = icon("plus-square-o", class = NULL, lib = "font-awesome")),
            bsTooltip(id = ns("snv_view_boxplots_button"), title = "Click here to view box plots with preview of the preprocessing substep", 
                      placement = "left", trigger = "hover")
          ) 
        })
      })
    } else {
      isolate({
        output$snv_boxplot_button_uioutput <- renderUI({})
      })
    }
  })
  

  observe({
    # _plot_visible$a keeps track of the state of the toggle button that shows or hides the box plot. The plot is rendered or hidden, based on the state of plot_visible$a
    if(plot_visible$a) {
      isolate({
        output$snv_boxplot_uioutput <- renderUI({
          tagList(
            tags$h5("Before"),
            tags$br(),
            plotOutput(ns("snv_plot_pre"), width = "100%", height = "100%"),
            downloadButton(ns("download_snv_pre_plot"), "Download the \"Before\" plot"),
            bsTooltip(id = ns("download_snv_pre_plot"), title = "Click here to download the \"Before\" plot as an image", 
                      placement = "left", trigger = "hover"),
            tags$h5("After"),
            tags$br(),
            plotOutput(ns("snv_plot_post"), width = "100%", height = "100%"),
            downloadButton(ns("download_snv_post_plot"), "Download the \"After\" plot"),
            bsTooltip(id = ns("download_snv_post_plot"), title = "Click here to download the \"After\" plot as an image", 
                      placement = "left", trigger = "hover"),
            image_format_selection_moduleUI(ns("preproc_snvplots_image_format")),
            tags$hr()
          )
        })
      })
      
    } else {
      isolate({
        output$snv_boxplot_uioutput <- renderUI({})
      })
    }
  })

  generate_snv_boxplot <- function(df_f) {
    # _Draws the box plot of the preprocessing substep
    dat <- df_remove_first_col(df_f)
    dat$group <- row.names(dat)
    dat.m <- melt(dat, id.vars = "group")
    snv_ggplot <- ggplot(dat.m, aes(x = factor(group, levels=unique(group)), y = value)) + geom_boxplot() + xlab("Variables") + ylab("Value") + ggtitle(plot_title_f) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.text = element_text(size = 10)) + theme(aspect.ratio=2/5)
    return(snv_ggplot)
  }
  
  update_button_icon <- function(plot_visible_a) {
    # _Changes the icon of the toggle button that shows or hides the box plot, depending on its state
    if(plot_visible_a) {
      updateActionButton(session, "snv_view_boxplots_button", icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
    } else if (!plot_visible_a) {
      updateActionButton(session, "snv_view_boxplots_button", icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
    }
  }
  
  output$download_snv_pre_plot <- downloadHandler(
    filename = function() { paste(input$dataset, ".", plot_file_format$a, sep='') },
    content = function(file) {
      ggsave(file, pre_plot_reactive(), device = plot_file_format$a, width = 20, height = 10, units = "in", dpi = 900)
    }
  )
  
  output$download_snv_post_plot <- downloadHandler(
    filename = function() { paste(input$dataset, ".", plot_file_format$a, sep='') },
    content = function(file) {
      ggsave(file, post_plot_reactive(), device = plot_file_format$a, width = 20, height = 10, units = "in", dpi = 900)
    }
  )
 
}