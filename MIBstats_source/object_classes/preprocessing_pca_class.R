# _This class is used for the data preprocessing step that uses PCA

preprocessing_pca <- setClass("preprocessing_pca",
                                    slots = c(t = "input_data_table", s = "list", l = "list", class_labels_vector = "character"),
                                      contains = c("preprocessing_shared"), 
                                      
                                      prototype=list(
                                        t = input_data_table(), s = list(), l = list(), class_labels_vector = c()
                                      )
)

setGeneric(name="run_pca",
           def=function(theObject, kmeans_number_of_clusters_f, components_selected_for_clustering_f)
           {
             standardGeneric("run_pca")
           }
)

setMethod(f="run_pca",
          # _Method to run PCA
          signature="preprocessing_pca",
          definition=function(theObject, kmeans_number_of_clusters_f, components_selected_for_clustering_f)
          {
            remove_inf_values1 <- function(df_f_data) {
              # _Replaces infinite values with NA in the PCA preview data, because the PCA function does not accept infinite values
              replacement_f1 <- function(x){replace(x, x == -Inf, NA)}
              replacement_f2 <- function(x){replace(x, x == Inf, NA)}
              replacement_f3 <- function(x){replace(x, is.nan(x), NA)}
              df_f_data <- as.data.frame(lapply(df_f_data, replacement_f1))
              df_f_data <- as.data.frame(lapply(df_f_data, replacement_f2))
              df_f_data <- as.data.frame(lapply(df_f_data, replacement_f3))
              return(df_f_data)
            }
            
            summary_list_f <- list()
            data_f <- preproc_reformat_input_table(theObject)
            input_frame_rownames <- theObject@t@df[,1]
            data_f <- remove_inf_values1(data_f)
            data_f <- t(data_f)
            data_f_old <- data_f
            data_f <- as.matrix(data_f, stringsAsFactors = FALSE)
            colnames(data_f) <- input_frame_rownames
            pca_result <- prcomp(data_f, center=T, scale=T)
            pca_component_names <- colnames(pca_result$x)
            components_not_selected <- setdiff(pca_component_names, components_selected_for_clustering_f)
            pca_result_x_sel_comp <- pca_result$x[, !(colnames(pca_result$x) %in% components_not_selected)]
            pca_hc <- kmeans(pca_result_x_sel_comp, kmeans_number_of_clusters_f)
            pca_data_clusters <- pca_hc$clust
            
            # _add cluster to data frame of scores
            pca_scores_frame <- data.frame(pca_result$x, "cluster" = factor(pca_data_clusters))
            pca_scores_frame <- transform(pca_scores_frame, cluster_name = paste("Cluster", pca_data_clusters))
            
            scores_frame_key <- rownames(pca_scores_frame)
            pca_loadings_frame <- data.frame(unclass(pca_result$rotation))
            loadings_frame_key <- rownames(pca_loadings_frame)
            
            summary_list_f$pca_result <- pca_result
            summary_list_f$pca_component_names <- pca_component_names
            summary_list_f$pca_hc <- pca_hc
            summary_list_f$pca_scores_frame <- pca_scores_frame
            summary_list_f$scores_frame_key <- scores_frame_key
            summary_list_f$pca_loadings_frame <- pca_loadings_frame
            summary_list_f$loadings_frame_key <- loadings_frame_key
            summary_list_f$input_data <- data_f
            summary_list_f$unprocessed_input_data <- theObject@t@df
            
            return(summary_list_f)
          }
)

setGeneric(name="get_pca_cumulative_result",
           def=function(theObject)
           {
             standardGeneric("get_pca_cumulative_result")
           }
)

setMethod(f="get_pca_cumulative_result",
          # _Extracts the cumulative explained variance for each principal component from the PCA data. Converts it from ratio to percentage
          signature="preprocessing_pca",
          definition=function(theObject)
          {
            summary_pca2_f <- summary(theObject@s$pca_result)
            cumulative_result_f <- summary_pca2_f$importance[3,]
            cumulative_result_f <- cumulative_result_f*100
            return(cumulative_result_f)
          }
)

setGeneric(name="generate_ggplot2_pca_scores_plot",
           def=function(theObject, colouring_scheme_f2, x_ind_f, y_ind_f, legend_boolean_f)
           {
             standardGeneric("generate_ggplot2_pca_scores_plot")
           }
)

setMethod(f="generate_ggplot2_pca_scores_plot",
          # _Generates the ggplot2 PCA scores plot for the static panel of PCA results
          signature="preprocessing_pca",
          definition=function(theObject, colouring_scheme_f2, x_ind_f, y_ind_f, legend_boolean_f)
          {
            s_f2 <- theObject@s
            colnames_f <- colnames(s_f2$input_data)
            colnames_f <- c(colnames_f, "colouring_scheme_ff")
            
            input_data_with_class_labels_f <- data.frame(s_f2$input_data, colouring_scheme_f2, stringsAsFactors = FALSE)
            colnames(input_data_with_class_labels_f) <- colnames_f
            new_plot_f <- NULL
            new_plot_f <- autoplot(s_f2$pca_result, data = input_data_with_class_labels_f, x = x_ind_f, y = y_ind_f, colour = "colouring_scheme_ff") + theme(axis.title=element_text(size=14)) + ggtitle("Scores")
            if(!legend_boolean_f) {
              new_plot_f <- new_plot_f + theme(legend.position="none")
            }
            
            new_plot_f$labels$colour <- "Class labels"
            return(new_plot_f)
          }
)

setGeneric(name="generate_ggplot2_pca_loadings_plot",
           def=function(theObject, x_ind_f, y_ind_f)
           {
             standardGeneric("generate_ggplot2_pca_loadings_plot")
           }
)

setMethod(f="generate_ggplot2_pca_loadings_plot",
          # _Generates the ggplot2 PCA loadings plot for the static panel of PCA results
          signature="preprocessing_pca",
          definition=function(theObject, x_ind_f, y_ind_f)
          {
            s_f2 <- theObject@s
            loadings_frame_f <- s_f2$pca_loadings_frame
            new_loadings_plot_f <- ggplot(loadings_frame_f, aes_string(colnames(loadings_frame_f)[x_ind_f], colnames(loadings_frame_f)[y_ind_f])) + geom_point() + theme(axis.title=element_text(size=14)) + ggtitle("Loadings")
            return(new_loadings_plot_f)
          }
)

setGeneric(name="generate_ggplot2_pca_stats_plot",
           def=function(theObject, ind_f)
           {
             standardGeneric("generate_ggplot2_pca_stats_plot")
           }
)

setMethod(f="generate_ggplot2_pca_stats_plot",
          # _Generates the ggplot2 plots that show the cumulative explained variance for every principal component, in text format. Used in the static panel of PCA results
          signature="preprocessing_pca",
          definition=function(theObject, ind_f)
          {
            cumulative_result_f2 <- get_pca_cumulative_result(theObject)
            cumul_names <- names(cumulative_result_f2)
            axis_title_f <- cumul_names[ind_f]
            
            stats_plot_text <- paste("Cumulative\n variance explained:\n", cumulative_result_f2[ind_f], "%", sep="")
            stats_plot_f <- ggplot() + aes_string(axis_title_f, axis_title_f) +
              annotate("text", x = 4, y = 25, size=4, label = stats_plot_text) + 
              theme_bw() +
              theme(panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank()) + theme(axis.title=element_text(size=14))
            
            return(stats_plot_f)
          }
)

setGeneric(name="get_pca_scores_plot_colouring_scheme",
           def=function(theObject, colour_mode_f)
           {
             standardGeneric("get_pca_scores_plot_colouring_scheme")
           }
)

setMethod(f="get_pca_scores_plot_colouring_scheme",
          # _Retrieves a vector with class labels or cluster labels, depending on which one is used to colour-code samples in plots
          signature="preprocessing_pca",
          definition=function(theObject, colour_mode_f)
          {
            pca1_f <- theObject
            class_labels_vector_f <- pca1_f@class_labels_vector
            cluster_labels_vector_f <- pca1_f@s$pca_scores_frame$cluster_name
            
            colouring_scheme_f2 <- NULL
            if(colour_mode_f == "clusters") {
              colouring_scheme_f2 <- cluster_labels_vector_f
            } else if(colour_mode_f == "class_labels") {
              colouring_scheme_f2 <- class_labels_vector_f
            }
            return(colouring_scheme_f2)
          }
)

