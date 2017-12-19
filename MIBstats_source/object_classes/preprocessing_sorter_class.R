# _This class is used for sorting data (a preprocessing step)

preprocessing_sorter <- setClass("preprocessing_sorter",
                              slots = c(t = "input_data_table", selected_samples = "character", class_labels_vector = "character", l = "list", l2 = "list"),
                              contains = c("panel_tracker", "preprocessing_shared"),
                              prototype=list(
                                t = input_data_table(), selected_samples = "", class_labels_vector = c(), l = list(), l2 = list()
                              )
)

setGeneric(name="sorter_get_sample_names",
           def=function(theObject)
           {
             standardGeneric("sorter_get_sample_names")
           }
)

setMethod(f="sorter_get_sample_names",
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            # _Gets input data sample names as a vector
            data_f <- theObject@t@df
            sample_names_f <- names(data_f[1, 2:ncol(data_f)])
            return(sample_names_f)
          }
)

setGeneric(name="sorter_get_variable_names",
           def=function(theObject)
           {
             standardGeneric("sorter_get_variable_names")
           }
)

setMethod(f="sorter_get_variable_names",
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            # _Gets input data variable names as a vector
            data_f <- theObject@t@df
            variable_names_f <- as.character(data_f[, 1])
            return(variable_names_f)
          }
)

setGeneric(name="sorter_samples_reordering_move_cells",
           def=function(theObject)
           {
             standardGeneric("sorter_samples_reordering_move_cells")
           }
)

setMethod(f="sorter_samples_reordering_move_cells",
          signature="preprocessing_sorter",
          definition=function(theObject)
          { 

            if(length(theObject@l$df_sel$in_frame) != length(theObject@l$df_sel$out_frame)) {
              return()
            }
            
            if(is.null(theObject@l$df_sel$in_frame) || is.null(theObject@l$df_sel$out_frame)) {
              show_generic_error_message("Rows need to be selected in both tables in order to move the elements")
              return()
            }
            if(length(theObject@l$df_sel$in_frame) != length(theObject@l$df_sel$out_frame)) {
              show_generic_error_message("The same number of rows needs to be selected in both tables in order to move the elements")
              return()
            }
            
            empty_row_selected_in_input <- FALSE
            for (i in 1:length(theObject@l$df_sel$in_frame)) {
              in_index <- theObject@l$df_sel$in_frame[i]
              if(theObject@l$dt_reordering$in_data[in_index, 1] == "-") {
                empty_row_selected_in_input <- TRUE
              }
            }
            if(empty_row_selected_in_input) {
              show_generic_error_message("The selection in the input table should not contain empty rows in order to move the elements")
              return()
            }
            filled_row_selected_in_output <- FALSE
            for (i in 1:length(theObject@l$df_sel$out_frame)) {
              out_index <- theObject@l$df_sel$out_frame[i]
              if(theObject@l$dt_reordering$out_data[out_index, 1] != "-") {
                filled_row_selected_in_output <- TRUE
              }
            }
            if(filled_row_selected_in_output) {
              show_generic_error_message("The selection in the output table should contain only empty rows in order to move the elements")
              return()
            }
            
            for (i in 1:length(theObject@l$df_sel$in_frame)) {
              sorted_sel_in <- sort(theObject@l$df_sel$in_frame)
              sorted_sel_out <- sort(theObject@l$df_sel$out_frame)
              in_index <- sorted_sel_in[i]
              out_index <- sorted_sel_out[i]
              
              theObject@l$dt_reordering$out_data[out_index, 1] <- theObject@l$dt_reordering$in_data[in_index, 1]
              theObject@l$dt_reordering$in_data[in_index, 1] <- "-"
            }
            
            theObject@l$dt_reordering$in_frame <- datatable(theObject@l$dt_reordering$in_data, rownames = FALSE, options = list(paging = FALSE), selection = "multiple")
            theObject@l$df_sel$out_switch <- TRUE
            theObject@l$df_sel$out_frame <- NULL
            
            theObject@l$df_sel$out_switch <- FALSE
            
            
            theObject@l$empty_rows_in_output <- FALSE
            for (i in 1:nrow(theObject@l$dt_reordering$out_data)) {
              if(theObject@l$dt_reordering$out_data[i, 1] == "-") {
                theObject@l$empty_rows_in_output <- TRUE
              }
            }
            return(theObject)
          }
          
)

setGeneric(name="sorter_apply_samples_order_loaded_from_file",
           def=function(theObject, new_sample_order)
           {
             standardGeneric("sorter_apply_samples_order_loaded_from_file")
           }
)

setMethod(f="sorter_apply_samples_order_loaded_from_file",
          signature="preprocessing_sorter",
          definition=function(theObject, new_sample_order)
          {
            # _Method for replacing the order of samples with the new order loaded from a file by the user
            df_empty_column_names <- rep("-", nrow(theObject@l$reordering_tracker$df_column_names_frame))
            df_empty_column_names <- as.data.frame(df_empty_column_names, stringsAsFactors = FALSE)
            colnames(df_empty_column_names) <- "Sample names"
            theObject@l$dt_reordering$in_data <- as.data.frame(df_empty_column_names)
            theObject@l$dt_reordering$out_data <- as.data.frame(new_sample_order, stringsAsFactors = FALSE)
            colnames(theObject@l$dt_reordering$out_data) <- "Sample names"
            theObject@l$dt_reordering$in_frame <- theObject@l$dt_reordering$in_data
            theObject@l$dt_reordering$out_frame <- theObject@l$dt_reordering$out_data
            theObject@l$df_sel$out_frame <- NULL
            return(theObject)
          }
          
)

setGeneric(name="sorter_update_sample_names_for_reordering",
           def=function(theObject, reset_f)
           {
             standardGeneric("sorter_update_sample_names_for_reordering")
           }
)

setMethod(f="sorter_update_sample_names_for_reordering",
          signature="preprocessing_sorter",
          definition=function(theObject, reset_f)
          {
            # _Updates the list of samples that is used in reordering of samples, taking into account the deletion of samples in the previous preprocessing step
            names_vector_x <- colnames(theObject@l$df_deletion_tracker$samples[2:ncol(theObject@l$df_deletion_tracker$samples)])
            names_vector_y <- theObject@l$reordering_tracker$result
            sorted_vector_x <- sort_vector_by_another_vector(names_vector_x, names_vector_y)
            
            if(reset_f) {
              theObject@l$reordering_tracker$df_column_names <- names_vector_x
            } else {
              theObject@l$reordering_tracker$df_column_names <- sorted_vector_x
            }
            return(theObject)
          }
)

setGeneric(name="sorter_reordering_prepare_input_table",
           def=function(theObject)
           {
             standardGeneric("sorter_reordering_prepare_input_table")
           }
)

setMethod(f="sorter_reordering_prepare_input_table",
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            # _Prepares the input table (with old sample order) that is used in the reordering of samples
            
            theObject@l$reordering_tracker$df_column_names_frame <- as.data.frame(theObject@l$reordering_tracker$df_column_names, stringsAsFactors = FALSE)
            colnames(theObject@l$reordering_tracker$df_column_names_frame) <- "Sample names"
            theObject@l$dt_reordering$in_data <- theObject@l$reordering_tracker$df_column_names_frame
            
            theObject@l$dt_reordering$in_frame <- datatable(theObject@l$dt_reordering$in_data, rownames= FALSE, options = list(paging = FALSE), selection = "multiple")
            
            return(theObject)
          }
)

setGeneric(name="sorter_reordering_prepare_output_table",
           def=function(theObject)
           {
             standardGeneric("sorter_reordering_prepare_output_table")
           }
)

setMethod(f="sorter_reordering_prepare_output_table",
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            # _Prepares the output table (with new sample order) that is used in the reordering of samples
            
            df_empty_column_names <- rep("-", nrow(theObject@l$reordering_tracker$df_column_names_frame))
            df_empty_column_names <- as.data.frame(df_empty_column_names, stringsAsFactors = FALSE)
            colnames(df_empty_column_names) <- "Sample names"
            
            theObject@l$dt_reordering$out_data <- as.data.frame(df_empty_column_names)
            return(theObject)
          }
)

setGeneric(name="sorter_compare_sample_names_from_deletion_and_reordering_steps",
           def=function(theObject)
           {
             standardGeneric("sorter_compare_sample_names_from_deletion_and_reordering_steps")
           }
)

setMethod(f="sorter_compare_sample_names_from_deletion_and_reordering_steps",
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            # _Compares sample names from the preprocessing steps of sample deletion and sample reordering. This is used to determine if the input list of samples for sample reordering needs to be updated
            
            del_tracker_colnames <- colnames(theObject@l$df_deletion_tracker$samples)
            del_tracker_colnames <- del_tracker_colnames[2:length(del_tracker_colnames)]
            colnames_comparison_f <- identical(sort(theObject@l$reordering_tracker$df_column_names), sort(del_tracker_colnames))
            return(colnames_comparison_f)
          }
)

setGeneric(name="sorter_reordering_limit_selected_rows",
           def=function(theObject, input_sel_f, input_last_clicked_f)
           {
             standardGeneric("sorter_reordering_limit_selected_rows")
           }
)

setMethod(f="sorter_reordering_limit_selected_rows",
          signature="preprocessing_sorter",
          definition=function(theObject, input_sel_f, input_last_clicked_f)
          {
            # _Adjusts the number of selected rows in the table of reordering samples. In the new order table, the maximum number of selected rows is equal to the number of selected rows in the old order table 
            
            if(length(input_sel_f) > length(theObject@l$df_sel$in_frame)) {
              if(theObject@l$df_sel$out_switch) {
                theObject@l$df_sel$out_frame <- NULL
              }
              if(length(theObject@l$df_sel$in_frame) == 0) {
                theObject@l$df_sel$out_frame <- NULL
              } else {
                if(!theObject@l$df_sel$out_switch) {
                  theObject@l$df_sel$out_frame <- setdiff(input_sel_f, input_last_clicked_f)
                }
              }

            } else {

              if(theObject@l$df_sel$out_switch) {
                theObject@l$df_sel$out_frame <- NULL
              }

              if(!theObject@l$df_sel$out_switch) {
                theObject@l$df_sel$out_frame <- input_sel_f
              }

            }
            return(theObject)
          }
)

setGeneric(name="sorter_update_after_deletion_of_items",
           def=function(theObject, mode_f, output1_items, output1_items_old)
           {
             standardGeneric("sorter_update_after_deletion_of_items")
           }
)

setMethod(f="sorter_update_after_deletion_of_items",
          signature="preprocessing_sorter",
          definition=function(theObject, mode_f, output1_items, output1_items_old)
          {
            # _Prepares the output table (with new sample order) that is used in the reordering of samples
            
            update_flag <- FALSE
            if(!is.null(output1_items_old)) {
            }
            if(!is.null(output1_items)) {
              if(is.null(output1_items_old)) {
                update_flag <- TRUE
              } else {
                if(!identical(output1_items[[1]]@t@df, output1_items_old[[1]]@t@df)) {
                  update_flag <- TRUE
                }
              }
            }
            if(update_flag) {
              new_pp_sorter1_a_items <- output1_items[[1]]@t@df
              theObject@l$df_deletion_tracker[[mode_f]] <- new_pp_sorter1_a_items
            }
            return(theObject)
          }
)

setGeneric(name="sorter_get_combined_results",
           def=function(theObject)
           {
             standardGeneric("sorter_get_combined_results")
           }
)

setMethod(f="sorter_get_combined_results",
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            # _Prepares a data frame that contains the combined results of deletion of samples and variables and the reordering of samples
            out_list_f <- list()
            
            sample_names_vector_x <- colnames(theObject@l$df_deletion_tracker$samples[2:ncol(theObject@l$df_deletion_tracker$samples)])
            sample_names_vector_y <- theObject@l$reordering_tracker$result
            sorted_samples_vector_x <- sort_vector_by_another_vector(sample_names_vector_x, sample_names_vector_y)
            
            combined_frame <- theObject@l$df_deletion_tracker$variables[sorted_samples_vector_x]
            combined_frame <- cbind(theObject@l$df_deletion_tracker$variables[,1], theObject@l$df_deletion_tracker$variables[sorted_samples_vector_x])
            colnames(combined_frame) <- c("X", sorted_samples_vector_x)
            
            samples_dict <- get_samples_dict(theObject)
            
            updated_class_labels <- samples_dict[sorted_samples_vector_x]
            
            updated_class_labels_frame <- class_labels_vector_to_class_labels_frame(updated_class_labels)
            
            out_list_f$class_labels_frame <- updated_class_labels_frame
            
            theObject <- get_mcl_result(theObject)
            if(!is.null(theObject@l2$mcl_tracker$result)) {
              class_labels_frame_mcl <- sorter_class_labels_vector_to_class_labels_frame(theObject)
              out_list_f$class_labels_frame <- class_labels_frame_mcl
            }
            
            out_list_f$combined_frame <- combined_frame
            
            return(out_list_f)
          }
)

source("object_classes/preprocessing_sorter_methods_for_class_labels_changer.R")

