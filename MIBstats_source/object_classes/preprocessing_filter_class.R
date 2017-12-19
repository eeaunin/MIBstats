# _This class is used for filtering data (preprocessing step)

compute.rsd <- function(data.QC.vector){
  # _Function that calculates relative standard deviation of a vector
  rsd <- abs((sd(data.QC.vector)/mean(data.QC.vector))*100)
  return(rsd)
} 

preprocessing_filter <- setClass("preprocessing_filter",
                                 slots = c(t = "input_data_table", l = "list", class_labels_vector = "character", rsd_done = "logical", iqr_done = "logical"),
                                 contains = c("panel_tracker", "preprocessing_shared"),
                                 prototype=list(
                                   t = input_data_table(), l = list(), class_labels_vector = "", rsd_done = FALSE, iqr_done = FALSE
                                 )
)

setGeneric(name="rsd_filter",
           def=function(theObject, rsd_threshold_f, qc_classes_f)
           {
             standardGeneric("rsd_filter")
           }
)

setMethod(f="rsd_filter",
          signature="preprocessing_filter",
          definition=function(theObject, rsd_threshold_f, qc_classes_f)
          {
            # _Filters variables in data by relative standard deviation (RSD), replaces values whose RSD exceeds a user-defined threshold with NA
            
            data_f <- preproc_reformat_input_table(theObject)
            t1_filtered_data <- theObject@t@df
            class_labels_vector_f <- theObject@class_labels_vector
            
            data.QC <- data_f[,which(class_labels_vector_f%in%qc_classes_f)]
            flag.rsd <- TRUE
            too_few_samples <- FALSE
            if(ncol(data.QC)<3){
              too_few_samples <- TRUE
              flag.rsd <- FALSE
            }
            # _columns are samples
            
            if(!too_few_samples) {
              rsd.vec <-apply(data.QC, 1,compute.rsd)
              ind.keep <- which(rsd.vec <= rsd_threshold_f)
              ind.rem <- which(rsd.vec > rsd_threshold_f)
              filtered_data_f <- data_f[ind.keep,]
            } else {
              show_generic_error_message("Too few quality control samples: at least 3 is required")
              filtered_data_f <- data_f
            }
            
            # _error messages
            if(length(ind.keep) < 4){
              show_generic_error_message("Filtering was aborted because too few variables (less than 4) would have survived the filter")
              filtered_data_f <- data_f
              flag.rsd <- FALSE
            }
            
            # _messages
            if(length(ind.rem)==0){
              message_out("No variables were removed", "RSD filtering")
              flag.rsd <- FALSE
            }  
            
            if(flag.rsd){
              message <- paste(length(ind.rem)," variables were removed: ", vector_to_comma_separated_character(as.character(rownames(data_f)[ind.rem])), sep="")
              message_out(message, "RSD filtering")
              theObject@rsd_done <- TRUE
            }
            theObject@t@df <- data_frame_to_t1_format(filtered_data_f)
            return(theObject)
          }
)

setGeneric(name="iqr_filter",
           def=function(theObject, selected_variables_f, iqr_multiplier_f, replacement_choice_f)
           {
             standardGeneric("iqr_filter")
           }
)

setMethod(f="iqr_filter",
          signature="preprocessing_filter",
          definition=function(theObject, selected_variables_f, iqr_multiplier_f, replacement_choice_f)
          {
            # _Filters samples in the data by the interquartile range (IQR) of values of a user-defined set of variables.
            # _Values that are different from the mean of the variable by more than IQR*iqr_multiplier_f2 are either
            # _1) replaced with NA or 2) replaced with the average value of the variable, depending on what the user chooses.
            
            data_f <- preproc_reformat_input_table(theObject)
            
            iqr_find_outliers_in_vector <- function(current_row_f, iqr_multiplier_f2) {
              # _Finds the indices of outliers in a vector, based on interquartile range (IQR)
              # _Adapted from code from http://stamfordresearch.com/outlier-removal-in-r-using-iqr-rule/
              max <- quantile(current_row_f,0.75, na.rm=TRUE) + (IQR(current_row_f, na.rm=TRUE) * iqr_multiplier_f2)
              min <- quantile(current_row_f,0.25, na.rm=TRUE) - (IQR(current_row_f, na.rm=TRUE) * iqr_multiplier_f2)
              idx <- which(current_row_f < min | current_row_f > max)
              return(idx)
            }
            
            iqr_find_and_replace_outliers <- function(data_f2, selected_variables_f2, replacement_choice_f2) {
              # _Function for finding outliers in a data matrix by interquartile range.
              # _Rows are variables and columns are samples in the input data.
              # _The function goes through the matrix row by row and finds outliers in each row. 
              out_list <- list()
              variables_with_outliers <- NULL
              
              outliers_count <- 0
              outliers_names_list <- list()
              
              new_matrix <- NULL
              if(!is.null(nrow(data_f2))) {
                
                for(i in 1:nrow(data_f2)){
                  current_row <- data_f2[i,]
                  
                  new_row <- current_row
                  outliers_names <- NULL
                  if(i>0 & i<=nrow(data_f2)) {
                    
                    if(is.element(i, selected_variables_f2)) {
                      
                      idx <- iqr_find_outliers_in_vector(current_row, iqr_multiplier_f)

                      
                      idx_colnames <- colnames(data_f2)[idx]
                      
                      if(replacement_choice_f2 == "replace_with_na") {
                        new_row <- replace(new_row, idx, NA)
                      } else if (replacement_choice_f2 == "replace_with_average") {
                        average_value <- mean(current_row, na.rm = TRUE)
                        new_row <- replace(new_row, idx, average_value)
                      }
                      
                      outliers_count <- outliers_count + length(idx)
                      if(length(idx)>0) {
                        variables_with_outliers <- c(variables_with_outliers, rownames(data_f2)[i])
                        outliers_names_list <- append(outliers_names_list, list(idx_colnames))
                      }
                      
                    }
                  }
                  if(is.null(new_matrix)) {
                    new_matrix <- new_row
                  } else {
                    new_matrix <- rbind(new_matrix, new_row)
                  }
                }
              }
              
              if(!is.null(variables_with_outliers)) {
                names(outliers_names_list) <- variables_with_outliers
              }
              out_list$new_matrix <- new_matrix
              out_list$outliers_names_list <- outliers_names_list
              out_list$outliers_count <- outliers_count
              return(out_list)
            }
            
            get_iqr_report_string <- function(iqr_outliers_f, replacement_choice_f2) {
              # _Generates a report message of the IQR filtering for displaying on the screen
              report_string <- "IQR filtering was run. "
              
              if(iqr_outliers$outliers_count > 0) {
                outliers_string <- NULL
                outliers_count_string <- NULL
                
                outliers_count_string <- paste("Number of outliers detected: ", as.character(iqr_outliers$outliers_count), ". ", sep="")
                if(replacement_choice_f2 == "replace_with_na") {
                  outliers_count_string <- paste(outliers_count_string, " The outliers were replaced with NA. ")
                } else if (replacement_choice_f2 == "replace_with_average") {
                  outliers_count_string <- paste(outliers_count_string, " The outliers were replaced with the average value of the variable. ")
                }
                outliers_string <- "Outliers: "
                counter = 1
                for(item in iqr_outliers_f$outliers_names_list) {
                  item_name <- names(iqr_outliers_f$outliers_names_list)[counter]
                  outliers_string <- paste(outliers_string, item_name, " (", sep="")
                  outliers_string <- paste(outliers_string, vector_to_comma_separated_character(item), ")", sep="")
                  if(counter < length(iqr_outliers_f$outliers_names_list)) {
                    outliers_string <- paste(outliers_string, ", ", sep="")
                  }
                  counter <- counter + 1
                }
                report_string <- paste(report_string, outliers_count_string, outliers_string)
              } else {
                report_string <- paste(report_string, "No outliers was found")
              }
              return(report_string)
            }
            
            iqr_outliers <- iqr_find_and_replace_outliers(data_f, selected_variables_f, replacement_choice_f)

            iqr_report <- get_iqr_report_string(iqr_outliers, replacement_choice_f)
            message_out(iqr_report, "IQR filtering")
            if(iqr_outliers$outliers_count > 0) {
              theObject@t@df <- matrix_to_t1_format(iqr_outliers$new_matrix)
              theObject@iqr_done <- TRUE
            }
            
            return(theObject)

          }
)

