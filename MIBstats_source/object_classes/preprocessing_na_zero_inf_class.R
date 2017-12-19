# _This class is used for the data preprocessing step that handles NA, zero and infinite values in input data

preprocessing_na_zero_inf <- setClass("preprocessing_na_zero_inf",
                             slots = c(t = "input_data_table"),
                             
                             prototype=list(
                               t = input_data_table()
                             )
)

setGeneric(name="replace_values_in_df",
           def=function(theObject, replacement_function_f)
           {
             standardGeneric("replace_values_in_df")
           }
)

setMethod(f="replace_values_in_df",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject, replacement_function_f)
            
          # _This method replaces data values in an input data frame. The replacement function is specified as an argument for this method.
          # _The method returns the modified data frame. It also returns a list of indexes in the data frame where the changes were made (for generating a report)
          {
            output_list_f <- list()
            df_f <- theObject@t@df
            
            processed_df_f <- df_f[,2:ncol(df_f)]
            processed_df_f <- as.data.frame(lapply(processed_df_f, replacement_function_f))
            processed_df_f <- cbind(df_f[,1], processed_df_f)
            
            colnames(processed_df_f) <- colnames(df_f)
            
            modified_indexes <- list()
            
            for (i in 1:nrow(df_f)) {
              df_row <- df_f[i, ]
              processed_df_row <- processed_df_f[i, ]
              if(!isTRUE(all.equal(df_row, processed_df_row))) {
                
                for (j in 2:ncol(df_f)) {
                  df_cell <- df_f[i,j]

                  processed_df_cell <- processed_df_f[i,j]
                  if((is.na(df_cell) & !is.na(processed_df_cell)) | (!is.na(df_cell) & is.na(processed_df_cell))) {
                    modified_indexes <- append(modified_indexes, list(c(i, j)))
                  }
                  if(!is.na(df_cell) & !is.na(processed_df_cell)) {
                    if((as.numeric(df_cell) == as.numeric(processed_df_cell)) == FALSE) {
                      modified_indexes <- append(modified_indexes, list(c(i, j)))
                    }
                  }
                }
              }
            }
            output_list_f$df <- processed_df_f
            output_list_f$modified_indexes <- modified_indexes
            return(output_list_f)
          }
)

setGeneric(name="replace_values_with_na",
           def=function(theObject, value_to_be_replaced_f)
           {
             standardGeneric("replace_values_with_na")
           }
)

setMethod(f="replace_values_with_na",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject, value_to_be_replaced_f)
            
          # _Method to replace values in a data frame with NA
          {
            replacement_f <- function(x){replace(x, x == value_to_be_replaced_f, NA)}
            new_df <- replace_values_in_df(theObject, replacement_f)
            return(new_df)
          }
)

setGeneric(name="replace_values_with_zero",
           def=function(theObject, value_to_be_replaced_f)
           {
             standardGeneric("replace_values_with_zero")
           }
)

setMethod(f="replace_values_with_zero",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject, value_to_be_replaced_f)
            
            # _Method to replace values in a data frame with zero
          {
            replacement_f <- NULL
            if(value_to_be_replaced_f == "<NA>") {
              replacement_f <- function(x){replace(x, is.na(x), 0)}
            } else {
              replacement_f <- function(x){replace(x, x == value_to_be_replaced_f, 0)} 
            }
            new_df <- replace_values_in_df(theObject, replacement_f)
            return(new_df)
          }
)

setGeneric(name="replace_values_with_detection_limit",
           def=function(theObject, value_to_be_replaced_f, detection_limit_f)
           {
             standardGeneric("replace_values_with_detection_limit")
           }
)

setMethod(f="replace_values_with_detection_limit",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject, value_to_be_replaced_f, detection_limit_f)
            
          # _Method to replace values in a data frame with an estimated detection limit of the measuring instrument
          {
            replacement_f <- NULL
            if(value_to_be_replaced_f == "<NA>") {
              replacement_f <- function(x){replace(x, is.na(x), detection_limit_f)}
            } else {
              replacement_f <- function(x){replace(x, x == value_to_be_replaced_f, detection_limit_f)} 
            }
            new_df <- replace_values_in_df(theObject, replacement_f)
            return(new_df)
          }
)

setGeneric(name="remove_variables_with_high_na_percentage",
           def=function(theObject, maximum_na_allowed_f)
           {
             standardGeneric("remove_variables_with_high_na_percentage")
           }
)

setMethod(f="remove_variables_with_high_na_percentage",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject, maximum_na_allowed_f)
          # _Method to remove variables from a data frame if the percentage of NA values exceeds a limit
          {
            output_list <- list()
            df_f <- theObject@t@df
            
            get_row_na_percentage <- function(df_row) {
              
              df_row_values <- unlist(df_row[2:length(df_row)])
              na_count <- sum(is.na(df_row_values))
              na_percentage <- (na_count/length(df_row_values))*100
              return(na_percentage)
            }
            
            na_percentages_vector <- c()
            for(i in 1:nrow(df_f)) {
              na_percentages_vector <- c(na_percentages_vector, get_row_na_percentage(df_f[i, ]))
            }
            
            over_na_limit <- which(na_percentages_vector>maximum_na_allowed_f)
            
            new_df <- df_f
            
            if(length(over_na_limit)>0) {
              new_df <- df_f[-over_na_limit,]
              
            }
            
            output_list$df <- new_df
            output_list$number_of_deleted_rows <- length(over_na_limit)
            output_list$deleted_row_indexes <- over_na_limit
            
            return(output_list)
          }
)

setGeneric(name="get_df_minimum_value",
           def=function(theObject)
           {
             standardGeneric("get_df_minimum_value")
           }
)

setMethod(f="get_df_minimum_value",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject)
          # _Method to find the minimum value of the input data
          {
            minimum_value_f <- NA
            df_f <- theObject@t@df
            df_f_data <- df_f[,2:ncol(df_f)]
            replacement_f1 <- function(x){replace(x, x == -Inf, NA)}
            df_f_data <- as.data.frame(lapply(df_f_data, replacement_f1))
            minimum_value_f <- min(df_f_data, na.rm=T)
            return(minimum_value_f)
          }
)

setGeneric(name="get_df_maximum_value",
           def=function(theObject)
           {
             standardGeneric("get_df_maximum_value")
           }
)

setMethod(f="get_df_maximum_value",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject)
          # _Method to find the maximum value of the input data
          {
            maximum_value_f <- NA
            df_f <- theObject@t@df
            df_f_data <- df_f[,2:ncol(df_f)]
            replacement_f1 <- function(x){replace(x, x == Inf, NA)}
            df_f_data <- as.data.frame(lapply(df_f_data, replacement_f1))
            maximum_value_f <- max(df_f_data, na.rm=T)
            return(maximum_value_f)
          }
)

setGeneric(name="run_na_zero_inf_preprocessing",
           def=function(theObject, preproc_NA_zero_inf_f, preproc_limits_tracker_f)
           {
             standardGeneric("run_na_zero_inf_preprocessing")
           }
)

setMethod(f="run_na_zero_inf_preprocessing",
          signature="preprocessing_na_zero_inf",
          definition=function(theObject, preproc_NA_zero_inf_f, preproc_limits_tracker_f)
          # _Method to run the preprocessing to handle NA, zero and infinite values. This method is used both for previewing the changes and for making the changes to the input data
          {
            
            prepare_modification_marks_frame <- function(df_f2, all_modified_indexes2, deleted_row_indexes_f2) {
              modification_marks_frame <- as.data.frame(matrix(0, ncol = ncol(df_f2), nrow = nrow(df_f2)))
              colnames(modification_marks_frame) <- colnames(df_f2)
              rownames(modification_marks_frame) <- rownames(df_f2)
              for(item_f in all_modified_indexes2) {
                modification_marks_frame[item_f[1], item_f[2]] <- 1
              }
              if(length(deleted_row_indexes_f2)>0) {
                modification_marks_frame <- modification_marks_frame[-deleted_row_indexes_f2,]
              }
              return(modification_marks_frame)
            }
            
            output_list_f <- list()
            df_f <- theObject@t@df
            report_f <- NULL
            
            zeros_count_f <- count_occurrencies_in_data_frame(df_f, "0", TRUE)
            
            all_modified_indexes <- list()
            deleted_row_indexes_f <- NULL
            
            if(zeros_count_f>0) {
              report_entry <- ""
              replacement_list <- list()
              if(preproc_NA_zero_inf_f@zero_values_choice == 1) {
                # _Replace with NAs
                replacement_list <- replace_values_with_na(theObject, 0)
                report_entry <- paste("Substituted ", zeros_count_f, " zero value(s) with NA.", sep="")
              } else if (preproc_NA_zero_inf_f@zero_values_choice == 2) {
                # _Substitute with detection limit
                replacement_list <- replace_values_with_detection_limit(theObject, 0, preproc_limits_tracker_f$minus_inf)
                report_entry <- paste("Substituted ", zeros_count_f, " zero value(s) with lower detection limit.", sep="")
              }
              if(length(replacement_list)>0) {
                theObject@t@df <- replacement_list$df
                all_modified_indexes <- append(all_modified_indexes, replacement_list$modified_indexes)
              }
              report_f <- c(report_f, report_entry)
            }
             
            minus_inf_count_f <- count_occurrencies_in_data_frame(df_f, "-Inf", TRUE)
            
            if(minus_inf_count_f>0) {
              report_entry <- ""
              replacement_list <- list()
              if(preproc_NA_zero_inf_f@minus_inf_values_choice == 1) {
                # _Replace with NAs
                replacement_list <- replace_values_with_na(theObject, -Inf)
                report_entry <- paste("Substituted ", minus_inf_count_f, " -Inf value(s) with NA.", sep="")
              } else if (preproc_NA_zero_inf_f@minus_inf_values_choice == 2) {
                # _Substitute with detection limit
                replacement_list <- replace_values_with_detection_limit(theObject, -Inf, preproc_limits_tracker_f$minus_inf)
                report_entry <- paste("Substituted ", minus_inf_count_f, " -Inf value(s) with lower detection limit.", sep="")
              }
              if(length(replacement_list)>0) {
                theObject@t@df <- replacement_list$df
                all_modified_indexes <- append(all_modified_indexes, replacement_list$modified_indexes)
              }
              report_f <- c(report_f, report_entry)
            }
            
            plus_inf_count_f <- count_occurrencies_in_data_frame(df_f, "Inf", TRUE)
            if(plus_inf_count_f>0) {
              report_entry <- ""
              replacement_list <- list()
              if(preproc_NA_zero_inf_f@plus_inf_values_choice == 1) {
                # _Replace with NAs
                replacement_list <- replace_values_with_na(theObject, Inf)
                report_entry <- paste("Substituted ", plus_inf_count_f, " Inf value(s) with NA.", sep="")
              } else if (preproc_NA_zero_inf_f@plus_inf_values_choice == 2) {
                # _Substitute with detection limit
                replacement_list <- replace_values_with_detection_limit(theObject, Inf, preproc_limits_tracker_f$plus_inf)
                report_entry <- paste("Substituted ", plus_inf_count_f, " Inf value(s) with upper detection limit.", sep="")
                
              }
              if(length(replacement_list)>0) {
                theObject@t@df <- replacement_list$df
                all_modified_indexes <- append(all_modified_indexes, replacement_list$modified_indexes)
              }
              report_f <- c(report_f, report_entry)
            }
            
            na_count_f <- sum(is.na(df_f[,2:ncol(df_f)]))
            if(na_count_f>0) {
              report_entry <- ""
              if(preproc_NA_zero_inf_f@NA_values_choice == 1) {
                remove_na_result <- remove_variables_with_high_na_percentage(theObject, preproc_NA_zero_inf_f@NA_values_threshold)
                
                if(remove_na_result$number_of_deleted_rows > 0) {
                  theObject@t@df <- remove_na_result$df
                  deleted_row_indexes_f <- remove_na_result$deleted_row_indexes
                  report_entry <- paste("Removed ", remove_na_result$number_of_deleted_rows, " variable(s) that contained more NA values than allowed.", sep="")
                  report_f <- c(report_f, report_entry)
                }
              } else if(preproc_NA_zero_inf_f@NA_values_choice == 2) {
                # _substitute with zeros
                remove_na_result <- replace_values_with_zero(theObject, "<NA>")
                theObject@t@df <- remove_na_result$df
                all_modified_indexes <- append(all_modified_indexes, remove_na_result$modified_indexes)
                report_entry <- paste("Substituted ", na_count_f, " NA value(s) with zero.", sep="")
                
              } else if(preproc_NA_zero_inf_f@NA_values_choice == 3) {
                # _substitute with minimum detected value
                remove_na_result <- replace_values_with_detection_limit(theObject, "<NA>", preproc_limits_tracker_f$minus_inf)
                theObject@t@df <- remove_na_result$df
                all_modified_indexes <- append(all_modified_indexes, remove_na_result$modified_indexes)
                report_entry <- paste("Substituted ", na_count_f, " NA value(s) with minimum detected value.", sep="")
                
              } else if(preproc_NA_zero_inf_f@NA_values_choice == 4) {
                # _substitute with maximum detected value
                remove_na_result <- replace_values_with_detection_limit(theObject, "<NA>", preproc_limits_tracker_f$plus_inf)
                theObject@t@df <- remove_na_result$df
                all_modified_indexes <- append(all_modified_indexes, remove_na_result$modified_indexes)
                report_entry <- paste("Substituted ", na_count_f, " NA value(s) with maximum detected value.", sep="")
                
              }
              report_f <- c(report_f, report_entry)
            }
            
            if(is.null(report_f)) {
              report_f <- "No changes were made to the data."
            }
            
            report_char <- ""
            for(item_f in report_f) {
              report_char <- paste(report_char, item_f, "<br>", sep="")
            }
            
            output_list_f$t <- theObject@t
            output_list_f$report <- report_char
            

            modification_marks_frame <- prepare_modification_marks_frame(df_f, all_modified_indexes, deleted_row_indexes_f)
            output_list_f$modification_marks_frame <- modification_marks_frame
            return(output_list_f)
          }
)
