# _Class for performing checks on data loaded from CSV files and generating summaries of thed data

library(dplyr)

input_data_checker <- setClass("input_data_checker", 
                            
                            slots = c(mydata_csv = "data.frame", class_labels_csv = "data.frame", summary = "list", t_merged_frame = "data.frame", log_transformed = "logical", log_base = "numeric"),
                            
                            prototype=list(
                              mydata_csv = NULL, class_labels_csv = NULL, summary = list(), t_merged_frame = NULL, log_transformed = FALSE, log_base = NULL
                            ))

setGeneric(name="check_data_frame",
           def=function(theObject, first_col_labels, minimum_rows, minimum_columns)
           {
             standardGeneric("check_data_frame")
           }
)

setMethod(f="check_data_frame",
          signature="input_data_checker",
          definition=function(theObject, first_col_labels, minimum_rows, minimum_columns)
          {
            # _This function checks if a data frame with data entries loaded from a csv file is OK for further steps
            mydata_csv <- theObject@mydata_csv
            
            out <- list()
            error_message <- ""
            mydata <- list()
            data_frame_loading_failed <- FALSE
            col_count <- 0
            
            if (is.null(mydata_csv) == FALSE) {
              # _Proceed if the data file is not empty
              if(is.data.frame(mydata_csv) == FALSE) {
                error_message <- paste(error_message, "Error: the input is not a data frame.", sep = " ")
                data_frame_loading_failed <- TRUE
              }
              if(ncol(mydata_csv) < minimum_columns) {
                col_message <- paste("Error: too few columns in data file (at least ", minimum_columns, " is required)", sep="")
                error_message <- paste(error_message, col_message, sep = " ")
                data_frame_loading_failed <- TRUE
              }
              if(nrow(mydata_csv) < minimum_rows) {
                row_message <- paste("Error: too few rows in data file (at least ", minimum_rows, " is required)", sep="")
                error_message <- paste(error_message, row_message, sep = " ")
                data_frame_loading_failed <- TRUE
              }
              # _Proceed if the data file can be loaded
              
              if(!data_frame_loading_failed) {
                if(first_col_labels == TRUE) {
                  mydata <- mydata_csv[, 2:ncol(mydata_csv)]
                } else {
                  mydata <- mydata_csv[, 1:ncol(mydata_csv)]
                }
                if (is.null(mydata) == TRUE || is.na(mydata) == TRUE) {
                  error_message <- paste(error_message, "Error: no data in data file.", sep = " ")
                  data_frame_loading_failed <- TRUE
                }
                col_count <- ncol(mydata)
              }
            } else {
              error_message <- paste(error_message, "Error: no data in data file.", sep = " ")
              data_frame_loading_failed <- TRUE
            }
            result <- TRUE
            if(data_frame_loading_failed) {
              result <- FALSE
            }
            out$result <- result
            out$error_message <- error_message
            out$col_count <- col_count
            return(out)
          }
)

setGeneric(name="get_class_labels_from_data_file",
           def=function(theObject, col_labels_used, col_label_separator)
           {
             standardGeneric("get_class_labels_from_data_file")
           }
)

setMethod(f="get_class_labels_from_data_file",
          signature="input_data_checker",
          definition=function(theObject, col_labels_used, col_label_separator)
          {
            # _Extracts class labels from the data file
            mydata_csv <- theObject@mydata_csv
            class_labels_frame <- data.frame()
            out <- list()
            error_message <- ""
            data_frame_loading_failed <- FALSE
            loaded_data <- mydata_csv
            
            class_label_from_col <- colnames(loaded_data, do.NULL = TRUE)
            
            column_labels_vector <- c()
            class_labels_vector <- c()
            
            if(length(class_label_from_col) > 2)  {
              counter_f <- 0
              for (i in class_label_from_col) {
                
                if(col_labels_used) {
                  i_split <- unlist(strsplit(i, col_label_separator, fixed = TRUE))
                  column_label <- ""
                  class_label <- ""
                  if(length(i_split) > 1) {
                    column_label <- (i_split[-1])
                    class_label <- (i_split[1: length(i_split)-1])
                    
                  } else if(length(i_split) == 1) {
                    column_label <- i_split
                    class_label <- i_split
                  }
                  if (column_label == "" || class_label == "") {
                    data_frame_loading_failed <- TRUE
                    error_message <- "Error: could not extract class labels from the data file."
                  } else {
                    column_labels_vector <- append(column_labels_vector, column_label)
                    class_labels_vector <- append(class_labels_vector, class_label)
                  }
                } else {
                  if(counter_f == 0) {
                    column_label <- "X"
                  } else {
                    column_label <- toString(counter_f)
                  }
                  class_label <- i
                  class_labels_vector <- append(class_labels_vector, class_label)
                  column_labels_vector <- append(column_labels_vector, column_label)
                }
                
                counter_f <- counter_f + 1
                
              }
              
              column_labels_title <- column_labels_vector[1]
              class_labels_title <- class_labels_vector[1]
              if (length(column_labels_vector) > 1 && length(class_labels_vector) > 1) {
                
                column_labels_vector <- column_labels_vector[2:length(column_labels_vector)]
                class_labels_vector <- class_labels_vector[2:length(class_labels_vector)]
              }
              
              class_labels_frame <- data.frame(column_labels_vector, class_labels_vector)
              names(class_labels_frame) <- c(column_labels_title,class_labels_title)
              
            } else {
              data_frame_loading_failed <- TRUE
              error_message <- "Error: could not extract class labels from the data file."
            }
            
            if(is.null(class_labels_frame)) {
              data_frame_loading_failed <- TRUE
              error_message <- "Error: could not extract class labels from the data file."
            }
            
            out$data_frame <- class_labels_frame
            out$error_occurred <- data_frame_loading_failed
            out$error_message <- error_message
            
            return(out)
          }
)

setGeneric(name="check_class_labels_file",
           def=function(theObject, no_of_data_file_cols)
           {
             standardGeneric("check_class_labels_file")
           }
)

setMethod(f="check_class_labels_file",
          signature="input_data_checker",
          definition=function(theObject, no_of_data_file_cols)
          {
            # _This function checks if a data frame containing class labels is OK to be used in further steps
            class_labels_csv <- theObject@class_labels_csv
            out <- list()
            error_message <- ""
            
            data_frame_loading_failed <- FALSE
            
            if ((is.null(class_labels_csv) == FALSE) & (is.data.frame(class_labels_csv) == TRUE)) {
              # _Proceed if class labels exist
              class_labels_csv_vector <- c()
              
              mtry <- try(as.vector(class_labels_csv[,2]), 
                          silent <- TRUE)
              
              if (class(mtry) != "try-error") {
                class_labels_csv_vector <- as.vector(class_labels_csv[,2])
              } else {
                data_frame_loading_failed <- TRUE
              }
              
              if(data_frame_loading_failed == FALSE) {
                # _Proceed if class labels can be loaded
                
                if(no_of_data_file_cols == length(class_labels_csv_vector)) {
                  # _Proceed if the number of class labels matches the number of data columns
                  
                  if ((is.null(class_labels_csv_vector) == TRUE) || (is.na(class_labels_csv_vector) == TRUE)) {
                    # _Proceed if the loaded data and class labels are not empty
                    error_message <- paste(error_message, "Error: could not find unique class labels.", sep = " ")
                    data_frame_loading_failed <- TRUE
                  }
                } else {
                  error_message <- paste(error_message, "Error: the number of items in class labels file does not match the data file.", sep = " ")
                  data_frame_loading_failed <- TRUE
                }
              }
            } else {
              error_message <- paste(error_message, "Error: the input is not a data frame.", sep = " ")
              data_frame_loading_failed <- TRUE
            }
            result <- TRUE
            if(data_frame_loading_failed) {
              result <- FALSE
            }
            out$result <- result
            out$error_message <- error_message
            return(out)
          }
)

setGeneric(name="data_loading_error_report_for_output",
           def=function(theObject, table_data_file_check_f, class_labels_extraction_report_f, table_class_labels_check_f)
           {
             standardGeneric("data_loading_error_report_for_output")
           }
)

setMethod(f="data_loading_error_report_for_output",
          signature="input_data_checker",
          definition=function(theObject, table_data_file_check_f, class_labels_extraction_report_f, table_class_labels_check_f)
          {
            # _Report after loading data file
            error_report <- NULL
            if((!table_data_file_check_f$result) | (!table_class_labels_check_f$result)) {
              # _Displays an error message if data loading results in an error
              
              error_report <- ("The specified file(s) could not be loaded. Please make sure the file format is correct.\n")
              if(table_data_file_check_f$result == FALSE) {
                error_report <- paste0(error_report, table_data_file_check_f$error_message)
              }
              if(table_class_labels_check_f$result == FALSE) {
                error_report <- paste0(error_report, table_class_labels_check_f$error_message)
              }
            }
            
            if(class_labels_extraction_report_f$error_occurred) {
              error_report <- paste0(error_report, class_labels_extraction_report_f$error_message)
            }
            return(error_report)
          }
)

setGeneric(name="get_class_labels_table",
           def=function(theObject)
           {
             standardGeneric("get_class_labels_table")
           }
)

setMethod(f="get_class_labels_table",
          signature="input_data_checker",
          definition=function(theObject)
          {
            class_labels_f <- theObject@class_labels_csv
            
            
            class_labels_list <- class_labels_f[, 2]
            class_labels_list <- class_labels_list[1:length(class_labels_list), drop = TRUE]
            
            unique_class_labels <- unique(class_labels_f[, 2])
            class_labels_table <- table(class_labels_list[class_labels_list %in% unique_class_labels])
            
            return(class_labels_table)
          }
)

setGeneric(name="summarise_data_frame_statistics",
           def=function(theObject)
           {
             standardGeneric("summarise_data_frame_statistics")
           }
)

setMethod(f="summarise_data_frame_statistics",
          signature="input_data_checker",
          definition=function(theObject)
          {
            # _Finds summary statistics of a data frame to preview to the user when loading data
            
            data_f <- theObject@mydata_csv
            class_labels_f <- theObject@class_labels_csv
            
            
            summary_f <- list()
            summary_f$ncol_data <- ncol(data_f)-1
            summary_f$nrow_data <- nrow(data_f)
            
            summary_f$number_of_unique_class_labels <- length(unique(class_labels_f[, 2]))
            
            
            
            class_labels_table <- get_class_labels_table(theObject)
            
            
            class_labels_counted <- ""
            counter_f <- 0
            
            for (c_name in names(class_labels_table)) {
              class_labels_counted <- paste(class_labels_counted, c_name, " (", class_labels_table[c_name], ")", sep="")
              if(counter_f < (length(class_labels_table)-1)) {
                class_labels_counted <- paste(class_labels_counted, ", ", sep="")
              }
              counter_f <- counter_f + 1
            }
            
            summary_f$class_labels_with_column_counts <- class_labels_counted
            
            summary_f$not_na <- colSums(!is.na(data_f))
            summary_f$is_na <- colSums(is.na(data_f))
            
            
            return(summary_f)
          }
)

setGeneric(name="summarise_data_frame_statistics_by_variable",
           def=function(theObject)
           {
             standardGeneric("summarise_data_frame_statistics_by_variable")
           }
)

setMethod(f="summarise_data_frame_statistics_by_variable",
          signature="input_data_checker",
          definition=function(theObject)
          {
            # _Finds summary statistics of a data frame to preview to the user when loading data
            data_f <- theObject@t_merged_frame
            
            data_f_cropped <- data_f[2:nrow(data_f),3:ncol(data_f)]
            summary_f <- list()
            
            summary_f$not_na <- colSums(!is.na(data_f_cropped))
            summary_f$is_na <- colSums(is.na(data_f_cropped))
            summary_f$min <- summarise_each(data_f_cropped, funs(min(., na.rm=TRUE)))
            summary_f$max <- summarise_each(data_f_cropped, funs(max(., na.rm=TRUE)))
            
            data_f_cropped_numeric <- data.matrix(data_f_cropped, rownames.force = NA)
            summary_f$mean <- colMeans(data_f_cropped_numeric)
            
            return(summary_f)
          }
)

setGeneric(name="prepare_table_statistics_list_for_printing_html",
           def=function(theObject)
           {
             standardGeneric("prepare_table_statistics_list_for_printing_html")
           }
)

setMethod(f="prepare_table_statistics_list_for_printing_html",
          signature="input_data_checker",
          definition=function(theObject)
          {
            # _Prepares summary statistics of the loaded data table for html text field output
            summary_f <- theObject@summary
            printing_string <- ("")
            for (name in names(summary_f)) {
              title_string <- ""
              name_string <- toString(name)
              if(is.element(name_string, c("ncol_data", "nrow_data", "number_of_unique_class_labels", "class_labels_with_column_counts"))) {
                if(name_string == "ncol_data") {
                  title_string <- "Number of samples (columns)"
                }
                if(name_string == "nrow_data") {
                  title_string <- "Number of variables (rows)"
                }
                if(name_string == "number_of_unique_class_labels") {
                  title_string <- "Number of unique class labels"
                }
                if(name_string == "class_labels_with_column_counts") {
                  title_string <- "Unique class labels (number of columns in the class)"
                }
                printing_string <- paste(printing_string, "<b>", title_string, ": </b>", sep="")
                printing_string <- paste(printing_string, toString(summary_f[[name]]), "<br>", sep="")
              }
              
            }
            return(printing_string)
          }
)

setGeneric(name="prepare_table_statistics_data_frame_by_sample",
           def=function(theObject)
           {
             standardGeneric("prepare_table_statistics_data_frame_by_sample")
           }
)

setMethod(f="prepare_table_statistics_data_frame_by_sample",
          signature="input_data_checker",
          definition=function(theObject)
          {
            # _Prepares summary statistics of the loaded data table's columns for output as a table
            summary_f <- theObject@summary
            header_vector <- c("not_na", "is_na")
            number_of_rows <- length(summary_f$not_na)-1
            
            output_frame <- data.frame(matrix(ncol = length(header_vector), nrow = number_of_rows))
            colnames(output_frame) <- header_vector
            
            title_string <- ""
            for (name in names(summary_f)) {
              name_string <- toString(name)
              if(is.element(name_string, header_vector)) {
                
                data_for_col <- c(unlist(summary_f[[name]]))
                data_for_col <- data_for_col[2: length(data_for_col)]
                
                output_frame[,name] <- data_for_col
              }
            }
            row_titles <- names(summary_f$not_na)
            row_titles <- row_titles[2:length(row_titles)]
            rownames(output_frame) <- row_titles
            return(output_frame)
          }
)

setGeneric(name="prepare_table_statistics_data_frame_by_variable",
           def=function(theObject, data_summary_by_variable_f, row_titles_f)
           {
             standardGeneric("prepare_table_statistics_data_frame_by_variable")
           }
)

setMethod(f="prepare_table_statistics_data_frame_by_variable",
          signature="input_data_checker",
          definition=function(theObject, data_summary_by_variable_f, row_titles_f)
          {
            # _Prepares summary statistics of the loaded data table's rows for output as a table
            
            summary_f <- data_summary_by_variable_f
              
            header_vector <- c("not_na", "is_na", "min", "max", "mean")
            number_of_rows <- length(summary_f$mean)
            
            output_frame <- data.frame(matrix(ncol = length(header_vector), nrow = number_of_rows))
            colnames(output_frame) <- header_vector
            
            title_string <- ""
            for (name in names(summary_f)) {
              name_string <- toString(name)
              if(is.element(name_string, header_vector)) {
                data_for_col <- c(unlist(summary_f[[name]]))
                output_frame[,name] <- data_for_col
              }
            }
            row_titles <- row_titles_f
            rownames(output_frame) <- row_titles
            return(output_frame)
          }
)

