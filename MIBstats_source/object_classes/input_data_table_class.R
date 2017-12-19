# _A class for storing loaded data in the app.
# _the main object from this class in this app is the variable t1$a in the shared namespace of the server
# _Objects from the input_data_table class are used for the communication of data between modules in this app
# _The slot df contains a data frame with data values
# _The slot class_labels contains a data frame with class labels
# _The slot table_statistics contains statistics on the rows and columns of the data (for displaying it after loading the data)
# _The slot subsets contains a list of data subsets (assigned on the two groups test setup page)
# _The slot log_transformed marks whether the data was already log transformed before the user loaded it to the app
# _the slot log_base stores the base of the logarithm if the data is log-transformed

input_data_table <- setClass("input_data_table",
                             slots = c(table_statistics = "list", class_labels = "data.frame", df = "data.frame", subsets = "list", log_transformed = "logical", log_base = "numeric", item_name = "character"),
                             
                             prototype=list(
                               table_statistics = list(),
                               class_labels = NULL,
                               df = NULL,
                               log_transformed = FALSE,
                               log_base = NULL,
                               subsets = list()
                             ),
                             
                             validity=function(object)
                             {
                               if(!is.null(object@df) & !is.data.frame(object@df)) {
                                 return("The data frame for input data is not in the correct format")
                               }
                               return(TRUE)
                             }
)

setGeneric(name="set_table_statistics",
           def=function(theObject,xVal)
           {
             standardGeneric("set_table_statistics")
           }
)

setMethod(f="set_table_statistics",
          signature="input_data_table",
          definition=function(theObject,xVal)
            
            # _assigns a value to the table statistics slot
          {
            theObject@table_statistics <- xVal
            return(theObject)
          }
)

setGeneric(name="get_table_statistics",
           def=function(theObject)
           {
             standardGeneric("get_table_statistics")
           }
)

setMethod(f="get_table_statistics",
          signature="input_data_table",
          definition=function(theObject)
            
            # gets a value to the table statistics slot
          {
            return(theObject@table_statistics)
          }
)

setGeneric(name="rearrange_input_data_columns_based_on_sample_pairing",
           def=function(theObject, pairings_f)
           {
             standardGeneric("rearrange_input_data_columns_based_on_sample_pairing")
           }
)

setMethod(f="rearrange_input_data_columns_based_on_sample_pairing",
          signature="input_data_table",
          definition=function(theObject, pairings_f)
            
            # _Method for rearranging the order of columns in the data if the user manually changes the pairings of samples on the two groups test setup page
          {
            
            t1_f <- theObject
            
            
            new_col_index_vector <- c(1)
            new_class_labels_index_vector <- c()
            
            t1_f_cropped_df <- t1_f@df[, 2:ncol(t1_f@df)]
            
            for(pairings_list_i in 1:2) {
              for(pairing_item in pairings_f[[pairings_list_i]]) {
                
                ind_g <- which(names(t1_f@df) == pairing_item)
                ind_g2 <- which(names(t1_f_cropped_df) == pairing_item)
                
                new_col_index_vector <- c(new_col_index_vector, ind_g)
                new_class_labels_index_vector <- c(new_class_labels_index_vector, ind_g2)
              }
            }
            
            missing_columns <- c()
            for(df_i in 1:ncol(t1_f@df)) {
              if(!is.element(df_i, new_col_index_vector)) {
                missing_columns <- c(missing_columns, df_i)
              }
            }
            
            new_col_index_vector <- c(new_col_index_vector, missing_columns)
            
            missing_columns2 <- c()
            for(df_i in 1:ncol(t1_f_cropped_df)) {
              if(!is.element(df_i, new_class_labels_index_vector)) {
                missing_columns2 <- c(missing_columns2, df_i)
              }
            }
            new_class_labels_index_vector <- c(new_class_labels_index_vector, missing_columns2)
            t1_f_new_df <- t1_f@df[, new_col_index_vector]
            t1_f_new_class_labels <- t1_f@class_labels[new_class_labels_index_vector, ]
            
            
            theObject@df <- t1_f_new_df
            theObject@class_labels <- t1_f_new_class_labels
            return(theObject)
          }
)

setGeneric(name="filter_t1_based_on_subsets",
           def=function(theObject, to_be_deleted_vector)
           {
             standardGeneric("filter_t1_based_on_subsets")
           }
)

setMethod(f="filter_t1_based_on_subsets",
          signature="input_data_table",
          definition=function(theObject, to_be_deleted_vector)
            
          # _removes samples that do not belong to the subset (that is defined by the user on the two groups test setup page)
          {
            t1_f <- theObject
            class_labels_cl2 <- names(t1_f@df[1, ])
            
            t1_f_df2 <- t1_f@df[ , !(names(t1_f@df) %in% to_be_deleted_vector)]
            
            
            new_class_labels <- t1_f@class_labels
            
            if(!is.null(to_be_deleted_vector)) {
              indexes_to_be_deleted <- match(to_be_deleted_vector, class_labels_cl2[2:length(class_labels_cl2)])
              new_class_labels <- new_class_labels[-indexes_to_be_deleted, ]
            }
            
            
            theObject@df <- t1_f_df2
            theObject@class_labels <- new_class_labels
            return(theObject)
          }
)

setGeneric(name="get_columns_to_be_deleted_based_on_subsets",
           def=function(theObject, table_row_cl_f)
           {
             standardGeneric("get_columns_to_be_deleted_based_on_subsets")
           }
)

setMethod(f="get_columns_to_be_deleted_based_on_subsets",
          signature="input_data_table",
          definition=function(theObject, table_row_cl_f)
            
            # _Finds the columns that need to be deleted to keep only a subset of the data (the subset is defined by the user on the two groups test setup page)
          {
            sample_to_be_deleted <- function(class_label_f, sample_name_f, group_cl_f, valid_samples_vector_f) {
              to_be_deleted <- FALSE
              if(is.element(class_label_f, group_cl_f$g1) || is.element(class_label_f, group_cl_f$g2)) {
                element_index <- NA
                subgroup_label <- ""
                if(is.element(class_label_f, group_cl_f$g1)) {
                  element_index <- which(group_cl_f$g1 == class_label_f)
                  subgroup_label <- names(group_cl_f$g1)[[element_index]]
                } else if(is.element(class_label_f, group_cl_f$g2)) {
                  element_index <- which(group_cl_f$g2 == class_label_f)
                  subgroup_label <- names(group_cl_f$g2)[[element_index]]
                }
                if(subgroup_label != "None") {
                  if(!is.element(sample_name_f, valid_samples_vector_f)) {
                    to_be_deleted <- TRUE
                  }
                }
              }
              return(to_be_deleted)
            }
            
            merged_frame_f <- get_merged_frame(theObject)
            t1_f <- theObject
            
            
            table_cl_groups1 <- toString(table_row_cl_f$groups1)
            table_cl_groups2 <- toString(table_row_cl_f$groups2)
            
            
            group_cl <- list()
            
            group_cl$g1 <- unlist(strsplit(table_cl_groups1, " "))
            group_cl$g2 <- unlist(strsplit(table_cl_groups2, " "))
            
            
            class_labels_cl <- (merged_frame_f[2, ])
            
            table_cl_subset_g1 <- toString(table_row_cl_f$subset_g1)
            table_cl_subset_g2 <- toString(table_row_cl_f$subset_g2)
            
            
            subsets_list <- list()
            
            subsets_list$g1 <- unlist(strsplit(table_cl_subset_g1, " "))
            subsets_list$g2 <- unlist(strsplit(table_cl_subset_g2, " "))
            
            names(group_cl$g1) <- subsets_list$g1
            names(group_cl$g2) <- subsets_list$g2
            
            
            
            valid_samples_vector <- c()
            for(data_group in group_cl) {
              counter_f <- 1
              for(group_item in data_group) {
                subset_name <- names(data_group)[[counter_f]]
                if(subset_name != "None") {
                  samples <- get_sample_names_by_subset_name(t1_f, subset_name)
                  valid_samples_vector <- c(valid_samples_vector, samples)
                }
              }
              counter_f <- counter_f + 1
            }
            
            
            
            col_counter <- 1
            to_be_deleted_vector <- c()
            for(table_col in class_labels_cl) {
              if(col_counter > 1) {
                sample_name <- names(class_labels_cl)[[col_counter]]
                to_be_deleted <- sample_to_be_deleted(table_col, sample_name, group_cl, valid_samples_vector)
                if(to_be_deleted) {
                  to_be_deleted_vector <- c(to_be_deleted_vector, sample_name)
                }
              }
              col_counter <- col_counter + 1
            }
            
            return(to_be_deleted_vector)
          }
)

setGeneric(name="get_sample_names_by_subset_name",
           def=function(theObject, subset_selection_name_f)
           {
             standardGeneric("get_sample_names_by_subset_name")
           }
)

setMethod(f="get_sample_names_by_subset_name",
          signature="input_data_table",
          definition=function(theObject, subset_selection_name_f)
            
            # _Gets the names of samples that belong to a subset
          {
            samples_in_subset_f <- c()
            subsets_list <- theObject@subsets
            
            if(subset_selection_name_f != "None" & subset_selection_name_f != "None,") {
              
              
              for(subset_item in subsets_list[[1]]) {
                
                if(subset_item@item_name == subset_selection_name_f) {
                  samples_in_subset_f <- subset_item@colnames
                  
                  break
                }
              }
            }
            return(samples_in_subset_f)
          }
)

setGeneric(name="get_merged_frame",
           def=function(theObject)
           {
             standardGeneric("get_merged_frame")
           }
)

setMethod(f="get_merged_frame",
          signature="input_data_table",
          definition=function(theObject)
            
            # _Merged frame is a data frame that contains the data values, sample names and class labels all merged into one data frame. Used for displaying the data on the screen
          {
            t1_f <- theObject
            table_class_labels <- t1_f@class_labels
            table_data <- t1_f@df
            t_table_class_labels <- transpose(table_class_labels)
            row_titles <- seq(1, nrow(table_data), 1)
            
            t_table_class_labels <- cbind(row_titles = 0, t_table_class_labels)
            t_table_class_labels[1, 1] <- "column_label"
            t_table_class_labels[2, 1] <- "class_label"
            
            colnames(t_table_class_labels) <- colnames(t1_f@df)
            merged_frame_f <- rbind(t_table_class_labels, t1_f@df)
            return(merged_frame_f)
          }
)

setGeneric(name="filter_merged_frame_based_on_subsets",
           def=function(theObject, merged_frame_f, to_be_deleted_vector)
           {
             standardGeneric("filter_merged_frame_based_on_subsets")
           }
)

setMethod(f="filter_merged_frame_based_on_subsets",
          signature="input_data_table",
          definition=function(theObject, merged_frame_f, to_be_deleted_vector)
            
            # _Method that only keeps a data subset in the merged data frame (of class labels and numeric data) and deletes the rest
          {
            filtered_merged_frame_f <- merged_frame_f[ , !(names(merged_frame_f) %in% to_be_deleted_vector)]
            return(filtered_merged_frame_f)
          }
)

setGeneric(name="add_header_titles_to_merged_frame",
           def=function(theObject, merged_frame_f)
           {
             standardGeneric("add_header_titles_to_merged_frame")
           }
)

setMethod(f="add_header_titles_to_merged_frame",
          signature="input_data_table",
          definition=function(theObject, merged_frame_f)
            # _Method for adding labels to the merged data frame that indicate which row is for sample names and which row is for class labels
          {
            merged_frame_f[1, 1] <- "Column Label"
            merged_frame_f[2, 1] <- "Class Label"
            return(merged_frame_f)
          }
)

setGeneric(name="update_input_data_summary",
           def=function(theObject)
           {
             standardGeneric("update_input_data_summary")
           }
)

setMethod(f="update_input_data_summary",
          signature="input_data_table",
          definition=function(theObject)
            
            # _Method for updating row and column statistics of a data table (used after loading the data from a file)
          {
            output_list_f <- list()
            t1_f <- theObject
            idc_preprocessing <- input_data_checker()
            idc_preprocessing@mydata_csv <- t1_f@df
            idc_preprocessing@class_labels_csv <- t1_f@class_labels
            
            data_summary <- summarise_data_frame_statistics(idc_preprocessing)
            
            data_summary2 <- data_summary[ - which(names(data_summary) == "not_na")]
            data_summary2 <- data_summary2[ - which(names(data_summary2) == "is_na")]
            
            header_class_label_statistics <- c("ncol_data" = "Number of samples (columns)", "nrow_data" = "Number of variables (rows)", "number_of_unique_class_labels" = "Number of unique class labels", "class_labels_with_column_counts" = "Unique class labels (number of columns in the class)")
            colnames_class_label_statistics <- c("Data table statistics")
            data_summary_frame_class_label_statistics_f <- table_from_list_for_displaying(data_summary2, header_class_label_statistics, colnames_class_label_statistics)
            output_list_f$data_summary_frame_class_label_statistics <- data_summary_frame_class_label_statistics_f
            output_list_f$data_summary <- data_summary
            return(output_list_f)
          }
)

setGeneric(name="df_get_sample_names",
           def=function(theObject)
           {
             standardGeneric("df_get_sample_names")
           }
)

setMethod(f="df_get_sample_names",
          # _Function to get column names of the data frame that is located at @df in the object, excluding the first column
          signature="input_data_table",
          definition=function(theObject)
          {
            names_f <- colnames(theObject@df)
            names_f <- names_f[2:length(names_f)]
            return(names_f)
          }
)

setGeneric(name="df_get_variable_names",
           def=function(theObject)
           {
             standardGeneric("df_get_variable_names")
           }
)

setMethod(f="df_get_variable_names",
          # _Function to get variable names of the data frame that is located at @df in the object
          signature="input_data_table",
          definition=function(theObject)
          {
            var_names_f <- as.character(theObject@df[, 1], stringsAsFactors = FALSE)
            return(var_names_f)
          }
)

data_frame_to_t1_format <- function(data_f) {
  
  # _Converts a normal data frame to the format where the row names are stored in the first column
  df_rownames_vector <- as.character(rownames(data_f))
  df_rownames <- data.frame(df_rownames_vector, stringsAsFactors = FALSE)
  colnames(df_rownames) <- c("X")
  data_f <- cbind(df_rownames, data_f)
  rownames_vector <- as.character(c(1:nrow(data_f)))
  rownames(data_f) <- rownames_vector
  return(data_f)
}

get_samples_by_class_label <- function(data_f, selected_class_f, class_labels_vector_f) {
  # _Gets the names of samples with a specific class label, based on class_labels_vector_f
  ind <- which(class_labels_vector_f == selected_class_f)
  data_f_1class_names <- colnames(data_f[, ind])
  return(data_f_1class_names)
}

call_function_on_sample_class <- function(data_f, selected_class_f, class_labels_vector_f, function_f) {
  # _Applies a function (function_f) selectively to one sample class in a data frame (based on class_labels_vector_f)
  selected_samples <- get_samples_by_class_label(data_f, selected_class_f, class_labels_vector_f)
  data_f[,selected_samples] <- do.call(what = function_f, list(data_f[,selected_samples]))
  return(data_f)
}


setGeneric(name="get_class_labels_as_vector",
           def=function(theObject)
           {
             standardGeneric("get_class_labels_as_vector")
           }
)

setMethod(f="get_class_labels_as_vector",
          # _Extracts class labels data from @class_labels data frame and outputs a vector
          signature="input_data_table",
          definition=function(theObject)
          {
            class_labels_vector_f <- as.character(theObject@class_labels[,2])
            names(class_labels_vector_f) <- theObject@class_labels[,1]
            
            return(class_labels_vector_f)
          }
)

setGeneric(name="get_unique_class_labels_as_vector",
           def=function(theObject)
           {
             standardGeneric("get_unique_class_labels_as_vector")
           }
)

setMethod(f="get_unique_class_labels_as_vector",
          # _Extracts unique class labels data from @class_labels data frame and outputs a vector
          signature="input_data_table",
          definition=function(theObject)
          {
            unique_class_labels_vector_f <- unique(get_class_labels_as_vector(theObject))
            return(unique_class_labels_vector_f)
          }
)

setGeneric(name="call_function_on_selected_classes",
           def=function(theObject, selected_classes_f, function_f)
           {
             standardGeneric("call_function_on_selected_classes")
           }
)

setMethod(f="call_function_on_selected_classes",
          # _Applies a function (function_f) selectively to specific classes in the theObject@df data frame (based on class_labels_vector_f)
          signature="input_data_table",
          definition=function(theObject, selected_classes_f, function_f)
          {
            data_f <- df_remove_first_col(theObject@df)
            
            class_labels_vector_f <- get_class_labels_as_vector(theObject)
            
            new_data_f <- data_f
            for(selected_class_f in selected_classes_f) {
              new_data_f <- call_function_on_sample_class(new_data_f, selected_class_f, class_labels_vector_f, function_f)
            }
            new_df <- data_frame_to_t1_format(new_data_f)
            
            return(new_df)
          }
)

setGeneric(name="call_function_on_df",
           def=function(theObject, function_f, na_omit = FALSE)
           {
             standardGeneric("call_function_on_df")
           }
)

setMethod(f="call_function_on_df",
          # _Applies a function (function_f) to theObject@df data frame
          signature="input_data_table",
          definition=function(theObject, function_f, na_omit = FALSE)
          {
            if(is.null(na_omit)) {
              na_omit <- FALSE
            }
            data_f <- df_remove_first_col(theObject@df)
            if(na_omit) {
              data_f <- na.omit(data_f)
            }
            data_f <- do.call(what = function_f, list(data_f))
            new_df <- data_frame_to_t1_format(data_f)
            return(new_df)
          }
)

setGeneric(name="get_t_df_as_matrix",
           def=function(theObject, na_omit = FALSE)
           {
             standardGeneric("get_t_df_as_matrix")
           }
)

setMethod(f="get_t_df_as_matrix",
          signature="input_data_table",
          definition=function(theObject, na_omit = FALSE)
          {
            # _Gets the t@df data frame and returns it as a matrix
            if(is.null(na_omit)) {
              na_omit <- FALSE
            }
            data_f <- df_remove_first_col(theObject@df)
            if(na_omit) {
              data_f <- na.omit(data_f)
            }
            x_matrix <- data.matrix(data_f)
            return(x_matrix)
          }
)

matrix_to_t1_format <- function(matrix_f) {
  # _Gets a matrix and converts it into a data frame, in the right format to be stored in @df slot of this object
  new_frame <- as.data.frame(matrix_f)
  result_frame <- data_frame_to_t1_format(new_frame)
  return(result_frame)
}

