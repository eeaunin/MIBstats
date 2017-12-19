add_extension_to_file_name_if_needed <- function(file_name_f, extension_f) {
# _Adds an extension to a file name if it does not already have the correct extension
  split_file_name <- unlist(strsplit(file_name_f, ".", fixed = TRUE))
  current_extension_f <- split_file_name[length(split_file_name)]
  if(current_extension_f != extension_f) {
    file_name_f <- paste(file_name_f, ".", extension_f, sep="")
  }
  return(file_name_f)
}

show_generic_error_message <- function(message_f) {
  # _Function for displaying error messages
  showModal(modalDialog(
    title = "Error",
    message_f)
  )
}


message_out <- function(message_f, title_f) {
# _Displays a message box
  showModal(modalDialog(
    title = title_f,
    message_f)
  )
}

sort_vector_by_another_vector <- function(x_f, y_f) {
# _Sorts the order of elements in one vector (x) based on the order of elements in another vector (y)
  sorted_x_f <- x_f[order(match(x_f,y_f))]
  return(sorted_x_f)
}

show_generic_error_message_w_e <- function(message_f, warning_f, error_f) {
  # _Function to display both error messages and warnings
  if(length(warning_f) > 0) {
    message_f <- paste(message_f, " ", unlist(warning_f), sep="")
  }
  if(length(error_f) > 0) {
    message_f <- paste(message_f, " ", unlist(error_f), sep="")
  }
  show_generic_error_message(message_f)
}

p_value_to_stars <- function(p_value_f) {
  # _Function for converting a p-value to stars indicating the level of significance
  p_value_stars_f <- "NA"
  if(!is.na(p_value_f)) {
    p_value_f <- as.numeric(p_value_f)
    p_value_stars_f <- "NS"
    if(p_value_f <= 0.05) {
      p_value_stars_f <- "*"
    }
    if(p_value_f <= 0.01) {
      p_value_stars_f <- "**"
    }
    if(p_value_f <= 0.001) {
      p_value_stars_f <- "***"
    }
  }
  
  return(p_value_stars_f)
}

p_value_to_stars_for_a_vector <- function(pval_list_f) {
  # _Function for converting a vector of p-values to stars indicating the level of significance
  two_groups_test_p_value_stars_f <- c()
  for(item_f in pval_list_f) {
    two_groups_test_p_value_stars_f <- c(two_groups_test_p_value_stars_f, p_value_to_stars(item_f))
  }
  return(two_groups_test_p_value_stars_f)
}

swap_names_and_values <- function(input_vector_f) {
  # _Swaps values and names of a vector. Meant to be used with character vectors
  names_f <- names(input_vector_f)
  names(names_f) <- input_vector_f
  return(names_f)
}

vector_to_comma_separated_character <- function(input_vector_f) {
# _Converts a vector to a character where the elements are separated by commas
  message_f <- ""
  if(is.vector(input_vector_f)) {
    counter_f = 1
    for(item_f in input_vector_f) {
      if(counter_f < length(input_vector_f)) {
        message_f <- paste(message_f, toString(item_f), ", ", sep = "")
      } else {
        message_f <- paste(message_f, toString(item_f), sep = "")
      }
      counter_f <- counter_f + 1
    }
  }
  return(message_f)
}

table_from_list_for_displaying <- function(input_list_f, rownames_f, colnames_f) {
  # _Takes a list as input and converts it to a data frame for displaying on the screen as a table
  data_summary2_frame <- data.frame(t(sapply(input_list_f, c)))
  
  data_summary2_frame <- t(data_summary2_frame)
  colnames(data_summary2_frame) <- colnames_f
 
  data_summary_row_labels <- c()
  for (name in rownames(data_summary2_frame)) {
    title_string <- ""
    name_string <- toString(name)
    if(is.element(name_string, names(rownames_f))) {
      title_string <- rownames_f[name_string]
      data_summary_row_labels <- c(data_summary_row_labels, title_string)
    }
  }
  rownames(data_summary2_frame) <- data_summary_row_labels
  return(data_summary2_frame)
}

count_occurrencies_in_data_frame <- function(df_f, input_character_f, remove_first_col) {
# _Counts how many times a character occurs in a data frame.
# _remove_first_col is a boolean that toggles if the first column is removed before counting  
  if(remove_first_col) {
    df_f_data <- df_f[,2:ncol(df_f)]
  }
  occurrencies_table <- table(unlist(df_f_data))
  count_f <- 0
  if(is.element(input_character_f, names(occurrencies_table))) {
    count_f <- occurrencies_table[names(occurrencies_table) == input_character_f]
    count_f <- count_f[1]
  }
  return(count_f)
}

txt_out <- function(input_s) {
  # _Function to print the name and the value of a variable to the console
  call_f <- match.call()
  call_f_list <- as.list(call_f)
  print(call_f_list[[-1]])
  print(input_s)
}

get_sample_names_from_t_df <- function(t_df) {
  # _Function to get column names of a data frame, excluding the first column
  names_f <- colnames(t_df)
  names_f <- names_f[2:length(names_f)]
  return(names_f)
}

class_labels_vector_to_class_labels_frame <- function(class_labels_vector_f) {
  # _Takes a vector with class labels as the input and converts it to a data frame that is in the format that is used by the input_data_table class 
  
  len1 <- length(unname(class_labels_vector_f))
  class_labels_firstrow <- c(1:len1)
  
  class_labels_frame_f <- cbind(class_labels_firstrow, unname(class_labels_vector_f))
  class_labels_frame_f <- as.data.frame(class_labels_frame_f, stringsAsFactors = FALSE)
  colnames(class_labels_frame_f) <- c("X", "X")
  return(class_labels_frame_f)
}

df_remove_first_col <- function(input_df) {
  # _Function that takes a data frame where row names are stored in the first column and converts this to a data frame where the row names are data frame rownames. The first column is deleted
  data_f <- input_df
  rownames(data_f) <- data_f[,1]
  input_frame_rownames <- data_f[,1]
  data_f <- data_f[,2:ncol(data_f)]
  return(data_f)
}



