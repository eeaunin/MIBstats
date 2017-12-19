invalid_values_count_in_list <- function(input_list_f) {
# _Counts the number of invalid (non-numeric and NaN) items in a list
  non_num_counter_f <- 0
  for (value_f in input_list_f) {
    if((is.numeric(value_f) == FALSE) | (is.na(value_f) == TRUE)) {
      non_num_counter_f <- non_num_counter_f + 1
    }
  }
  return(non_num_counter_f)
}

enough_numeric_values_in_vector <- function(input_vector_f, max_inv_ratio_f) {
# _Checks if the ratio of invalid items to numeric items in a vector is below a threshold.
# _Returns TRUE if the vector is OK (not too many invalid values).
# _Returns FALSE if the vector is not OK (too many invalid values).
  invalid_val_count <- invalid_values_count_in_list(input_vector_f)
  total_val_count <- length(input_vector_f)
  invalid_values_check_ok <- TRUE
  if(total_val_count == 0) {
    invalid_values_check_ok <- FALSE
  } else {
    if((invalid_val_count / total_val_count) > max_inv_ratio_f) {
      invalid_values_check_ok <- FALSE
    }
  }
  return(invalid_values_check_ok)
}