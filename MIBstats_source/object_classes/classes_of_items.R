# _File for small classes that have no methods

input_data_subset <- setClass("input_data_subset", slots = c(parent_class = "character", colnames = "character"))

preprocessing_settings_NA_zero_inf_values <- setClass("preprocessing_settings_NA_zero_inf_values",
                                                      slots = c(NA_values_choice = "numeric",
                                                                NA_values_threshold = "numeric",
                                                                zero_values_choice = "numeric",
                                                                minus_inf_values_choice = "numeric",
                                                                plus_inf_values_choice = "numeric"))

