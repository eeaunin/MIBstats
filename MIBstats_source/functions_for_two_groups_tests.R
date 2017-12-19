multiple_testing_correction_radiobutton_names <- function() {
  # _Function for conversion between multiple testing correction radiobutton labels and radiobutton values
  names_f <- c("BH" = "BH", "Holm" = "holm", "Hochberg" = "hochberg", "Hommel" = "hommel", "Bonferroni" = "bonferroni", "BY" = "BY", "none" = "none")
  return(names_f)
}         

get_full_name_of_p_val_adjustment_method <- function(short_name_f) {
  # _Gets the capitalised name of a p-adjustment method from the lowercase input
  full_name_f <- ""
  radiobutton_names_f <- multiple_testing_correction_radiobutton_names()
  multiple_testing_correction_radiobutton_names2 <- swap_names_and_values(radiobutton_names_f)
  full_name_f <- multiple_testing_correction_radiobutton_names2[short_name_f]
  return(full_name_f)
}

get_full_name_of_a_two_groups_test <- function(short_name_f) {
  # _Gets the full name of a statistical test (for displaying on the screen) from abbreviated name (that is used in the server part of the app)
  full_name_f <- ""
  names_dict <- c("t-test" = "t-test", "f-test" = "F-test", "shapiro_test" = "Shapiro-Wilk test", "shapiro" = "Shapiro-Wilk test", "fold_changes" = "Fold changes", "rank_product" = "Rank product", "rank_sum" = "Rank sum", "iga" = "iGA", "db_iga" = "db-iGA", "mann-whitney_u_test" = "Mann-Whitney U test")
  if(is.element(short_name_f, names(names_dict))) {
    full_name_f <- names_dict[short_name_f]
  }
  return(full_name_f)
}

