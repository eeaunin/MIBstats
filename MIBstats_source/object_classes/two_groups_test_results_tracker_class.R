# _Class for collecting test results of each type of statistical test. Each object of this class stores the results of a different type of statistical test

two_groups_test_results_tracker <- setClass("two_groups_test_results_tracker",
                                            slots = c(test_type = "character",
                                                      results = "list", results_pages = "list",
                                                      curr_sel_index = "list", fc_out_frame = "data.frame", p_values_answer = "list", unadjusted_p_values_answer = "list", plot_val = "list"),
                                            
                                          prototype=list(
                                            test_type = "",
                                            results = list(), results_pages = list(),
                                            curr_sel_index = list(), p_values_answer = list(), unadjusted_p_values_answer = list(), plot_val = list(), fc_out_frame = data.frame()
                                          ))

setGeneric(name="get_test_result_details",
           def=function(theObject, rtr_sel_f)
           {
             standardGeneric("get_test_result_details")
           }
)

setMethod(f="get_test_result_details",
          signature="two_groups_test_results_tracker",
          definition=function(theObject, rtr_sel_f)
          {
            # _Gets the details of results of performed statistical tests in list format.
            # _The results from fold changes tests and p-value yielding tests are sorted into separate sublists (this is useful for making a volcano plot)
            out_list <- list()
            
            fc_tests_list <- list()
            pval_tests_list <- list()
            
            results_list <- theObject@results$l
            if(length(results_list) > 0) {
              results_list_counter <- 1
              
              for(test_result in results_list) {
                if(!is.list(test_result)) {
                  new_list_item <- list()
                  new_list_item$test_type <- test_result@test_type
                  new_list_item$test_queue_item_details <- test_result@test_queue_item_details
                  new_list_item$rtr_location <- c(rtr_sel_f, results_list_counter)
                  
                  
                  if(test_result@test_type == "fold_changes") {
                    fc_tests_list <- append(fc_tests_list, list(new_list_item))
                  } else {
                    pval_tests_list <- append(pval_tests_list, list(new_list_item))
                  }
                  
                }
                results_list_counter <- results_list_counter + 1
              }
            }
            out_list$fc_tests_list <- fc_tests_list
            out_list$pval_tests_list <- pval_tests_list
            return(out_list)
          }
)

rtr_check_if_two_lists_have_identical_items <- function(list1, list2, ignored_indices) {
  # _Compares two lists element by element to see if they are identical. The two lists have to consist of booleans or character vectors only. ignored_indices are list indices that will not be included in the comparison
  lists_identical <- TRUE
  if(length(list1) != length(list2)) {
    lists_identical <- FALSE
  } else {
    for(i in 1:length(list1)) {
      if(!is.element(i, ignored_indices)) {
        x <- list1[[i]]
        y <- list2[[i]]
        if(is.logical(x)) {
          if(x != y) {
            lists_identical <- FALSE
          }
        } else if (is.character(x)) {
          if(length(x[!x %in% y]) != 0 | length(y[!y %in% x])) {
            lists_identical <- FALSE
          }
        }
      }
    }
  }
  return(lists_identical)
}

rtr_test_parameters_to_string <- function(test_settings_item) {
  # _Takes a list with statistical test parameters as input and produces a text string with these parameters that is formatted for displaying it in the UI
  
  contains_subsets <- function(subset_f) {
    # _Checks if the test made use of data subsets
    contains_subsets_flag <- FALSE
    for(subset_item in subset_f) {
      if(subset_item != "None") {
        contains_subsets_flag <- TRUE
      }
    }
    return(contains_subsets_flag)
  }
  
  group_names_with_subsets_to_string <- function(group_f, subset_f) {
    # _Parses the the group names and subset names into one string
    counter <- 1
    out_string <- ""
    for(group_item in group_f) {
      out_string <- paste(out_string, group_item, sep = "")
      subset_item <- subset_f[counter]
      if(subset_item != "None") {
        out_string <- paste(out_string, " (", subset_item, ")", sep="")
      }
      if(counter<length(group_f)) {
        out_string <- paste(out_string, ", ")
      }
      counter <- counter + 1
    }
    return(out_string)
  }
  
  
  test_full_name <- ""
  if(!is.null(test_settings_item$test_type)) {
    test_full_name <- get_full_name_of_a_two_groups_test(test_settings_item$test_type)
  }

  
  test_description <- paste(test_full_name, ": ", sep="")
  d <- test_settings_item$test_queue_item_details
  
  if(!contains_subsets(d$subset_g1)) {
    test_description <- paste(test_description, vector_to_comma_separated_character(d$groups1), sep="")
  } else {
    test_description <- paste(test_description, group_names_with_subsets_to_string(d$groups1, d$subset_g1), sep="")
  }
  
  test_description <- paste(test_description, " vs ", sep="")
  
  if(!contains_subsets(d$subset_g2)) {
    test_description <- paste(test_description, vector_to_comma_separated_character(d$groups2), sep="")
  } else {
    test_description <- paste(test_description, group_names_with_subsets_to_string(d$groups2, d$subset_g2), sep="")
  }
  
  test_description <- paste(test_description, ". ", sep="")
  
  
  if(d$paired) {
    
    if(is.list(d$pairings) || (is.character(d$pairings) & d$pairings != "<Empty>")) {
      
      test_description <- paste(test_description, "Paired samples (custom pairing). ", sep="")
    } else {
      test_description <- paste(test_description, "Paired samples. ", sep="")
    }
  } else {
    test_description <- paste(test_description, "Unpaired samples. ", sep="")
  }
  
  if(test_settings_item$test_type != "fold_changes") {
    test_description <- paste(test_description, "p-value adjustment: ", get_full_name_of_p_val_adjustment_method(d$p_adjustment), ". ", sep="")
    if(test_settings_item$test_type == "t-test") {
      if(d$welch) {
        test_description <- paste(test_description, "Welch correction. ", sep="")
      }
    }
  }
  
  return(test_description)
}

get_test_queue_details_of_test_results <- function(rtr_f2_a, excluded_indices) {
  # _Extracts parameters of previously performed statistical tests from the variables that store test results
  # _excluded_indices: rtr indices of test types that will not be included in the extraction
  excluded_indices_used <- TRUE
  if(missing(excluded_indices) || is.null(excluded_indices)) {
    excluded_indices_used <- FALSE
  }
  test_settings_l <- list(pval_tests = list(), fc_tests = list())
  test_type_counter <- 1
  for(test_type_f in rtr_f2_a) {
    if(!excluded_indices_used || !is.element(test_type_counter, excluded_indices)) {
      result_lists <- get_test_result_details(test_type_f, test_type_counter)
      if(length(result_lists$fc_tests_list) != 0) {
        test_settings_l$fc_tests <- append(test_settings_l$fc_tests, result_lists$fc_tests_list)
      }
      if(length(result_lists$pval_tests_list) != 0) {
        test_settings_l$pval_tests <- append(test_settings_l$pval_tests, result_lists$pval_tests_list)
      }
    }
    test_type_counter <- test_type_counter + 1
  }
  return(test_settings_l)
}

get_test_result_from_rtr <- function(rtr_indices, rtr_copy_f) {
  # _Function that takes a vector with two numeric indices (rtr_indices) and the test results tracker (rtr) as input.
  # _The first index represents statistical test type in the results tracker (rtr)
  # _The second index represents the sequence number of the test in the list of the performed tests
  # _The function retrieves the test results that correspond to the input indices
  test_type_index <- rtr_indices[1]
  results_list_index <- rtr_indices[2]
  test_type_f <- rtr_copy_f[[test_type_index]]
  results_list <- test_type_f@results$l
  test_result <- results_list[[results_list_index]]
  return(test_result)
}




