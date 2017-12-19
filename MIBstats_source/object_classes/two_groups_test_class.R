# _class for performing statistical tests that compare two groups in the data (e.g., the t-test, F-test, Shapiro-Wilk test, Mann-Whitney U test or fold changes calculation)

two_groups_test <- setClass("two_groups_test", 
                            slots = c(test_type = "character", label = "character", input_data = "input_data_table", group_a = "character", group_b = "character", paired_option = "logical", extra_params = "list", max_invalid_val_ratio = "numeric", p_adjustment = "character"),

prototype=list(
  test_type = "", label = "", input_data = NULL, group_a = "", group_b = "", paired_option = FALSE, extra_params = list(), max_invalid_val_ratio = 0, p_adjustment = ""
))

source("object_classes/two_groups_test_fold_changes.R")

setGeneric(name="adjust_p_values_list_for_multiple_comparisons",
           def=function(theObject, pval_list_f)
           {
             standardGeneric("adjust_p_values_list_for_multiple_comparisons")
           }
)

setMethod(f="adjust_p_values_list_for_multiple_comparisons",
          signature="two_groups_test",
          definition=function(theObject, pval_list_f)
            
            # _Method for performing p-value correction for multiple hypothesis testing
          {
            pval_list_f <- as.numeric(pval_list_f)
            adjusted_p_f <- p.adjust(pval_list_f, theObject@p_adjustment)
            return(adjusted_p_f)
          }
)

setGeneric(name="adjust_p_values_in_full_results_list",
           def=function(theObject, results_list_f)
           {
             standardGeneric("adjust_p_values_in_full_results_list")
           }
)

setMethod(f="adjust_p_values_in_full_results_list",
          signature="two_groups_test",
          definition=function(theObject, results_list_f)
            
            # _Method for performing p-value correction for multiple hypothesis testing. Here the adjustment is done on the full tests statistics list (that is downloadable from the test results page)
          {
            results_list_adjusted_f <- list()
            
            if(!is.null(results_list_f) && is.list(results_list_f) && length(results_list_f) > 0) {
              p_values_stored <- c()
              for(item_f in results_list_f) {
                
                if(!is.null(item_f) && is.list(item_f) && length(item_f) > 0) {
                  p_values_stored <- c(p_values_stored, item_f$p.value)
                } else {
                  p_values_stored <- c(p_values_stored, NaN)
                }
                
              }
              
              p_values_adjusted <- adjust_p_values_list_for_multiple_comparisons(theObject, p_values_stored)
              
              results_list_adjusted_f <- results_list_f
              counter_f <- 1
              for(item_f in results_list_f) {
                if(!is.null(item_f) && is.list(item_f) && length(item_f) > 0) {
                  results_list_adjusted_f[[counter_f]]$p.value <- p_values_adjusted[[counter_f]]
                }
                
                counter_f <- counter_f + 1
              }
              
            }
            
            return(results_list_adjusted_f)
          }
)

setGeneric(name="get_initialised_results_object",
           def=function(theObject)
           {
             standardGeneric("get_initialised_results_object")
           }
)

setMethod(f="get_initialised_results_object",
          signature="two_groups_test",
          definition=function(theObject)
          {
            # _Initialises a two_groups_test object
            
            r1_f <- two_groups_test_result()
            r1_f@header_unique <- unique(get_class_labels_as_vector(theObject@input_data)) # _Unique class labels
            r1_f@test_type <- theObject@test_type
            r1_f@item_name <- paste(t1_f@item_name, "two_groups_test", sep = "|", collapse = NULL)
            r1_f@item_label <- paste(t1_f@item_name, "two_groups_test", sep = "|", collapse = NULL)
            r1_f@group_a <- theObject@group_a
            r1_f@group_b <- theObject@group_b
            r1_f@label <- theObject@label
            r1_f@group1_labels <- theObject@group_a
            r1_f@group2_labels <- theObject@group_b
            r1_f@paired_option <- theObject@paired_option
            return(r1_f)
          }
)

setGeneric(name="two_groups_test_on_data_frame",
           def=function(theObject)
           {
             standardGeneric("two_groups_test_on_data_frame")
           }
)

setMethod(f="two_groups_test_on_data_frame",
          signature="two_groups_test",
          definition=function(theObject)
            
          # _This method performs a statistical test that compares two groups. Used for the t-test, F-test and Mann-Whitney U test
          # _welch_correction_option: boolean that toggles the var.equal option of t.test
          {
            test_type_f <- theObject@test_type
            label_f <- theObject@label
            t1_f <- theObject@input_data
            group_a_f <- theObject@group_a
            group_b_f <- theObject@group_b
            paired_option <- theObject@paired_option
            extra_params_f <- theObject@extra_params
            max_invalid_val_ratio <- theObject@max_invalid_val_ratio
            p_adjustment_f <- theObject@p_adjustment
            
            failed_variables_counter <- 0
            
            if(test_type_f == "t-test") {
              welch_correction_option <- extra_params_f$welch_correction_option
            }
            
            extract_alternative_hypothesis <- function(extra_params_f2) {
              alt_hypot_f <- ""
              alt_hypot_instruction <- extra_params_f2$alternative_hypothesis
              
              if(alt_hypot_instruction == "alternative_hypothesis_two_sided") {
                alt_hypot_f <- "two.sided"
              } else if (alt_hypot_instruction == "alternative_hypothesis_less") {
                alt_hypot_f <- "less"
              } else if (alt_hypot_instruction == "alternative_hypothesis_greater") {
                alt_hypot_f <- "greater"
              }
              
            }
            
            perform_t_test <- function(data_row_a_f2, data_row_b_f2, paired_option_f2, var_equal_option_f2, alternative_hypothesis_f2) {

              result_f2 <- t.test(data_row_a_f2, data_row_b_f2, paired = paired_option_f2, var.equal = var_equal_option_f2, alternative = alternative_hypothesis_f2)
              return(result_f2)
            }
            perform_f_test <- function(data_row_a_f2, data_row_b_f2) {
              result_f2 <- var.test(data_row_a_f2, data_row_b_f2)
              return(result_f2)
            }
            
            mydata_csv <- t1_f@df
            class_labels_csv <- t1_f@class_labels
            
            r1_f <- two_groups_test_result(associated_table = t1_f@item_name, out_frame = data.frame(), full_results_list = list(), data_row_a = list(), data_row_b = list(), header_unique = "", error_encountered = FALSE, errors = list())
            r1_f@test_type <- test_type_f
            r1_f@item_name <- paste(t1_f@item_name, "two_groups_test", sep = "|", collapse = NULL)
            r1_f@item_label <- paste(t1_f@item_name, "two_groups_test", sep = "|", collapse = NULL)
            r1_f@group_a <- group_a_f
            r1_f@group_b <- group_b_f
            r1_f@p_adjustment <- p_adjustment_f
            
            two_groups_test_failed <- FALSE
            error_message <- c()
            error_messages_list <- list(main = NULL, data = NULL, class_labels = NULL)
            error_encountered <- FALSE
            mydata <- mydata_csv[, 2:ncol(mydata_csv)]
            
            row_titles <- mydata_csv[, 1:1]
            
            class_labels_csv_vector <- as.vector(class_labels_csv[,2])
            data_header_unique <- unique(class_labels_csv_vector)
            
            p_values_list <- c()
            two_groups_test_p_value_stars <- c()
            two_groups_test_full_results_list <- list()
            
            data_row_a_list <- list()
            data_row_b_list <- list()
            out_frame <- data.frame()
            test_performed_f <- FALSE
            
            groups_in_header <- group_a_and_group_b_in_header(theObject, data_header_unique)
            
            if(test_type_f == "t-test") {
              var_equal_option <- TRUE
              if(welch_correction_option) {
                var_equal_option <- FALSE
              }
            }
            
            if(groups_in_header$a && groups_in_header$b) {
              indA <- get_sorted_indexes(theObject, group_a_f, class_labels_csv_vector)
              indB <- get_sorted_indexes(theObject, group_b_f, class_labels_csv_vector)
              
              for (i in 1:nrow(mydata)) {
                two_groups_test_failed <- FALSE              
                data_row_a <- mydata[i,indA]
                data_row_b <- mydata[i,indB]
                
                data_row_a_list <- append(data_row_a_list, list(data_row_a))
                data_row_b_list <- append(data_row_b_list, list(data_row_b))
                
                # _Separates group A and group B data
                
                pval <- NaN
                two_groups_test_full_result <- NaN
                
                data_row_a <- unlist(data_row_a)
                data_row_b <- unlist(data_row_b)
                
                if((enough_numeric_values_in_vector(data_row_a, max_invalid_val_ratio) == TRUE) & (enough_numeric_values_in_vector(data_row_b, max_invalid_val_ratio) == TRUE) ) {
                  # _Proceed if there is not too many invalid values
                  
                  # _warnings
                  w.list <- NULL # init warning
                  w.handler <- function(w){ # warning handler
                    warn <- simpleWarning(w$message, w$call) # build warning
                    w.list <<- c(w.list, paste(warn, collapse = " ")) # save warning
                    invokeRestart("muffleWarning")
                  }
                  # _errors
                  e.list <- NULL # init errors
                  e.handler <- function(e){ # error handler
                    err <- simpleError(e$message, e$call)
                    e.list <<- c(e.list, paste(err, collapse = " ")) # save error
                  } 
                  
                  if(test_type_f == "t-test") {
                    mtry <- try(withCallingHandlers(perform_t_test(data_row_a, data_row_b, paired_option, var_equal_option, extract_alternative_hypothesis(extra_params_f)), warning = w.handler, error = e.handler), silent = TRUE)
                  } else if(test_type_f == "mann-whitney_u_test") {
                    mtry <- try(withCallingHandlers(wilcox.test(x = data_row_a, y = data_row_b, paired = paired_option, alternative = extract_alternative_hypothesis(extra_params_f)), warning = w.handler, error = e.handler), silent = TRUE)
                  } else if(test_type_f == "f-test") {
                    mtry <- try(withCallingHandlers(perform_f_test(data_row_a, data_row_b), warning = w.handler, error = e.handler), silent = TRUE)
                  }
                  
                  warning = w.list
                  error = e.list
                  
                  if ((length(warning) == 0) && (length(error) == 0)) {
                    if(test_type_f == "t-test") {
                      two_groups_test_full_result <- perform_t_test(data_row_a, data_row_b, paired_option, var_equal_option, extract_alternative_hypothesis(extra_params_f))
                    } else if(test_type_f == "mann-whitney_u_test") {
                      two_groups_test_full_result <- wilcox.test(x = data_row_a, y = data_row_b, paired = paired_option, alternative = extract_alternative_hypothesis(extra_params_f))
                    } else if(test_type_f == "f-test") {
                      two_groups_test_full_result <- perform_f_test(data_row_a, data_row_b)
                    }
                    
                    # _Performs the statistical test and stores the full report on the result
                    
                    pval <- two_groups_test_full_result$p.value
                    # _Stores the p-value of the t-test separately
                    
                    p_values_list <- c(p_values_list, pval)
                    two_groups_test_full_results_list <- append(two_groups_test_full_results_list, list(two_groups_test_full_result))
                    
                    # _Stores test statistics and input data for using it in the output of the function
                  } else {
                    
                    #error_messages_list$main <- "Variables were encountered with which the test could not be performed. "
                    two_groups_test_failed <- TRUE
                  }
                } else {
                  error_messages_list$data <- "Too many non-numeric values in the data. "
                  two_groups_test_failed <- TRUE
                }
                
                if(two_groups_test_failed == TRUE) {
                  
                  failed_variables_counter <- failed_variables_counter + 1
                  
                  p_values_list <- c(p_values_list, NaN)
                  two_groups_test_full_results_list <- append(two_groups_test_full_results_list, list(NaN))
                  two_groups_test_p_value_stars <- c(two_groups_test_p_value_stars, NaN)
                  error_encountered <- TRUE
                }
              }
              
              adjusted_p_values_list <- adjust_p_values_list_for_multiple_comparisons(theObject, p_values_list)
              two_groups_test_p_value_stars <- p_value_to_stars_for_a_vector(adjusted_p_values_list)
              two_groups_test_unadjusted_p_value_stars <- p_value_to_stars_for_a_vector(p_values_list)
              
              out_frame <- data.frame(variable_index = row_titles, two_groups_test_p_value = adjusted_p_values_list, two_groups_test_asterisks = two_groups_test_p_value_stars)
              
              unadjusted_out_frame <- data.frame(variable_index = row_titles, two_groups_test_p_value = p_values_list, two_groups_test_asterisks = two_groups_test_unadjusted_p_value_stars)
              
              test_performed_f <- TRUE
            } else {
              error_messages_list$class_labels <- "The specified class labels were not found in the table. "
              error_encountered <- TRUE
            }
            p_adjusted_full_results_list <- adjust_p_values_in_full_results_list(theObject, two_groups_test_full_results_list)
            
            if(failed_variables_counter>0) {
              error_messages_list$main <- paste("The test could not be performed with ", as.character(failed_variables_counter), " variables. ", sep="")
            }
            
            error_message <- paste(error_messages_list$main, error_messages_list$data, error_messages_list$class_labels, sep="")
            # _Puts the variables and test statistics in a data frame for outputting
          
            r1_f@out_frame <- out_frame # _Data frame that contains row indexes and p-values (p-values are corrected for multiple testing)
            r1_f@unadjusted_out_frame <- unadjusted_out_frame # _Data frame that contains row indexes and p-values (p-values are uncorrected for multiple testing)
            
            r1_f@full_results_list <- p_adjusted_full_results_list # _Full set of t-test statistics (p-adjusted)
            r1_f@unadjusted_full_results_list <- two_groups_test_full_results_list # _Full set of t-test statistics (not p-adjusted)
            
            r1_f@data_row_a <- data_row_a_list # _Input data of group A
            r1_f@data_row_b <- data_row_b_list # _Input data of group B
            r1_f@header_unique <- data_header_unique # _Unique class labels
            r1_f@error_encountered <- error_encountered
            r1_f@errors <- list(error_message)
            r1_f@test_performed <- test_performed_f
            r1_f@label <- label_f
            r1_f@group1_labels <- group_a_f
            r1_f@group2_labels <- group_b_f
            r1_f@paired_option <- theObject@paired_option
            
            return(r1_f)
          }
)

setGeneric(name="shapiro_test_on_data_frame",
           def=function(theObject)
           {
             standardGeneric("shapiro_test_on_data_frame")
           }
)

setMethod(f="shapiro_test_on_data_frame",
          signature="two_groups_test",
          definition=function(theObject)
          {
            # _This method performs a Shapiro-Wilk test
            
            test_type_f <- theObject@test_type
            label_f <- theObject@label
            t1_f <- theObject@input_data
            group_a_f <- theObject@group_a
            group_b_f <- theObject@group_b
            paired_option <- theObject@paired_option
            extra_params_f <- theObject@extra_params
            max_invalid_val_ratio <- theObject@max_invalid_val_ratio
            p_adjustment_f <- theObject@p_adjustment
            
            mydata_csv <- t1_f@df
            class_labels_csv <- t1_f@class_labels
            
            r1_f <- two_groups_test_result(associated_table = t1_f@item_name, out_frame = data.frame(), full_results_list = list(), data_row_a = list(), data_row_b = list(), header_unique = "", error_encountered = FALSE, errors = list())
            r1_f@test_type <- "shapiro_test"
            r1_f@item_name <- paste(t1_f@item_name, "shapiro_test", sep = "|", collapse = NULL)
            r1_f@item_label <- paste(t1_f@item_name, "shapiro_test", sep = "|", collapse = NULL)
            r1_f@group_a <- group_a_f
            r1_f@group_b <- group_b_f
            
            shapiro_test_failed_a <- FALSE
            shapiro_test_failed_b <- FALSE
            shapiro_test_failed_pairs <- FALSE
            error_messages_list <- list(main = NULL, data = NULL, class_labels = NULL)
            error_message <- c()
            error_encountered <- FALSE
            mydata <- mydata_csv[, 2:ncol(mydata_csv)]
            row_titles <- mydata_csv[, 1:1]
            
            class_labels_csv_vector <- as.vector(class_labels_csv[,2])
            data_header_unique <- unique(class_labels_csv_vector)
            
            p_values_list_s_a <- c()
            p_values_list_s_b <- c()
            p_values_list_s_pairs <- c()
            
            shapiro_test_p_value_stars_a <- c()
            shapiro_test_p_value_stars_b <- c()
            shapiro_test_p_value_stars_pairs <- c()
            shapiro_test_full_results_list_a <- list()
            shapiro_test_full_results_list_b <- list()
            shapiro_test_full_results_list_pairs <- list()
            
            data_row_a_list <- list()
            data_row_b_list <- list()
            out_frame <- data.frame()
            test_performed_f <- FALSE
            
            groups_in_header <- group_a_and_group_b_in_header(theObject, data_header_unique)
            
            if(groups_in_header$a && groups_in_header$b) {
              indA <- get_sorted_indexes(theObject, group_a_f, class_labels_csv_vector)
              indB <- get_sorted_indexes(theObject, group_b_f, class_labels_csv_vector)
              
              for (i in 1:nrow(mydata)) {
                shapiro_test_failed_a <- FALSE
                shapiro_test_failed_b <- FALSE
                data_row_a <- mydata[i,indA]
                data_row_b <- mydata[i,indB]
                
                data_row_a_list <- append(data_row_a_list, list(data_row_a))
                data_row_b_list <- append(data_row_b_list, list(data_row_b))
                
                # _Separates group A and group B data
                
                pval_s_a <- NaN
                pval_s_b <- NaN
                pval_s_pairs <- NaN
                shapiro_test_full_result <- NaN
                
                data_row_a <- unlist(data_row_a)
                data_row_b <- unlist(data_row_b)
                
                
                data_row_pairs <- c()
                if(paired_option) {
                  data_row_pairs <- data_row_a - data_row_b
                }
                
                
                if((enough_numeric_values_in_vector(data_row_a, max_invalid_val_ratio) == TRUE) & (enough_numeric_values_in_vector(data_row_b, max_invalid_val_ratio) == TRUE) ) {
                  
                  # _warnings
                  w.list <- NULL # init warning
                  w.handler <- function(w){ # warning handler
                    warn <- simpleWarning(w$message, w$call) # build warning
                    w.list <<- c(w.list, paste(warn, collapse = " ")) # save warning
                    invokeRestart("muffleWarning")
                  }
                  # _errors
                  e.list <- NULL # init errors
                  e.handler <- function(e){ # error handler
                    err <- simpleError(e$message, e$call)
                    e.list <<- c(e.list, paste(err, collapse = " ")) # save error
                  } 
                  
                  mtry <- try(withCallingHandlers(shapiro.test(data_row_a), warning = w.handler, error = e.handler), silent = TRUE)
                  
                  warning = w.list
                  error = e.list
                  
                  mtry2 <- try(withCallingHandlers(shapiro.test(data_row_b), warning = w.handler, error = e.handler), silent = TRUE)
                  
                  warning2 = w.list
                  error2 = e.list
                  
                  if ((length(warning) == 0) && (length(error) == 0)) {
                    if((length(warning2) == 0) && (length(error2) == 0)) {
                      warning3 <- list()
                      error3 <- list()
                      if(paired_option) {
                        mtry3 <- try(withCallingHandlers(shapiro.test(data_row_pairs), warning = w.handler, error = e.handler), silent = TRUE)
                        
                        warning3 = w.list
                        error3 = e.list
                      }
                      if((length(warning3) == 0) && (length(error3) == 0)) {
                        shapiro_test_full_result_a <- shapiro.test(data_row_a)
                        shapiro_test_full_result_b <- shapiro.test(data_row_b)
                        
                        if(paired_option) {
                          shapiro_test_full_result_pairs <- shapiro.test(data_row_pairs)
                        }
                        
                        # _Performs the Shapiro test and stores the full report on the result
                        
                        pval_s_a <- shapiro_test_full_result_a$p.value
                        pval_s_b <- shapiro_test_full_result_b$p.value
                        # _Stores the p-value of the Shapiro test separately
                        
                        p_values_list_s_a <- c(p_values_list_s_a, pval_s_a)
                        p_values_list_s_b <- c(p_values_list_s_b, pval_s_b)
                        
                        shapiro_test_full_results_list_a <- append(shapiro_test_full_results_list_a, list(shapiro_test_full_result_a))
                        shapiro_test_full_results_list_b <- append(shapiro_test_full_results_list_b, list(shapiro_test_full_result_b))
                        
                        if(paired_option) {
                          pval_s_pairs <- shapiro_test_full_result_pairs$p.value
                          p_values_list_s_pairs <- c(p_values_list_s_pairs, pval_s_pairs)
                          shapiro_test_full_results_list_pairs <- append(shapiro_test_full_results_list_pairs, list(shapiro_test_full_result_pairs))
                        }
                        
                        # _Stores test statistics and input data for using it in the output of the function
                      } else {
                        error_messages_list$main <- "Variables were encountered with which the test could not be performed. "
                        shapiro_test_failed_pairs <- TRUE
                      }
                      
                    } else {
                      error_messages_list$main <- "Variables were encountered with which the test could not be performed. "
                      shapiro_test_failed_b <- TRUE
                    }
                    
                  } else {
                    
                    error_messages_list$main <- "Variables were encountered with which the test could not be performed. "
                    shapiro_test_failed_a <- TRUE
                  }
                } else {
                  if(enough_numeric_values_in_vector(data_row_a, max_invalid_val_ratio) == FALSE) {
                    shapiro_test_failed_a <- TRUE
                  }
                  if(enough_numeric_values_in_vector(data_row_b, max_invalid_val_ratio) == FALSE) {
                    shapiro_test_failed_b <- TRUE
                  }
                  error_messages_list$data <- "Too many non-numeric values in the data. "
                }
                
                if(shapiro_test_failed_a == TRUE) {
                  
                  p_values_list_s_a <- c(p_values_list_s_a, NaN)
                  shapiro_test_full_results_list_a <- append(shapiro_test_full_results_list_a, list(NaN))
                  error_encountered <- TRUE
                }
                if(shapiro_test_failed_b == TRUE) {
                  p_values_list_s_b <- c(p_values_list_s_b, NaN)
                  shapiro_test_full_results_list_b <- append(shapiro_test_full_results_list_b, list(NaN))
                  error_encountered <- TRUE
                }
                if(shapiro_test_failed_pairs == TRUE) {
                  error_encountered <- TRUE
                }
                if(shapiro_test_failed_pairs == TRUE || !paired_option) {
                  p_values_list_s_pairs <- c(p_values_list_s_pairs, NaN)
                  shapiro_test_full_results_list_pairs <- append(shapiro_test_full_results_list_pairs, list(NaN))
                }
              }
              
              
              adjusted_p_values_list_s_a <- adjust_p_values_list_for_multiple_comparisons(theObject, p_values_list_s_a)
              
              
              adjusted_p_values_list_s_b <- adjust_p_values_list_for_multiple_comparisons(theObject, p_values_list_s_b)
              
              
              adjusted_p_values_list_s_pairs <- adjust_p_values_list_for_multiple_comparisons(theObject, p_values_list_s_pairs)
              
              
              
              shapiro_test_p_value_stars_a <- p_value_to_stars_for_a_vector(adjusted_p_values_list_s_a)
              shapiro_test_p_value_stars_b <- p_value_to_stars_for_a_vector(adjusted_p_values_list_s_b)
              shapiro_test_p_value_stars_pairs <- p_value_to_stars_for_a_vector(adjusted_p_values_list_s_pairs)
              
              p_adjusted_full_results_list_a <- adjust_p_values_in_full_results_list(theObject, shapiro_test_full_results_list_a)
              p_adjusted_full_results_list_b <- adjust_p_values_in_full_results_list(theObject, shapiro_test_full_results_list_b)
              
              p_adjusted_full_results_list_pairs <- adjust_p_values_in_full_results_list(theObject, shapiro_test_full_results_list_pairs)  
              
              lengths_vector <- c(length(row_titles), length(adjusted_p_values_list_s_a), length(adjusted_p_values_list_s_b), length(adjusted_p_values_list_s_pairs), length(shapiro_test_p_value_stars_a), length(shapiro_test_p_value_stars_b), length(shapiro_test_p_value_stars_pairs))
              unique_count <- length(unique(lengths_vector))
              
              if(unique_count == 1) {
                out_frame <- data.frame(variable_index = row_titles, shapiro_test_p_value_a = adjusted_p_values_list_s_a, shapiro_test_p_value_b = adjusted_p_values_list_s_b, shapiro_test_p_value_pairs = adjusted_p_values_list_s_pairs, shapiro_test_asterisks_a = shapiro_test_p_value_stars_a, shapiro_test_asterisks_b = shapiro_test_p_value_stars_b, shapiro_test_asterisks_pairs = shapiro_test_p_value_stars_pairs)
                test_performed_f <- TRUE
              } else {
                out_frame <- NULL
                test_performed_f <- FALSE
                error_encountered <- TRUE
                error_messages_list$main <- "Variables were encountered with which the test could not be performed. "
              }
              
              
            } else {
              error_messages_list$class_labels <- "The specified class labels were not found in the table. "
              error_encountered <- TRUE
            }
            
            error_message <- paste(error_messages_list$main, error_messages_list$data, error_messages_list$class_labels, sep="")
            
            # _Puts the variables and test statistics in a data frame for outputting
            
            r1_f@out_frame <- out_frame # _Data frame that contains row indexes and p-values
            
            r1_f@full_results_list$a <- p_adjusted_full_results_list_a # _Full set of t-test statistics
            r1_f@full_results_list$b <- p_adjusted_full_results_list_b
            r1_f@full_results_list$pairs <- p_adjusted_full_results_list_pairs
            r1_f@full_results_list$pairs_boolean <- paired_option
            r1_f@data_row_a <- data_row_a_list # _Input data of group A
            r1_f@data_row_b <- data_row_b_list # _Input data of group B
            r1_f@header_unique <- data_header_unique # _Unique class labels
            r1_f@error_encountered <- error_encountered
            r1_f@errors <- list(error_message)
            r1_f@test_performed <- test_performed_f
            r1_f@label <- label_f
            r1_f@group1_labels <- group_a_f
            r1_f@group2_labels <- group_b_f
            r1_f@paired_option <- theObject@paired_option
            
            return(r1_f)
          }
)

setGeneric(name="group_a_and_group_b_in_header",
           def=function(theObject, data_header_unique_f)
           {
             standardGeneric("group_a_and_group_b_in_header")
           }
)

setMethod(f="group_a_and_group_b_in_header",
          signature="two_groups_test",
          definition=function(theObject, data_header_unique_f)
          {
            # _Checks if the selected groups exist in the data for the two groups test
            output_f <- list()
            group_a_in_header <- TRUE
            for(group_a_f_el in theObject@group_a) {
              if(!is.element(group_a_f_el, data_header_unique_f)) {
                group_a_in_header <- FALSE
              }
            }
            group_b_in_header <- TRUE
            for(group_b_f_el in theObject@group_b) {
              if(!is.element(group_b_f_el, data_header_unique_f)) {
                group_b_in_header <- FALSE
              }
            }
            output_f$a <- group_a_in_header
            output_f$b <- group_b_in_header
            return(output_f)
          }
)

setGeneric(name="get_sorted_indexes",
           def=function(theObject, group_f, class_labels_csv_vector_f)
           {
             standardGeneric("get_sorted_indexes")
           }
)

setMethod(f="get_sorted_indexes",
          signature="two_groups_test",
          definition=function(theObject, group_f, class_labels_csv_vector_f)
          {
            # _Makes lists of sorted class label indexes of samples as a part of preparation for a two groups statistical test
            ind <- c()
            for(group_f_el in group_f) {
              ind <- c(ind, which(class_labels_csv_vector_f == group_f_el))
            }
            ind <- sort(ind, decreasing = FALSE)
            return(ind)
          }
)




