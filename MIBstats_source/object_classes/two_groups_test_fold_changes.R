# _Method for calculating fold changes, belongs to the two_groups_test class

setGeneric(name="calculate_fold_changes",
           def=function(theObject)
           {
             standardGeneric("calculate_fold_changes")
           }
)

setMethod(f="calculate_fold_changes",
          signature="two_groups_test",
          definition=function(theObject)
          {
            test_type_f <- theObject@test_type
            label_f <- theObject@label
            t1_f <- theObject@input_data
            group_a_f <- theObject@group_a
            group_b_f <- theObject@group_b
            paired_option <- theObject@paired_option
            extra_params_f <- theObject@extra_params
            
            
            max_invalid_val_ratio <- theObject@max_invalid_val_ratio

            
            get_fold_changes <- function(data_row_a_f2, data_row_b_f2, paired_option_f2) {
              result_f2 <- list()
              
              
              theObject@input_data
              
              is_valid_number <- function(x) {
                is_valid <- TRUE
                if(is.null(x) || !is.numeric(x) || is.na(x) || is.nan(x) || is.infinite(x)) {
                  is_valid <- FALSE
                }
                return(is_valid)
              }
              
              get_fold_change <- function(a, b, log_option, log_base_f) {
                # _Function for calculating fold change of two values. Initial value is a and final value is b
                
                if(is_valid_number(a) | is_valid_number(b)) {
                  fold_ch_f <- NULL
                  if(log_option) {
                    fold_ch_f <- log_base_f^(a-b)
                  } else {
                    if(b != 0) {
                      fold_ch_f <- a/b
                    } else {
                      fold_ch_f <- NaN
                    }
                  }
                } else {
                  fold_ch_f <- NaN
                }
  
                return(fold_ch_f)
              }
              
              fold_change_of_means <- function(gr_a, gr_b, log_option, log_base_f) {
                # _Function for calculating fold change of the means of two groups (vectors). Initial values are gr_a and final values are gr_b
                fold_change_f <- NA
                if(!is.na(mean(gr_b)) && mean(gr_b) != 0) {
                  fold_change_f <- get_fold_change(mean(gr_a), mean(gr_b), log_option, log_base_f)
                }
                return(fold_change_f)
              }
              
              fold_change_paired_samples <- function(gr_a, gr_b, log_option, log_base_f) {
                # _Function for calculating fold changes of a set of paired samples (vectors). Initial values are gr_a and final value are gr_b
                fold_changes_f <- NULL
                
                elem_counter <- 1
                for(elem in gr_a) {
                  elem_fold_change <- NA
                  if(!is.na(as.numeric(gr_b[elem_counter])) && as.numeric(gr_b[elem_counter]) != 0) {
                    elem_fold_change <- get_fold_change(as.numeric(elem), as.numeric(gr_b[elem_counter]), log_option, log_base_f)
                  }
                  fold_changes_f <- c(fold_changes_f, elem_fold_change)
                  elem_counter <- elem_counter + 1
                }
                return(fold_changes_f)
              }
              
              log_transformed_flag <- theObject@input_data@log_transformed
              log_base <- theObject@input_data@log_base
              
              result_f2$group1_vs_group2 <- fold_change_of_means(data_row_a_f2, data_row_b_f2, log_transformed_flag, log_base)
              result_f2$group2_vs_group1 <- fold_change_of_means(data_row_b_f2, data_row_a_f2, log_transformed_flag, log_base)
              
              if(paired_option_f2) {
                result_f2$group1_vs_group2_paired_f <- fold_change_paired_samples(data_row_a_f2, data_row_b_f2, log_transformed_flag, log_base)
                result_f2$group2_vs_group1_paired_f <- fold_change_paired_samples(data_row_b_f2, data_row_a_f2, log_transformed_flag, log_base)
              }
              
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

            two_groups_test_failed <- FALSE
            error_message <- c()
            error_messages_list <- list(main = NULL, data = NULL, class_labels = NULL)
            error_encountered <- FALSE
            mydata <- mydata_csv[, 2:ncol(mydata_csv)]

            row_titles <- mydata_csv[, 1:1]

            class_labels_csv_vector <- as.vector(class_labels_csv[,2])
            data_header_unique <- unique(class_labels_csv_vector)

            fold_changes_list_group1_vs_group2 <- c()
            fold_changes_list_group2_vs_group1 <- c()
            
            paired_fold_changes_group1_vs_group2 <- list()
            paired_fold_changes_group2_vs_group1 <- list()
            
            two_groups_test_full_results_list <- list()

            data_row_a_list <- list()
            data_row_b_list <- list()
            out_frame <- data.frame()
            test_performed_f <- FALSE

            groups_in_header <- group_a_and_group_b_in_header(theObject, data_header_unique)


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

                  mtry <- try(withCallingHandlers(get_fold_changes(data_row_a, data_row_b, paired_option), warning = w.handler, error = e.handler), silent = TRUE)

                  warning = w.list
                  error = e.list
                  
                  if(length(warning) > 0) {
                  }
                  if(length(error) > 0) {
                  }

                  if ((length(warning) == 0) && (length(error) == 0)) {
                    two_groups_test_full_result <- get_fold_changes(data_row_a, data_row_b, paired_option)

                    # _Performs the statistical test and stores the full report on the result

                    fold_change_group1_vs_group2 <- two_groups_test_full_result$group1_vs_group2
                    fold_change_group2_vs_group1 <- two_groups_test_full_result$group2_vs_group1
                    
                    fold_changes_list_group1_vs_group2 <- c(fold_changes_list_group1_vs_group2, fold_change_group1_vs_group2)
                    fold_changes_list_group2_vs_group1 <- c(fold_changes_list_group2_vs_group1, fold_change_group2_vs_group1)
                    
                    
                    if(paired_option) {
                      paired_fold_changes_group1_vs_group2 <- append(paired_fold_changes_group1_vs_group2, list(two_groups_test_full_result$group1_vs_group2_paired_f))
                      paired_fold_changes_group2_vs_group1 <- append(paired_fold_changes_group2_vs_group1, list(two_groups_test_full_result$group2_vs_group1_paired_f))
                    }
                    
                    two_groups_test_full_results_list <- append(two_groups_test_full_results_list, list(two_groups_test_full_result))

                    # _Stores test statistics and input data for using it in the output of the function
                  } else {

                    error_messages_list$main <- "Variables were encountered with which the test could not be performed. "
                    two_groups_test_failed <- TRUE
                  }
                } else {
                  error_messages_list$data <- "Error: non-numeric data. "
                  two_groups_test_failed <- TRUE
                }

                if(two_groups_test_failed == TRUE) {

                  two_groups_test_full_results_list <- append(two_groups_test_full_results_list, list(NaN))
                  error_encountered <- TRUE
                }
              }


              
              
              if(two_groups_test_failed == FALSE) {
                out_frame <- data.frame(variable_index = row_titles, group1_vs_group2_fc = fold_changes_list_group1_vs_group2, group2_vs_group1_fc = fold_changes_list_group2_vs_group1)
                test_performed_f <- TRUE
              }
              

            } else {
              error_messages_list$class_labels <- "Error: the specified class labels were not found in the table. "
              error_encountered <- TRUE
            }
            
            
            if(paired_option) {
            }
            
            
            error_message <- paste(error_messages_list$main, error_messages_list$data, error_messages_list$class_labels, sep="")
            
            # _Puts the variables and test statistics in a data frame for outputting

            r1_f@out_frame <- out_frame # _Data frame that contains row indexes and p-values (p-values are corrected for multiple testing)

            r1_f@full_results_list <- two_groups_test_full_results_list 

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