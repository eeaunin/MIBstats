# _The class for two groups test queue. Contains the test queue that is set up on the page of setting up a statistical test

two_groups_test_queue <- setClass("two_groups_test_queue", slots = c(l = "list", test_queue_serial_nr = "numeric"),

prototype=list(
  l = list(two_sample_test_group = NULL, queued_two_sample_tests = NULL, queued_two_sample_tests_frame = NULL, queued_two_sample_tests_pairings = NULL, editing_queued_two_sample_tests_row_index = NULL), test_queue_serial_nr = 0
))


setGeneric(name="run_two_groups_test",
           
           # _Goes through the list of queued two groups tests and sets them to run, one by one
           
           def=function(theObject, t1_f, test_type_f, additional_f, max_allowed_na_ratio_f)
           {
             standardGeneric("run_two_groups_test")
           }
)

setMethod(f="run_two_groups_test",
          signature="two_groups_test_queue",
          definition=function(theObject, t1_f, test_type_f, additional_f, max_allowed_na_ratio_f)
          {
            performed_tests_count <- 0
            
            # _Goes through the list of queued two groups tests and sets them to run, one by one
            
            queued_two_sample_tests_f <- theObject@l$queued_two_sample_tests$l
            queued_two_sample_tests_pairings_l <- theObject@l$queued_two_sample_tests_pairings$l
            
            new_rtr_item <- two_groups_test_results_tracker()
            
            
            test_item_counter <- 1
            for(item_q in queued_two_sample_tests_f) {
              
              item_q2 <- item_q[[1]]
              
              
              
              columns_to_be_deleted_f <- get_columns_to_be_deleted_based_on_subsets(t1_f, item_q2)
              input_data_f <- filter_t1_based_on_subsets(t1_f, columns_to_be_deleted_f)
              
              
              pairings_f <- queued_two_sample_tests_pairings_l[[test_item_counter]]
              
              if(length(pairings_f) == 2) {
                input_data_f <- rearrange_input_data_columns_based_on_sample_pairing(input_data_f, pairings_f)
                
              }
              
              opt_f <- two_groups_test_extract_input_data_for_report(theObject, item_q2)
              
              welch_string <- ""
              alternative_hypothesis_string <- ""
              if(test_type_f == "t-test") {
                if(additional_f$welch){
                  welch_string <- "Welch correction"
                } else {
                  welch_string <- "Student's t-test"
                }
              }
              
              if(test_type_f == "t-test" | test_type_f == "mann-whitney_u_test") {
                if(additional_f$alternative_hypothesis == "alternative_hypothesis_two_sided") {
                  alternative_hypothesis_string <- "two-tailed"
                } else if (additional_f$alternative_hypothesis == "alternative_hypothesis_less" || additional_f$alternative_hypothesis == "alternative_hypothesis_greater") {
                  alternative_hypothesis_string <- "one-tailed"
                }
              }
              
              extra_params_f <- list()
              if(test_type_f == "t-test") {
                extra_params_f$welch_correction_option <- additional_f$welch
              }
              
              if(test_type_f == "t-test" | test_type_f == "mann-whitney_u_test") {
                extra_params_f$alternative_hypothesis <- additional_f$alternative_hypothesis
              }
              
              new_two_groups_test <- NULL
              
              if(test_type_f == "rank_product" | test_type_f == "rank_sum") {
                new_two_groups_test <- rank_product_calculator(test_type = test_type_f, label = item_q2$label, input_data = input_data_f, group_a = item_q2$groups1, group_b = item_q2$groups2, paired_option = item_q2$paired, extra_params = extra_params_f, max_invalid_val_ratio = max_allowed_na_ratio_f, p_adjustment = item_q2$p_adjustment)
              } else {
                new_two_groups_test <- two_groups_test(test_type = test_type_f, label = item_q2$label, input_data = input_data_f, group_a = item_q2$groups1, group_b = item_q2$groups2, paired_option = item_q2$paired, extra_params = extra_params_f, max_invalid_val_ratio = max_allowed_na_ratio_f, p_adjustment = item_q2$p_adjustment)
              }

              
              # _The test is triggered here
              two_groups_test_result_f <- NULL
              if(test_type_f == "fold_changes") {
                two_groups_test_result_f <- calculate_fold_changes(new_two_groups_test)
              } else if(test_type_f == "rank_product") {
                two_groups_test_result_f <- calculate_rank_product(new_two_groups_test, calculateP = TRUE)
              } else if(test_type_f == "rank_sum") {
                two_groups_test_result_f <- calculate_rank_product(new_two_groups_test, calculateP = FALSE)
              } else if (test_type_f == "t-test" | test_type_f == "f-test" | test_type_f == "mann-whitney_u_test") {
                two_groups_test_result_f <- two_groups_test_on_data_frame(new_two_groups_test)
              } else if (test_type_f == "shapiro_test") {
                two_groups_test_result_f <- shapiro_test_on_data_frame(new_two_groups_test)
              }
              
              if(two_groups_test_result_f@test_performed) {
                performed_tests_count <- performed_tests_count + 1
                
                two_groups_test_result_f@test_queue_item_details <- item_q2
                two_groups_test_result_f@test_queue_item_details$pairings <- pairings_f
                
                new_rtr_item@results$l <- append(new_rtr_item@results$l, list(two_groups_test_result_f))
                
                
                p_val_correction_name <- get_full_name_of_p_val_adjustment_method(item_q2$p_adjustment)
                
                
                selectitem_entry_name <- ""
                if(test_type_f == "t-test") {
                  selectitem_entry_name <- paste(toString(test_item_counter), "[", as.character(theObject@test_queue_serial_nr), "]", " | ", opt_f$label_string, toString(item_q2$groups1), " vs ", toString(item_q2$groups2), "; ", opt_f$paired_string, "; ", welch_string, "; ", alternative_hypothesis_string, "; ", "p-value correction: ", p_val_correction_name, sep = "") 
                } else if (test_type_f == "mann-whitney_u_test"){
                  selectitem_entry_name <- paste(toString(test_item_counter), "[", as.character(theObject@test_queue_serial_nr), "]"," | ", opt_f$label_string, toString(item_q2$groups1), " vs ", toString(item_q2$groups2), "; ", opt_f$paired_string, "; ", alternative_hypothesis_string, "; ", "p-value correction: ", p_val_correction_name, sep = "") 
                } else if (test_type_f == "f-test" | test_type_f == "shapiro_test"){
                  selectitem_entry_name <- paste(toString(test_item_counter), "[", as.character(theObject@test_queue_serial_nr), "]", " | ", opt_f$label_string, toString(item_q2$groups1), " vs ", toString(item_q2$groups2), "; ", opt_f$paired_string, "; ", "p-value correction: ", p_val_correction_name, sep = "") 
                } else if (test_type_f == "fold_changes" | test_type_f == "rank_product" | test_type_f == "rank_sum") {
                  selectitem_entry_name <- paste(toString(test_item_counter), "[", as.character(theObject@test_queue_serial_nr), "]", " | ", opt_f$label_string, toString(item_q2$groups1), " vs ", toString(item_q2$groups2), "; ", opt_f$paired_string, sep = "") 
                }
                new_rtr_item@results_pages$l <- append(new_rtr_item@results_pages$l, list(selectitem_entry_name))
                
              }
              if(two_groups_test_result_f@error_encountered){
                show_generic_error_message("Error encountered")
                error_report_t <- two_groups_test_error_report_for_output(theObject, two_groups_test_result_f)
                show_error_message_two_groups_test_preparation(theObject, error_report_t)
              }
              
              test_item_counter = test_item_counter + 1
            }
            if(performed_tests_count == 0) {
              new_rtr_item <- NULL
            }
            
            
            return(new_rtr_item)
          }
)


setGeneric(name="two_groups_test_error_report_for_output",
           def=function(theObject, two_groups_test_result_f)
           {
             standardGeneric("two_groups_test_error_report_for_output")
           }
)

setMethod(f="two_groups_test_error_report_for_output",
          signature="two_groups_test_queue",
          definition=function(theObject, two_groups_test_result_f)
          {
            # _Report after running a test with two groups
            if(two_groups_test_result_f@error_encountered) {
              error_report <- ("A problem with some samples occurred in the statistical test.\n")
              error_report <- paste0(error_report, two_groups_test_result_f@errors)
            }
            return(error_report)
          }
)

setGeneric(name="two_groups_test_extract_input_data_for_report",
           def=function(theObject, item_q2_f)
           {
             standardGeneric("two_groups_test_extract_input_data_for_report")
           }
)

setMethod(f="two_groups_test_extract_input_data_for_report",
          signature="two_groups_test_queue",
          definition=function(theObject, item_q2_f)
            
            # _Takes test queue information and formats it for storing in test results
          {
            output_f <- list()
            paired_string <- ""
            if(item_q2_f$paired){
              paired_string <- "paired"
            } else {
              paired_string <- "unpaired"
            }
            
            label_string <- ""
            if(item_q2_f$label != "") {
              label_string <- paste(item_q2_f$label, "; ")
            }
            output_f$paired_string <- paired_string
            output_f$label_string <- label_string
            return(output_f)
          }
)

setGeneric(name="new_entry_for_class_label_vectors_to_html_report",
           def=function(theObject, old_result_f, subsets_i, class_labels_i)
           {
             standardGeneric("new_entry_for_class_label_vectors_to_html_report")
           }
)

setMethod(f="new_entry_for_class_label_vectors_to_html_report",
          signature="two_groups_test_queue",
          definition=function(theObject, old_result_f, subsets_i, class_labels_i)
          {
            result_f_new <- old_result_f
            
            
            subsets_i_output <- unlist(strsplit(subsets_i, "_"))
            subsets_i_output <- subsets_i_output[1:length(subsets_i_output)-1]
            subsets_i_output <- as.character(subsets_i_output, sep = " ")
            subsets_i_output <- paste(subsets_i_output, sep = "", collapse = " ")
            
            if(names(subsets_i) != "") {
              result_f_new <- paste(old_result_f, class_labels_i, " [", subsets_i_output, " (", names(subsets_i), ")]<br>", sep="")
            } else {
              if(subsets_i == "None") {
                result_f_new <- paste(old_result_f, class_labels_i, "<br>", sep="")
              } else {
                result_f_new <- paste(old_result_f, class_labels_i, " [", subsets_i_output, "]<br>", sep="")
              }
            }
            return(result_f_new)
          }
)

setGeneric(name="class_label_vectors_to_html_report",
           def=function(theObject, class_labels_a_f, subsets_a_f, class_labels_b_f, subsets_b_f)
           {
             standardGeneric("class_label_vectors_to_html_report")
           }
)

setMethod(f="class_label_vectors_to_html_report",
          signature="two_groups_test_queue",
          definition=function(theObject, class_labels_a_f, subsets_a_f, class_labels_b_f, subsets_b_f)
          {
            result_f <- ""
            max_length_f <- 0
            if(!is.null(class_labels_a_f) || !is.null(class_labels_b_f)) {

              if(length(class_labels_a_f) >= length(class_labels_b_f)) {
                max_length_f <- length(class_labels_a_f)
              } else {
                max_length_f <- length(class_labels_b_f)
              }
              result_f <- paste(result_f, "<b>Group #1:</b><br>")
              if(length(class_labels_a_f) == 0) {
                result_f <- paste(result_f, "(none)<br>")
              }
              for(i in 1:max_length_f) {
                if(i <= length(class_labels_a_f)) {
                  result_f <- new_entry_for_class_label_vectors_to_html_report(theObject, result_f, subsets_a_f[i], class_labels_a_f[i])
                }
              }
              result_f <- paste(result_f, "<br>")
              result_f <- paste(result_f, "<b>Group #2:</b><br>")
              if(length(class_labels_b_f) == 0) {
                result_f <- paste(result_f, "(none)<br>")
              }
              for(i in 1:max_length_f) {
                if(i <= length(class_labels_b_f)) {
                  result_f <- new_entry_for_class_label_vectors_to_html_report(theObject, result_f, subsets_b_f[i], class_labels_b_f[i])
                }
              }
              result_f <- paste(result_f, "<br>")
            }
            return(result_f)
          }
)

setGeneric(name="two_groups_html_report_to_data_frame",
           def=function(theObject, html_string)
           {
             standardGeneric("two_groups_html_report_to_data_frame")
           }
)

setMethod(f="two_groups_html_report_to_data_frame",
          signature="two_groups_test_queue",
          definition=function(theObject, html_string)
            
          # _Prepares the table that is displayed on the page of setting up a statistical test and contains the lists of samples assigned to group 1 and group 2 (which the user plans to add to the test queue)
          {
            html_string_split <- unlist(strsplit(html_string, "<b>Group #2:</b><br>"))
            html_items <- list()
            html_items$a <- html_string_split[1]
            html_items$b <- html_string_split[2]
            
            html_items$a <- unlist(strsplit(html_items$a, "<b>Group #1:</b><br>"))[2]
            html_items$a <- unlist(strsplit(html_items$a, "<br> <br>"))[1]
            html_items$b <- unlist(strsplit(html_items$b, "<br> <br>"))[1]
            
            html_items$a <- unlist(strsplit(html_items$a, "<br>"))
            html_items$b <- unlist(strsplit(html_items$b, "<br>"))
            
            if(length(html_items$a) != length(html_items$b)) {
              if(length(html_items$a) > length(html_items$b)) {
                for (i in 1:(length(html_items$a)-length(html_items$b))) {
                  html_items$b <- c(html_items$b, " ")
                }
              } else if(length(html_items$b) > length(html_items$a)) {
                for (i in 1:(length(html_items$b)-length(html_items$a))) {
                  html_items$a <- c(html_items$a, " ")
                }
              }
            }
            groups_report_frame <- cbind(data.frame(html_items$a, html_items$b))
            colnames(groups_report_frame) <- c("Group #1", "Group #2")
            return(groups_report_frame)
          }
)

setGeneric(name="two_sample_test_queue_already_contains_the_item",
           def=function(theObject, new_item_f)
           {
             standardGeneric("two_sample_test_queue_already_contains_the_item")
           }
)

setMethod(f="two_sample_test_queue_already_contains_the_item",
          signature="two_groups_test_queue",
          definition=function(theObject, new_item_f)
            
            # _Method to check if an item has already been added to the two groups test queue. Duplicate items are not allowed
          {
            item_found_f <- FALSE
            queue_f <- theObject@l$queued_two_sample_tests$l
            
            for(i in queue_f) {
              group_items_identical <- FALSE
              
              if((length(i[[1]]$groups1) == length(new_item_f$groups1)) && ((length(i[[1]]$groups2) == length(new_item_f$groups2)))) {
                if((i[[1]]$groups1 == new_item_f$groups1) && (i[[1]]$groups2 == new_item_f$groups2)) {
                  if((i[[1]]$subset_g1 == new_item_f$subset_g1) && (i[[1]]$subset_g2 == new_item_f$subset_g2)) {
                    group_items_identical <- TRUE
                  }
                }
              }
              if((length(i[[1]]$groups1) == length(new_item_f$groups2)) && ((length(i[[1]]$groups2) == length(new_item_f$groups1)))) {
                if((i[[1]]$groups1 == new_item_f$groups2) && (i[[1]]$groups2 == new_item_f$groups1)) {
                  if((i[[1]]$subset_g1 == new_item_f$subset_g2) && (i[[1]]$subset_g2 == new_item_f$subset_g1)) {
                    group_items_identical <- TRUE 
                  }
                }
              }
              if(group_items_identical == TRUE && (i[[1]]$welch == new_item_f$welch) && (i[[1]]$paired == new_item_f$paired) && (i[[1]]$p_adjustment == new_item_f$p_adjustment)) {
                item_found_f <- TRUE
              }
            }
            return(item_found_f)
          }
)

setGeneric(name="find_all_possible_pairs_of_class_labels",
           def=function(theObject, class_labels_f)
           {
             standardGeneric("find_all_possible_pairs_of_class_labels")
           }
)

setMethod(f="find_all_possible_pairs_of_class_labels",
          signature="two_groups_test_queue",
          definition=function(theObject, class_labels_f)
            # _Method for finding all possible pairs of sample classes in the data. Used on the page of setting up the statistical tests (for the option of filling the test queue with all possible pairs of sample classes)
          {
            combinations_f <- t(combn(class_labels_f, 2))
            return(combinations_f)
          }
)

setGeneric(name="create_new_test_queue_entry",
           def=function(theObject, two_sample_test_group_a_f, two_sample_test_group_a_subset_f, two_sample_test_group_b_f, two_sample_test_group_b_subset_f, var_equal_false_checkbox_f, paired_test_checkbox_f, descriptive_label_f, multiple_testing_correction_select_f)
           {
             standardGeneric("create_new_test_queue_entry")
           }
)

setMethod(f="create_new_test_queue_entry",
          signature="two_groups_test_queue",
          definition=function(theObject, two_sample_test_group_a_f, two_sample_test_group_a_subset_f, two_sample_test_group_b_f, two_sample_test_group_b_subset_f, var_equal_false_checkbox_f, paired_test_checkbox_f, descriptive_label_f, multiple_testing_correction_select_f)
          # _Method for adding a new entry to the test queue
          {
            new_test_queue_entry_f <- list()
            new_test_queue_entry_f$groups1 <- two_sample_test_group_a_f
            new_test_queue_entry_f$subset_g1 <- two_sample_test_group_a_subset_f
            
            new_test_queue_entry_f$groups2 <- two_sample_test_group_b_f
            new_test_queue_entry_f$subset_g2 <- two_sample_test_group_b_subset_f
            
            new_test_queue_entry_f$welch <- var_equal_false_checkbox_f
            new_test_queue_entry_f$paired <- paired_test_checkbox_f
            new_test_queue_entry_f$label <- descriptive_label_f
            new_test_queue_entry_f$p_adjustment <- multiple_testing_correction_select_f
            return(new_test_queue_entry_f)
          }
)

setGeneric(name="prepare_all_possible_pairings_of_class_labels_for_two_groups_test",
           def=function(theObject, class_labels_f, var_equal_false_checkbox_f, paired_test_checkbox_f, descriptive_label_f, multiple_testing_correction_select_f)
           {
             standardGeneric("prepare_all_possible_pairings_of_class_labels_for_two_groups_test")
           }
)

setMethod(f="prepare_all_possible_pairings_of_class_labels_for_two_groups_test",
          signature="two_groups_test_queue",
          definition=function(theObject, class_labels_f, var_equal_false_checkbox_f, paired_test_checkbox_f, descriptive_label_f, multiple_testing_correction_select_f)
            # _Method for adding all possible pairs of sample classes in the data to the test queue. Used on the page of setting up the statistical tests (for the option of filling the test queue with all possible pairs of sample classes)
          {
            
            new_test_queue_entries_f <- list()
            counter_f <- 0
            class_labels_vector_f <- unique_class_labels_df_to_vector(theObject, class_labels_f)
            combinations_f <- find_all_possible_pairs_of_class_labels(theObject, class_labels_vector_f)
            
            for (i in 1:nrow(combinations_f)) {
              labels_pair <- combinations_f[i, ]
              new_test_queue_entry_f <- create_new_test_queue_entry(theObject, labels_pair[[1]], "None", labels_pair[[2]], "None", var_equal_false_checkbox_f, paired_test_checkbox_f, descriptive_label_f, multiple_testing_correction_select_f)
              new_test_queue_entries_f <- append(new_test_queue_entries_f, list(new_test_queue_entry_f))
            }
            
            return(new_test_queue_entries_f)
          }
)



setGeneric(name="add_entry_to_two_groups_test_queue",
           def=function(theObject, t1_f, new_test_queue_entry_f)
           {
             standardGeneric("add_entry_to_two_groups_test_queue")
           }
)

setMethod(f="add_entry_to_two_groups_test_queue",
          signature="two_groups_test_queue",
          definition=function(theObject, t1_f, new_test_queue_entry_f)
            # _Method for adding an entry to the queue of statistical tests
          {
            queued_two_sample_tests_f <- theObject@l$queued_two_sample_tests
            queued_two_sample_tests_frame_f <- theObject@l$queued_two_sample_tests_frame
            class_labels_frame_f <- t1_f@class_labels
            
            error_occurred_f <- FALSE
            error_message_f <- ""
            output_f <- list()
            queue_previously_empty <- FALSE
            if(nrow(queued_two_sample_tests_frame_f$df) == 0) {
              queue_previously_empty <- TRUE
            }
            if(!is.null(new_test_queue_entry_f$groups1) && !is.null(new_test_queue_entry_f$groups2)) {
              paired_test_possible <- check_sample_size_equality_for_paired_two_groups_test(theObject, t1_f, new_test_queue_entry_f$groups1, new_test_queue_entry_f$subset_g1, new_test_queue_entry_f$groups2, new_test_queue_entry_f$subset_g2)
              if(paired_test_possible | !new_test_queue_entry_f$paired) {
                item_already_added <- FALSE
                
                item_already_added <- two_sample_test_queue_already_contains_the_item(theObject, new_test_queue_entry_f)
                
                if(!item_already_added) {
                  
                  
                  new_test_queue_entry_frame_f <- data.frame(new_test_queue_entry_f$label, groups1 = paste(new_test_queue_entry_f$groups1, collapse=" "), subset_g1 = paste(new_test_queue_entry_f$subset_g1, collapse=" "),
                                                             groups2 = paste(new_test_queue_entry_f$groups2, collapse=" "), subset_g2 = paste(new_test_queue_entry_f$subset_g2, collapse=" "), welch = new_test_queue_entry_f$welch, paired = new_test_queue_entry_f$paired, p_adjustment = new_test_queue_entry_f$p_adjustment)
                  names(new_test_queue_entry_frame_f) <- names(queued_two_sample_tests_frame_f$df)
                  queued_two_sample_tests_frame_f$df <- rbind(queued_two_sample_tests_frame_f$df, new_test_queue_entry_frame_f)
                  queued_two_sample_tests_f$l <- list.append(queued_two_sample_tests_f$l, list(new_test_queue_entry_f))
                  
                  output_f$queue_frame <- queued_two_sample_tests_frame_f$df
                  output_f$queue_list <- queued_two_sample_tests_f$l
                  
                  if(queue_previously_empty) {
                    tabs_to_enable <- c("tab_shapiro_wilk_test",
                      "tab_f_test",
                      "tab_t_test",
                      "tab_mann_whitney_u_test",
                      "tab_fold_changes",
                      "tab_rank_product",
                      "tab_rank_sum")
                    for(tab_item in tabs_to_enable) {
                      js$enableTab(tab_item)
                    }
                  }
                  
                } else {
                  error_message_f <- "This element has already been added"
                  error_occurred_f <- TRUE
                }
              } else {
                error_message_f <- "The selected samples are not suited for a paired test because the group sizes are not equal"
                error_occurred_f <- TRUE
              }
            }
            output_f$error_occurred <- error_occurred_f
            output_f$error_message <- error_message_f
            return(output_f)
          }
)

setGeneric(name="count_samples_in_a_group",
           def=function(theObject, t1_f, group_names, group_subsets, cl_table)
           {
             standardGeneric("count_samples_in_a_group")
           }
)

setMethod(f="count_samples_in_a_group",
          signature="two_groups_test_queue",
          definition=function(theObject, t1_f, group_names, group_subsets, cl_table)
          # _Method for counting samples in a group in the queue of statistical tests
          {
            
            group_size <- 0
            counter_f <- 1
            for(g_name in group_names) {
              
              current_subset <- group_subsets[[counter_f]]
              if(current_subset == "None") {
                group_size <- group_size + cl_table[[g_name]]
              } else {
                
                current_subset_samples <- get_sample_names_by_subset_name(t1_f, current_subset)
                number_of_subset_samples <- length(current_subset_samples)
                group_size <- group_size + number_of_subset_samples
              }
              counter_f <- counter_f + 1
            }
            return(group_size)
          }
)

setGeneric(name="check_sample_size_equality_for_paired_two_groups_test",
           def=function(theObject, t1_f, group1_names, group1_subsets, group2_names, group2_subsets)
           {
             standardGeneric("check_sample_size_equality_for_paired_two_groups_test")
           }
)

setMethod(f="check_sample_size_equality_for_paired_two_groups_test",
          signature="two_groups_test_queue",
          definition=function(theObject, t1_f, group1_names, group1_subsets, group2_names, group2_subsets)
          # _Method that checks if two sample groups have an equal number of samples when the user chooses to do a paired test. Paired tests cannot be done with groups with unequal sizes
          {
            idc1_f <- input_data_checker()
            idc1_f@class_labels_csv <- t1_f@class_labels
            paired_test_possible_f <- FALSE
            cl_table <- get_class_labels_table(idc1_f)
            
            group1_size <- count_samples_in_a_group(theObject, t1_f, group1_names, group1_subsets, cl_table)
            group2_size <- count_samples_in_a_group(theObject, t1_f, group2_names, group2_subsets, cl_table)
            
            if(group1_size == group2_size) {
              paired_test_possible_f <- TRUE
            }
            
            return(paired_test_possible_f)
          }
)

setGeneric(name="show_error_message_two_groups_test_preparation",
           def=function(theObject, error_message_f)
           {
             standardGeneric("show_error_message_two_groups_test_preparation")
           }
)

setMethod(f="show_error_message_two_groups_test_preparation",
          signature="two_groups_test_queue",
          definition=function(theObject, error_message_f)
            # _Method for displaying an error message
          {
            showModal(modalDialog(
              title = "Error",
              error_message_f
            ))
          }
)

setGeneric(name="find_paired_samples_in_two_groups",
           def=function(theObject, table_row_cl_f, merged_frame_f, info2_row_f)
           {
             standardGeneric("find_paired_samples_in_two_groups")
           }
)

setMethod(f="find_paired_samples_in_two_groups",
          signature="two_groups_test_queue",
          definition=function(theObject, table_row_cl_f, merged_frame_f, info2_row_f)
            
          # _Method for finding paired samples in the test queue. Used for displaying the information on which samples are paired on the screen
          {
            queued_two_sample_tests_pairings_l <- theObject@l$queued_two_sample_tests_pairings$l
            
            output_list_f <- list()
            output_text_full <- ""
            table_row_paired <- table_row_cl_f$paired
            
            
            table_cl_groups1 <- toString(table_row_cl_f$groups1)
            table_cl_groups2 <- toString(table_row_cl_f$groups2)
            
            group1_cl <- unlist(strsplit(table_cl_groups1, " "))
            group2_cl <- unlist(strsplit(table_cl_groups2, " "))
            
            class_labels_cl <- (merged_frame_f[2, ])
            
            group1_sample_labels <- c()
            group2_sample_labels <- c()
            
            sample_pairs_list <- queued_two_sample_tests_pairings_l[[info2_row_f]]
            if(table_row_paired & length(sample_pairs_list) == 2) {
              group1_sample_labels <- sample_pairs_list[[1]]
              group2_sample_labels <- sample_pairs_list[[2]]
            } else if(table_row_paired & queued_two_sample_tests_pairings_l[[info2_row_f]] == "<Empty>") {
              
              counter_cl <- 1
              for(cl_item in class_labels_cl[2:length(class_labels_cl)]) {
                sample_cl <- names(merged_frame_f)[counter_cl+1]
                if(is.element(cl_item, group1_cl)) {
                  group1_sample_labels <- c(group1_sample_labels, sample_cl)
                } else if (is.element(cl_item, group2_cl)) {
                  group2_sample_labels <- c(group2_sample_labels, sample_cl)
                }
                counter_cl <- counter_cl + 1
              }
            } else if(table_row_paired & queued_two_sample_tests_pairings_l[[info2_row_f]] != "<Empty>") {
              show_error_message_two_groups_test_preparation(theObject, "Corrupted sample pairings data")
            }
            
            if ((length(group1_sample_labels) == 0) & (length(group2_sample_labels) == 0)) return()
            
            output_text_cl <- ""
            output_header_cl <- ""
            output_header_cl <- paste(output_header_cl, "<b>test queue item nr. </b>", toString(info2_row_f), "<br>", sep="")
            output_header_cl <- paste(output_header_cl,"<b>test parameters:</b> [", sep="")
            if(table_row_cl_f$label != "") {
              output_header_cl <- paste("<b>label:</b> ", table_row_cl_f$label, "; ", sep="")
            }
            output_header_cl <- paste(output_header_cl, "<b>Group 1:</b> ", table_cl_groups1, "; <b>Group 2:</b> ", table_cl_groups2, "; <b>Welch's correction:</b> ", table_row_cl_f$welch, "; <b>paired:</b> ", table_row_cl_f$paired, "]<br>", sep = "")
            if(length(group1_sample_labels) == length(group2_sample_labels)) {
              for (cl_i in 1:length(group1_sample_labels)) {
                output_text_cl <- paste(output_text_cl, "[", group1_sample_labels[[cl_i]], ", ", group2_sample_labels[[cl_i]], "]<br>", sep="")
              }
              
              output_text_full <- paste("<b>Preview of the pairing of samples:</b><br>", output_header_cl, "<b>Sample pairs:</b><br>", output_text_cl)
            } else {
              show_error_message_two_groups_test_preparation(theObject, "Group sizes do not match")
            }
            output_list_f$output_text_full <- output_text_full
            output_list_f$group1_sample_labels <- group1_sample_labels
            output_list_f$group2_sample_labels <- group2_sample_labels
            return(output_list_f)
          }
)

setGeneric(name="unique_class_labels_df_to_vector",
           def=function(theObject, class_labels_df_f)
           {
             standardGeneric("unique_class_labels_df_to_vector")
           }
)

setMethod(f="unique_class_labels_df_to_vector",
          signature="two_groups_test_queue",
          definition=function(theObject, class_labels_df_f)
          # _Method for preparing a vector of unique class labels in the input data
          {
            unique_class_labels_f <- toString(unique(class_labels_df_f[, 2]))
            unique_class_labels_f <- unlist(strsplit(unique_class_labels_f, "[, ]"))
            unique_class_labels_f2 <- c()
            for (i in unique_class_labels_f) {
              if(i != "") {
                unique_class_labels_f2 <- c(unique_class_labels_f2, i)
              }
            }
            return(unique_class_labels_f2)
          }
)

setGeneric(name="get_two_groups_test_table_for_output",
           def=function(theObject)
           {
             standardGeneric("get_two_groups_test_table_for_output")
           }
)

setMethod(f="get_two_groups_test_table_for_output",
          signature="two_groups_test_queue",
          definition=function(theObject)
            
          # _Generates the data frame that contains the test queue (for displaying it on the page of setting up statistical tests)
          {
            output_frame_f <- theObject@l$queued_two_sample_tests_frame$df
            
            p_adjustment_col <- output_frame_f$p_adjustment
            p_adjustment_col_new <- NULL
            for(item_f in p_adjustment_col) {
              item_new <- get_full_name_of_p_val_adjustment_method(item_f)
              p_adjustment_col_new <- c(p_adjustment_col_new, item_new)
            }
            output_frame_f$p_adjustment <- p_adjustment_col_new
            header_f <- colnames(output_frame_f)
            output_header_f <- NULL
            for(item_f in header_f) {
              new_item_f <- item_f
              if(item_f == "label") {
                new_item_f <- "Label"
              } else if (item_f == "groups1") {
                new_item_f <- "Group 1"
              } else if (item_f == "groups2") {
                new_item_f <- "Group 2"
              } else if (item_f == "subset_g1") {
                new_item_f <- "Group 1 subsets"
              } else if (item_f == "subset_g2") {
                new_item_f <- "Group 2 subsets"
              } else if (item_f == "welch") {
                new_item_f <- "Welch correction"
              } else if (item_f == "paired") {
                new_item_f <- "Paired samples"
              } else if (item_f == "welch") {
                new_item_f <- "Welch correction"
              } else if (item_f == "p_adjustment") {
                new_item_f <- "Multiple comparisons correction"
              }
              output_header_f <- c(output_header_f, new_item_f)
            }
            colnames(output_frame_f) <- output_header_f
            return(output_frame_f)
          }
)



