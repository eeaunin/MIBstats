# _Class for storing test results

two_groups_test_result <- setClass("two_groups_test_result",
                                   slots = c(label = "character", associated_table = "character", test_type = "character", group_a = "character", group_b = "character", out_frame = "data.frame", unadjusted_out_frame = "data.frame", full_results_list = "list", unadjusted_full_results_list = "list", data_row_a = "list", data_row_b = "list", header_unique = "character", error_encountered = "logical", test_performed = "logical", errors = "list", item_name = "character", item_label = "character", group1_labels = "character", group2_labels = "character", p_adjustment = "character", paired_option = "logical", test_queue_item_details = "list", iga = "list"),
                                   prototype=list(
                                   label = "", associated_table = "", test_type = "", group_a = "", group_b = "", out_frame = data.frame(), unadjusted_out_frame = data.frame(), full_results_list = list(), unadjusted_full_results_list = list(), data_row_a = list(), data_row_b = list(), header_unique = "", error_encountered = FALSE, test_performed = FALSE, errors = list(), group1_labels = "", group2_labels = "", p_adjustment = "", paired_option = FALSE, item_name = "", item_label = "", test_queue_item_details = list(), iga = list())
                                   )

# _test_queue_item_details: is used to keep track of details test settings in order to match p-values and fold changes for volcano plot

setGeneric(name="replace_abbreviated_statistic_name_with_full_name",
           def=function(theObject, statistic_f)
           {
             standardGeneric("replace_abbreviated_statistic_name_with_full_name")
           }
)

setMethod(f="replace_abbreviated_statistic_name_with_full_name",
          signature="two_groups_test_result",
          definition=function(theObject, statistic_f)
            
            # _Method for converting abbreviated test statistic to its full name (for displaying on the screen)
          {
            
            get_full_name <- function(abbreviated_name, full_names_vector) {
              long_name_f <- abbreviated_name
              
              if(is.element(abbreviated_name, names(full_names_vector))) {
                long_name_f <- full_names_vector[abbreviated_name]
              }
              return(long_name_f)
            }
            
            t_test_statistics_full_names <- c("statistic" = "t-statistic",
                                              "parameter"="t-statistic's degrees of freedom",
                                              "p.value" = "p-value",
                                              "conf.int" = "Confidence interval",
                                              "estimate" = "Estimated difference in means",
                                              "null.value" = "Hypothesised value of the mean difference",
                                              "alternative" = "Alternative hypothesis",
                                              "method" = "Method")
            
            f_test_statistics_full_names <- c("statistic" = "F test statistic",
                                              "parameter"="Degrees of the freedom of the F distribution of the test statistic",
                                              "p.value" = "p-value",
                                              "conf.int" = "Confidence interval",
                                              "estimate" = "Ratio of the sample variances",
                                              "null.value" = "Ratio of population variances under the null hypothesis",
                                              "alternative" = "Alternative hypothesis",
                                              "method" = "Method")
            
            shapiro_test_statistics_full_names <- c("statistic" = "Shapiro-Wilk statistic",
                                                    "p.value" = "p-value",
                                                    "method" = "Method")
            
            test_type_f <- theObject@test_type
            name_new <- toString(statistic_f)
            if(test_type_f == "t-test") {
              name_new <- get_full_name(name_new, t_test_statistics_full_names)
            } else if(test_type_f == "f-test") {
              name_new <- get_full_name(name_new, f_test_statistics_full_names)

            } else if(test_type_f == "shapiro_test") {
              name_new <- get_full_name(name_new, shapiro_test_statistics_full_names)
            }
            return(name_new)
          }
)

setGeneric(name="format_two_groups_statistics_for_output",
           def=function(theObject, printing_list, decimal_places_f)
           {
             standardGeneric("format_two_groups_statistics_for_output")
           }
)

setMethod(f="format_two_groups_statistics_for_output",
          signature="two_groups_test_result",
          definition=function(theObject, printing_list, decimal_places_f)
            
            # _Takes test statistics stored in the object and formats them for displaying on the screen
          {
            printing_string <- ("")
            for (name in names(printing_list)) {
              if (toString(name) != "data.name") {

                name_new <- replace_abbreviated_statistic_name_with_full_name(theObject, toString(name))
    
                printing_string <- paste(printing_string, "<b>", name_new, ":</b> ", sep="")
                
                current_name_f <- printing_list[[name]]
                if (toString(name) != "p.value") {
                  if(is.numeric(current_name_f)) {
                    current_name_f <- round(current_name_f, digits = decimal_places_f)
                  }
                }
                
                printing_string <- paste(printing_string, toString(current_name_f), sep="")
                printing_string <- paste(printing_string, "<br>", sep="")
              }
            }
            return(printing_string)
          }
)

setGeneric(name="format_two_groups_statistics_for_output_as_table",
           def=function(theObject, printing_list, decimal_places_f)
           {
             standardGeneric("format_two_groups_statistics_for_output_as_table")
           }
)

setMethod(f="format_two_groups_statistics_for_output_as_table",
          signature="two_groups_test_result",
          definition=function(theObject, printing_list, decimal_places_f)
            
            # _Formats test statistics stored in the object as a table (for output)
          {
            titles_string <- ""
            values_string <- ""
            for (name in names(printing_list)) {
              if (toString(name) != "data.name") {
                
                name_new <- replace_abbreviated_statistic_name_with_full_name(theObject, toString(name))
                
                titles_string <- c(titles_string, name_new)
                
                current_name_f <- printing_list[[name]]
                if (toString(name) != "p.value") {
                  if(is.numeric(current_name_f)) {
                    current_name_f <- round(current_name_f, digits = decimal_places_f)
                  }
                }
                values_string <- c(values_string, toString(current_name_f))
                
              }
            }
            values_string <- values_string[2:length(values_string)]
            titles_string <- titles_string[2:length(titles_string)]
            
            
            for (item in values_string) {
            }
            
            for (item in titles_string) {
            }
            
            report_frame_f <- data.frame(values_string)
            rownames(report_frame_f) <- titles_string
            colnames(report_frame_f) <- "Test statistic"
            
            
            
            return(report_frame_f)
          }
)

setGeneric(name="format_two_groups_statistics_for_downloading",
           def=function(theObject, printing_list, decimal_places_f)
           {
             standardGeneric("format_two_groups_statistics_for_downloading")
           }
)

setMethod(f="format_two_groups_statistics_for_downloading",
          signature="two_groups_test_result",
          definition=function(theObject, printing_list, decimal_places_f)
          {
            # _Prepares list of test statistics for downloading
            printing_string <- ("")
            names_counter <- 1
            for (name in names(printing_list)) {
              if (toString(name) != "data.name") {
                name_new <- replace_abbreviated_statistic_name_with_full_name(theObject, toString(name))
                printing_string <- paste(printing_string, name_new, ": ", sep="")
                
                current_name_f <- printing_list[[name]]
                if (toString(name) != "p.value") {
                  if(is.numeric(current_name_f)) {
                    current_name_f <- round(current_name_f, digits = decimal_places_f)
                  }
                }
                printing_string <- paste(printing_string, toString(current_name_f), sep="")
                if(names_counter < length(names(printing_list))-1) {
                  printing_string <- paste(printing_string, "\n", sep="")
                } else {
                  printing_string <- paste(printing_string, sep="")
                }
                
              }
              names_counter <- names_counter + 1
            }
            return(printing_string)
          }
)


setGeneric(name="format_two_groups_full_statistics_for_downloading",
           def=function(theObject, header_f, pval_correction_mode_f, data_rtr_sel_f)
           {
             standardGeneric("format_two_groups_full_statistics_for_downloading")
           }
)

setMethod(f="format_two_groups_full_statistics_for_downloading",
          signature="two_groups_test_result",

          definition=function(theObject, header_f, pval_correction_mode_f, data_rtr_sel_f)
            
            # _Prepares full list of test statistics for downloading
            
          {
            two_groups_test_full_results_f <- NULL
            if(pval_correction_mode_f) {
              two_groups_test_full_results_f <- theObject@full_results_list
            } else if (!pval_correction_mode_f) {
              two_groups_test_full_results_f <- theObject@unadjusted_full_results_list
            }
            
            
            row_titles_f2 <- theObject@out_frame$variable_index
            
            repeats <- 1
            if(data_rtr_sel_f==1) {
              repeats <- 4
            }
            out_string <- NULL
            for(i in 1:repeats) {
              printing_string2 <- paste(header_f, "\n\n", sep="")
              counter_f <- 1
              two_groups_test_full_results_f3 <- NULL
              if(data_rtr_sel_f==1) {
                two_groups_test_full_results_f3 <- two_groups_test_full_results_f[[i]]
              } else {
                two_groups_test_full_results_f3 <- two_groups_test_full_results_f
              }
              for(item_f in two_groups_test_full_results_f3) {
                printing_string2 <- paste(printing_string2, "row: ", counter_f, "\n", sep="")
                printing_string2 <- paste(printing_string2, "variable: ", row_titles_f2[[counter_f]], "\n", sep="")
                printing_string2 <- paste(printing_string2, format_two_groups_statistics_for_downloading(theObject, item_f, 100), sep="")
                if(counter_f < length(two_groups_test_full_results_f)) {
                  printing_string2 <- paste(printing_string2, "\n\n", sep="")
                }
                counter_f <- counter_f + 1
              }
              out_string <- paste(out_string, printing_string2)
            }
            
            return(out_string)
          }
)

setGeneric(name="extract_two_groups_test_data_for_box_plot",
           def=function(theObject, row_index_f)
           {
             standardGeneric("extract_two_groups_test_data_for_box_plot")
           }
)

setMethod(f="extract_two_groups_test_data_for_box_plot",
          signature="two_groups_test_result",
          definition=function(theObject, row_index_f)
          {
            # _Extracts the data for box plot of the results of one variable that is displayed on the results pages of the t-test, F-test and Mann-Whitney U test
            
            data_row_a_f <- theObject@data_row_a
            data_row_b_f <- theObject@data_row_b
            a_names_f <- gsub(" ", "_", theObject@group1_labels)
            b_names_f <- gsub(" ", "_", theObject@group2_labels)
            
            
            row_a <- unname(unlist(data_row_a_f[[row_index_f]]))
            row_b <- unname(unlist(data_row_b_f[[row_index_f]]))
            
            concat_a_names_f <- paste(a_names_f, collapse = ", ")
            concat_b_names_f <- paste(b_names_f, collapse = ", ")
            
            rows_list <- list(row_a, row_b)
            rows_frame <- plyr::ldply(rows_list, rbind)
            rows_frame <- t(rows_frame)
            a0 <- c(rows_frame[,1])
            a1 <- c(rows_frame[,2])
            rows_frame = data.frame(v1=a0, v2=a1)
            colnames(rows_frame) <- c(concat_a_names_f, concat_b_names_f)
            return(rows_frame)
          }
)


