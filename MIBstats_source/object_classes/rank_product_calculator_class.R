library(RankProd)
# _This class is for calculating rank product using the RankProd package

rank_product_calculator <- setClass("rank_product_calculator", 
                            slots = c(placeholder = "character"),
                            contains = c("two_groups_test"),
                            # _Set the default values for the slots. (optional)
                            prototype=list(
                              placeholder = "placeholder")
                            )

setGeneric(name="calculate_rank_product",
           def=function(theObject, calculateP)
           {
             standardGeneric("calculate_rank_product")
           }
)

setMethod(f="calculate_rank_product",
          signature="rank_product_calculator",
          definition=function(theObject, calculateP)
            
          # _Method for calculating rank product or rank sum.
          # _The calculateP option toggles between calculating rank product (if TRUE) or rank sum (if FALSE)
          {
            
            t1_f <- theObject@input_data
            
            log_transformed_bool <- theObject@input_data@log_transformed

            get_rp_input_data <- function() {
              # _Takes input data stored in the object and formats it so that it can be used for RankProducts
              out_list <- list()
              
              get_numeric_class_labels_vector <- function(class_labels_vector_cropped_f) {
                out_list <- list()
                cl_f <- NULL
                for(i in 1:length(class_labels_vector_cropped_f)) {
                  if(is.element(class_labels_vector_cropped_f[i], theObject@group_a)) {
                    cl_f <- c(cl_f, 0)
                  } else if (is.element(class_labels_vector_cropped_f[i], theObject@group_b)) {
                    cl_f <- c(cl_f, 1)
                  }
                }
                return(cl_f)
              }
              
              class_labels_vector <- get_class_labels_as_vector(theObject@input_data)
              data_matrix <- get_t_df_as_matrix(theObject@input_data)
              
              column_deletion_marks <- NULL
              for(i in 1:length(class_labels_vector)) {
                if(is.element(class_labels_vector[i], theObject@group_a) | is.element(class_labels_vector[i], theObject@group_b)) {
                  column_deletion_marks <- c(column_deletion_marks, "keep")
                } else {
                  column_deletion_marks <- c(column_deletion_marks, "delete")
                }
              }
              
              deletion_mark_indices <- which(column_deletion_marks == "delete")
              
              data_matrix_cropped <- data_matrix
              class_labels_vector_cropped <- class_labels_vector
              
              if(length(deletion_mark_indices > 0)) {
                data_matrix_cropped <- data_matrix[, -deletion_mark_indices]
                
                class_labels_vector_cropped <- class_labels_vector[-deletion_mark_indices]
              }

              cl <- get_numeric_class_labels_vector(class_labels_vector_cropped)
              
              indices_clA <- which(cl == 0)
              indices_clB <- which(cl == 1)
              
              out_list$data <- data_matrix_cropped
              out_list$cl <- cl
              out_list$matrix_group1 <- data_matrix_cropped[, -indices_clB]
              out_list$matrix_group1 <- data_matrix_cropped[, -indices_clA]
              return(out_list)
            }
            
            calculate_rank_product_function <- function(data_matrix_f, cl_f, paired_option_f, calculateP) {
              # _Wrapper function for calling RankProducts function to calculate rank products or rank sums
              
              out_list <- list()
              RP.out <- NULL
              
              input_data_log_ok <- TRUE
              if(!log_transformed_bool) {
                neg_values_count <- length(which(data_matrix_f < 0))
                zero_values_count <- length(which(data_matrix_f == 0))
                if(neg_values_count>0 | zero_values_count>0) {
                  input_data_log_ok <- FALSE
                }
              }
              
              if(input_data_log_ok) {
                if(paired_option_f) {
                  RP.out <- RankProducts(data = data_matrix_f, cl = cl_f, RandomPairs = 1, calculateProduct = calculateP, huge = TRUE, logged = log_transformed_bool)
                } else {
                  RP.out <- RankProducts(data = data_matrix_f, cl = cl_f, calculateProduct = calculateP, huge = TRUE, logged = log_transformed_bool)
                }
                
                pval_group2_vs_group1_unadjusted <- RP.out$pval[,1]
                pval_group1_vs_group2_unadjusted <- RP.out$pval[,2]
                pval_min_unadjusted <- apply(RP.out$pval, 1, min)
                
                pval_group2_vs_group1_adjusted <- p.adjust(pval_group2_vs_group1_unadjusted, theObject@p_adjustment)
                pval_group1_vs_group2_adjusted <- p.adjust(pval_group1_vs_group2_unadjusted, theObject@p_adjustment)
                pval_min_adjusted <- apply(cbind(pval_group2_vs_group1_adjusted, pval_group1_vs_group2_adjusted), 1, min)
                
                pfp1 <- RP.out$pfp
                pfp2 <- RP.out$pfp
                
                out_list$pval_group2_vs_group1_unadjusted <- pval_group2_vs_group1_unadjusted
                out_list$pval_group1_vs_group2_unadjusted <- pval_group1_vs_group2_unadjusted
                out_list$pval_min_unadjusted <- pval_min_unadjusted
                
                out_list$pval_group2_vs_group1_adjusted <- pval_group2_vs_group1_adjusted
                out_list$pval_group1_vs_group2_adjusted <- pval_group1_vs_group2_adjusted
                out_list$pval_min_adjusted <- pval_min_adjusted
                
                out_list$pfp1 <- pfp1
                out_list$pfp2 <- pfp2
                
                
                out_list$errors <- NULL
                
                out_list$rp_full <- RP.out
              } else {
                out_list$errors <- "Input data contains negative values or values equal to 0."
              }
              
              return(out_list)
            }
            
            rp_input <- get_rp_input_data()
            rank_prod_result <- calculate_rank_product_function(rp_input$data, rp_input$cl, theObject@paired_option, calculateP)
            
            r1_f <- get_initialised_results_object(theObject)
            
            if(is.null(rank_prod_result$errors)) {
              r1_f@out_frame <- data.frame(df_get_variable_names(t1_f), rank_prod_result$pval_min_adjusted, rank_prod_result$pval_group1_vs_group2_adjusted, rank_prod_result$pval_group2_vs_group1_adjusted)
              r1_f@unadjusted_out_frame <- data.frame(df_get_variable_names(t1_f), rank_prod_result$pval_min_unadjusted, rank_prod_result$pval_group1_vs_group2_unadjusted, rank_prod_result$pval_group2_vs_group1_unadjusted)
              
              r1_f@full_results_list <- list(rank_prod_result$pval_group1_vs_group2_adjusted, rank_prod_result$pval_group2_vs_group1_adjusted, rank_prod_result$pval_min_adjusted, rank_prod_result$pfp1, rank_prod_result$pfp2, rank_prod_result$rp_full, calculateP)
              r1_f@unadjusted_full_results_list <- list(rank_prod_result$pval_group1_vs_group2_unadjusted, rank_prod_result$pval_group2_vs_group1_unadjusted, rank_prod_result$pval_min_unadjusted, rank_prod_result$pfp1, rank_prod_result$pfp2, rank_prod_result$rp_full, calculateP) 
              
              r1_f@data_row_a <- list(rp_input$matrix_group1) # _Input data of group A
              r1_f@data_row_b <- list(rp_input$matrix_group2) # _Input data of group B
              
              r1_f@test_performed <- TRUE
              r1_f@errors <- list()
              r1_f@error_encountered <- FALSE
            } else {
              r1_f@errors <- list(rank_prod_result$errors)
              r1_f@error_encountered <- TRUE
            }
            
            return(r1_f)
          }
)