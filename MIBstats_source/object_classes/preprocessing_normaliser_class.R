# _This class is used normalisation of data (preprocessing step)

library(preprocessCore)
library(LMGene)

preprocessing_normaliser <- setClass("preprocessing_normaliser",
                                  slots = c(t = "input_data_table", l = "list", old_df = "data.frame", log_transf_done = "logical"),
                                  contains = c("panel_tracker", "preprocessing_shared"),
                                  prototype=list(
                                    t = input_data_table(), l = list(), old_df = data.frame(), log_transf_done = FALSE
                                  )
)

setGeneric(name="initialise_normaliser_if_needed",
           def=function(theObject, pp_scaler1_a)
           {
             standardGeneric("initialise_normaliser_if_needed")
           }
)

setMethod(f="initialise_normaliser_if_needed",
          signature="preprocessing_normaliser",
          definition=function(theObject, pp_scaler1_a)
          {
            # _Gets input from the scaler object, if the previous preprocessing substep performed was scaling
            if(!theObject@l$initialised) {
              theObject <- get_input_from_scaler(theObject, pp_scaler1_a)
              theObject@log_transf_done <- FALSE
              theObject@l$initialised <- TRUE
            }
            return(theObject)
          }
)

setGeneric(name="get_input_from_scaler",
           def=function(theObject, pp_scaler1_a)
           {
             standardGeneric("get_input_from_scaler")
           }
)

setMethod(f="get_input_from_scaler",
          signature="preprocessing_normaliser",
          definition=function(theObject, pp_scaler1_a)
          {
            # _Method for getting input from the scaler object
            if(pp_scaler1_a@l$scaling_choice == "none") {
              theObject@t@df <- pp_scaler1_a@t@df
            } else if (pp_scaler1_a@l$scaling_choice == "unit_variance") {
              theObject@t@df <- pp_scaler1_a@unit_variance_df
            } else if (pp_scaler1_a@l$scaling_choice == "pareto") {
              theObject@t@df <- pp_scaler1_a@pareto_df
            }
            return(theObject)
          }
)

setGeneric(name="perform_quantile_normalisation",
           def=function(theObject)
           {
             standardGeneric("perform_quantile_normalisation")
           }
)

setMethod(f="perform_quantile_normalisation",
          signature="preprocessing_normaliser",
          definition=function(theObject)
          {
            # _Method to perform quantile normalisation of data
            x_matrix <- get_t_df_as_matrix(theObject@t)
            x_matrix_processed <- normalize.quantiles(x_matrix, copy = FALSE)
            theObject <- update_t_df(theObject, x_matrix_processed)
            return(theObject)
          }
)

setGeneric(name="perform_log_transformation",
           def=function(theObject, log_base_f)
           {
             standardGeneric("perform_log_transformation")
           }
)

setMethod(f="perform_log_transformation",
          signature="preprocessing_normaliser",
          definition=function(theObject, log_base_f)
          {
            # _Method for log transforming data
            log_transform <- function(data_in, base_f) {
              # _Log transformation function
              log_data <- log(data_in, base=base_f)
              return(log_data)
            }
            
            if(!theObject@t@log_transformed) {
              x_matrix <- get_t_df_as_matrix(theObject@t)
              number_of_negative_values <- sum(x_matrix < 0)
              
              if(number_of_negative_values > 0) {
                show_generic_error_message(paste("Logarithmic transformation could not be performed because of the ", number_of_negative_values, " negative value(s) found in the data."))
              } else {
                x_matrix_processed <- log_transform(x_matrix, log_base_f)
                theObject <- update_t_df(theObject, x_matrix_processed)
                message_out("Log transformation was applied to the preview data", "Log transformation")
                perform_zero_values_check(x_matrix)
                theObject@log_transf_done <- TRUE
                theObject@t@log_transformed <- TRUE
              }
            } else {
              show_generic_error_message(paste("Logarithmic transformation could not be performed because the input data has been marked as already log transformed"))
            }
            return(theObject)
          }
)

setGeneric(name="perform_glog_transformation",
           def=function(theObject, glog_lambda_f)
           {
             standardGeneric("perform_glog_transformation")
           }
)

setMethod(f="perform_glog_transformation",
          signature="preprocessing_normaliser",
          definition=function(theObject, glog_lambda_f)
          {

            # _Method for glog transforming the data
            if(!theObject@t@log_transformed) {
              x_matrix <- get_t_df_as_matrix(theObject@t)
              number_of_negative_values <- sum(x_matrix < 0)
              
              x_matrix_processed <- glog(x_matrix, lambda = glog_lambda_f)
              theObject <- update_t_df(theObject, x_matrix_processed)
              neg_values_warning <- ""
              if(number_of_negative_values > 0) {
                neg_values_warning <- paste("Warning: ", number_of_negative_values, " negative values were detected in the input data. ", sep="")
              }
              message_out(paste(neg_values_warning, "glog transformation was applied to the preview data", sep=""), "glog transformation")
              perform_zero_values_check(x_matrix)
              theObject@log_transf_done <- TRUE
              theObject@t@log_transformed <- TRUE
            } else {
              show_generic_error_message(paste("Logarithmic transformation could not be performed because the input data has been marked as already log transformed"))
            }
            return(theObject)
          }
)

perform_zero_values_check <- function(x_matrix_f) {
  # _Checks for zero values in log transformation data and displays a warning if they are found
  number_of_zero_values <- sum(x_matrix_f == 0)
  if(number_of_zero_values > 0) {
    message_out(paste("-Inf values were introduced because the input data contained ", number_of_zero_values, " zero values", sep=""), "Logarithmic transformation")
  }
}