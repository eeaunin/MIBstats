# _This class is used scaling of data (preprocessing step)

preprocessing_scaler <- setClass("preprocessing_scaler",
                                 slots = c(t = "input_data_table", l = "list", old_df = "data.frame", pareto_df = "data.frame", unit_variance_df = "data.frame"),
                                 contains = c("panel_tracker", "preprocessing_shared"),
                                 prototype=list(
                                   t = input_data_table(), l = list(), old_df = data.frame(), pareto_df = data.frame(), unit_variance_df = data.frame()
                                 )
)

setGeneric(name="get_output_df",
           def=function(theObject)
           {
             standardGeneric("get_output_df")
           }
)

setMethod(f="get_output_df",
          signature="preprocessing_scaler",
          definition=function(theObject)
          {
            # _Gets the output data frame of this object. The output depends on whether pareto scaling, unit variance scaling or no scaling was used.
            if(theObject@l$scaling_choice == "none") {
              output_df <- theObject@t@df
            } else if(theObject@l$scaling_choice == "unit_variance") {
              output_df <- theObject@unit_variance_df
            } else if(theObject@l$scaling_choice == "pareto") {
              output_df <- theObject@pareto_df
            }
            return(output_df)
          }
)

setGeneric(name="perform_mean_centering",
           def=function(theObject)
           {
             standardGeneric("perform_mean_centering")
           }
)

setMethod(f="perform_mean_centering",
          signature="preprocessing_scaler",
          definition=function(theObject)
          {
            # _Method for performing mean centering of the data
            x_matrix <- get_t_df_as_matrix(theObject@t)
            
            center_apply <- function(x) {
              # _Mean centering function for a numeric matrix
              t_centered_data <- apply(x, 1, function(y) y - mean(y))
              centered_data <- t(t_centered_data)
              return(centered_data)
            }
            
            x_sc <- center_apply(x_matrix)

            result_frame <- matrix_to_t1_format(x_sc)

            theObject@old_df <- theObject@t@df
            theObject@t@df <- result_frame

            return(theObject)
          }
)

setGeneric(name="perform_scaling",
           def=function(theObject, type_f)
           {
             standardGeneric("perform_scaling")
           }
)

setMethod(f="perform_scaling",
          signature="preprocessing_scaler",
          definition=function(theObject, type_f)
          {
            # _Method for scaling the data
            
            pareto_scaling <- function(x_matrix_f) {
              # _Pareto scaling for matrix which has samples in columns and variables in rows. The scaling is done by rows. Has to be preceded by mean centering the data
              t_x_sc <- apply(x_matrix_f, 2, function(x) x/sqrt(sd(x, na.rm=T)))
              
              x_sc <- t(t_x_sc)
              return(t_x_sc)
            }
            
            unit_variance_scaling <- function(x_matrix_f) {
              # _Unit variance scaling for matrix which has samples in columns and variables in rows. The scaling is done by rows. The function does not include centering of the data, which should be done as a separate step, preceding this one.
              
              
              x_sc <- t(scale(t(x_matrix_f), center = FALSE, scale = TRUE))
              return(x_sc)
            }
            
            x_matrix <- get_t_df_as_matrix(theObject@t)
            
            scaled_data <- NULL

            if(type_f=="pareto") {
              scaled_data <- pareto_scaling(x_matrix)
            } else if(type_f=="unit_variance") {
              scaled_data <- unit_variance_scaling(x_matrix)
            }
            
            result_frame <- matrix_to_t1_format(scaled_data)
            
            theObject@old_df <- theObject@t@df
            
            if(type_f=="pareto") {
              theObject@pareto_df <- result_frame
            } else if(type_f=="unit_variance") {
              theObject@unit_variance_df <- result_frame
            }
            return(theObject)
          }
)