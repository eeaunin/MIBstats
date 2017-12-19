# _This class is used variance stabilisation of data (preprocessing step)

library(vsn)

preprocessing_variance_stabiliser <- setClass("preprocessing_variance_stabiliser",
                                  slots = c(t = "input_data_table", l = "list", old_df = "data.frame"),
                                  contains = c("panel_tracker", "preprocessing_shared"),
                                  prototype=list(
                                    t = input_data_table(), l = list(), old_df = data.frame()
                                  )
)

setGeneric(name="get_input_from_normaliser",
           def=function(theObject, pp_normaliser1_a)
           {
             standardGeneric("get_input_from_normaliser")
           }
)

setMethod(f="get_input_from_normaliser",
          signature="preprocessing_variance_stabiliser",
          definition=function(theObject, pp_normaliser1_a)
          {
            # _Method to get input data from the normaliser object (used in the previous preprocessing substep)
            theObject@t@df <- pp_normaliser1_a@t@df
            return(theObject)
          }
)

setGeneric(name="perform_variance_stabilisation",
           def=function(theObject)
           {
             standardGeneric("perform_variance_stabilisation")
           }
)

setMethod(f="perform_variance_stabilisation",
          signature="preprocessing_variance_stabiliser",
          definition=function(theObject)
          {
            # _Method for performing the variance stabilisation of data
            x_matrix <- get_t_df_as_matrix(theObject@t)
            x_matrix[is.infinite(x_matrix)] <- NA
            x_matrix[is.nan(x_matrix)] <- NA
            
            # _Fitting the data
            fit = vsn2(x_matrix)
            # _Applying the fit
            applied_fit = predict(fit, newdata=x_matrix)
            
            x_matrix_processed <- applied_fit
            
            theObject <- update_t_df(theObject, x_matrix_processed)
            return(theObject)
          }
)
