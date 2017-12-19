# _This class is contains methods that are shared between different preprocessing classes

preprocessing_shared <- setClass("preprocessing_shared")

setGeneric(name="preproc_reformat_input_table",
           def=function(theObject)
           {
             standardGeneric("preproc_reformat_input_table")
           }
)

setMethod(f="preproc_reformat_input_table",
          signature="preprocessing_shared",
          definition=function(theObject)
            # _Reformats input data so that variable names are stored as row names of the data frame instead of being stored in the first column of the dataframe
          {
            data_f <- df_remove_first_col(theObject@t@df)
            
            return(data_f)
          }
)

setGeneric(name="preproc_remove_samples",
           def=function(theObject, samples_to_be_removed_f)
           {
             standardGeneric("preproc_remove_samples")
           }
)

setMethod(f="preproc_remove_samples",
          # _Method for deleting samples from the preprocessing preview data
          signature="preprocessing_shared",
          definition=function(theObject, samples_to_be_removed_f)
          {
            data_f <- preproc_reformat_input_table(theObject)
            
            class_labels_vector_f <- theObject@class_labels_vector
            
            ind <- match(samples_to_be_removed_f, colnames(data_f))
            ind <- na.omit(ind)
            
            if(!is.null(ind) & length(ind)>0) {
              new_df <- data_f[,-ind]
              class_labels_vector_f <- preproc_remove_samples_from_class_labels_vector(theObject, ind)
            } else {
              new_df <- data_f
            }
            
            new_df2 <- cbind(theObject@t@df[,1], as.data.frame(new_df))
            rownames(new_df2) <- rownames(theObject@t@df)
            colnames(new_df2)[1] <- colnames(theObject@t@df)[1]
            
            theObject@class_labels_vector <- class_labels_vector_f
            theObject@t@df <- new_df2
            return(theObject)
          }
)

setGeneric(name="preproc_remove_variables",
           def=function(theObject, variables_to_be_removed_f)
           {
             standardGeneric("preproc_remove_variables")
           }
)

setMethod(f="preproc_remove_variables",
          # _Method for deleting variables from the preprocessing preview data
          signature="preprocessing_shared",
          definition=function(theObject, variables_to_be_removed_f)
          {
            df_f <- data.frame(theObject@t@df, stringsAsFactors = FALSE)
            firstcol <- as.character(df_f[,1])
            
            ind <- match(variables_to_be_removed_f, firstcol)
            ind <- na.omit(ind)
            
            if(!is.null(ind) & length(ind)>0) {
              new_df <- as.data.frame(df_f[-ind, ])
            } else {
              new_df <- as.data.frame(df_f)
            }
            
            return(new_df)
          }
)

setGeneric(name="preproc_initialise_class_labels_vector",
           def=function(theObject, t1_f)
           {
             standardGeneric("preproc_initialise_class_labels_vector")
           }
)

setMethod(f="preproc_initialise_class_labels_vector",
          # _Extracts class labels data from t1_f@class_labels data frame and stores it in a vector
          signature="preprocessing_shared",
          definition=function(theObject, t1_f)
          {
            class_labels_vector_f <- t1_f@class_labels[,2]
            names(class_labels_vector_f) <- t1_f@class_labels[,1]
            theObject@class_labels_vector <- as.character(class_labels_vector_f)
            
            return(theObject)
          }
)

setGeneric(name="preproc_remove_samples_from_class_labels_vector",
           def=function(theObject, sample_indexes_f)
           {
             standardGeneric("preproc_remove_samples_from_class_labels_vector")
           }
)

setMethod(f="preproc_remove_samples_from_class_labels_vector",
          # _Updates the class labels vector when samples are deleted
          signature="preprocessing_shared",
          definition=function(theObject, sample_indexes_f)
          {
            class_labels_vector_f <- theObject@class_labels_vector
            
            
            class_labels_vector_f <- class_labels_vector_f[-sample_indexes_f]
            
            return(class_labels_vector_f)
          }
)

setGeneric(name="preproc_class_labels_vector_to_class_labels_frame",
           def=function(theObject)
           {
             standardGeneric("preproc_class_labels_vector_to_class_labels_frame")
           }
)

setMethod(f="preproc_class_labels_vector_to_class_labels_frame",
          # _Converts class labels vector back to the data frame format that is used to store class labels elsewhere in the app
          signature="preprocessing_shared",
          definition=function(theObject)
          {
            class_labels_vector_f <- theObject@class_labels_vector
            new_class_labels_frame_f <- data.frame(keyName=names(class_labels_vector_f), value=class_labels_vector_f, row.names=NULL)
            colnames(new_class_labels_frame_f) <- c("X", "X")
            return(new_class_labels_frame_f)
          }
)

setGeneric(name="update_t_df",
           def=function(theObject, x_matrix_processed_f)
           {
             standardGeneric("update_t_df")
           }
)

setMethod(f="update_t_df",
          signature="preprocessing_shared",
          definition=function(theObject, x_matrix_processed_f)
          {
            # _Used only in the scaling/normalisation/variance stabilisation part of preprocessing
            
            result_frame <- matrix_to_t1_format(x_matrix_processed_f)
            theObject@old_df <- theObject@t@df
            theObject@t@df <- result_frame
            return(theObject)
          }
)

