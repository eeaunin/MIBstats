# _Methods for the "preprocessing_sorter_class" that deal with the modification of class labels by the user

setGeneric(name="get_samples_dict",
           def=function(theObject)
           {
             standardGeneric("get_samples_dict")
           }
)

setMethod(f="get_samples_dict",
          # _Prepares a vector that contains class labels as elements and sample names as element names
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            samples_dict_f <- as.character(theObject@t@class_labels[,2])
            samples_list <- df_get_sample_names(theObject@t)
            names(samples_dict_f) <- samples_list
            return(samples_dict_f)
          }
)

setGeneric(name="get_class_labels_for_samples",
           def=function(theObject, sample_names)
           {
             standardGeneric("get_class_labels_for_samples")
           }
)

setMethod(f="get_class_labels_for_samples",
          # _Takes sample names as input and gives the matching class label names as output
          signature="preprocessing_sorter",
          definition=function(theObject, sample_names)
          {
            class_labels_f <- NULL
            for(item in sample_names) {
              current_class_label <- theObject@l2$mcl_tracker$samples_dict[[item]]
              class_labels_f <- c(class_labels_f, current_class_label)
            }
            return(class_labels_f)
          }
)

setGeneric(name="initialise_mcl_variables",
           def=function(theObject)
           {
             standardGeneric("initialise_mcl_variables")
           }
)

setMethod(f="initialise_mcl_variables",
          # _Initialises various variables that are used in the class label modification part of preprocessing
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            samples_dict1 <- get_samples_dict(theObject)
            
            theObject@l2$mcl_tracker$samples_dict <- samples_dict1
            
            combined_results_list <- sorter_get_combined_results(theObject)
            combined_results_frame <- combined_results_list$combined_frame
            
            sample_names <- get_sample_names_from_t_df(combined_results_frame)
            
            class_labels <- get_class_labels_for_samples(theObject, sample_names)
            
            
            theObject@l2$mcl_tracker$samples_frame <- as.data.frame(sample_names, stringsAsFactors = FALSE)
            colnames(theObject@l2$mcl_tracker$samples_frame) <- c("Samples")
            
            theObject@l2$mcl_tracker$class_labels_frame <- as.data.frame(class_labels, stringsAsFactors = FALSE)
            colnames(theObject@l2$mcl_tracker$class_labels_frame) <- c("Class labels")
            
            theObject@l2$mcl_tracker$unique_class_labels <- unique(class_labels)
            return(theObject)
          }
)

setGeneric(name="mcl_add_new_class_label",
           def=function(theObject, new_class_label_name)
           {
             standardGeneric("mcl_add_new_class_label")
           }
)

setMethod(f="mcl_add_new_class_label",
          # _Deals with adding new user-defined class labels to the data
          signature="preprocessing_sorter",
          definition=function(theObject, new_class_label_name)
          {
            if(is.null(new_class_label_name)) {
              show_generic_error_message("The name of the new class label cannot be empty")
              return()
            }
            new_class_label_test1 <- gsub("_", "", new_class_label_name)
            new_class_label_test1 <- gsub("\\.", "", new_class_label_test1)
            new_class_label_name_alphanumeric <- gsub("[^[:alnum:] ]", "", new_class_label_name)
            
            if(new_class_label_name_alphanumeric != new_class_label_test1) {
              show_generic_error_message("The name of the new class label can only consist of characters that are alphanumeric, with the exception of dot and underscore")
              return()
            }
            
            if(is.element(new_class_label_name, theObject@l2$mcl_tracker$unique_class_labels)) {
              show_generic_error_message("A class label with the specified name already exists")
              return()
            }
            
            theObject@l2$mcl_tracker$unique_class_labels <- c(new_class_label_name, theObject@l2$mcl_tracker$unique_class_labels)
            return(theObject)
          }
)

setGeneric(name="get_mcl_result",
           def=function(theObject, sample_names)
           {
             standardGeneric("get_mcl_result")
           }
)

setMethod(f="get_mcl_result",
          # _Generates a vector that contains the modified class labels (from the preprocessing step of modifying class labels).
          # _Vector elements are class labels and the element names are sample names
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            mcl_result <- NULL
            
            mcl_result <- theObject@l2$mcl_tracker$class_labels_frame[, 1]
            names(mcl_result) <- theObject@l2$mcl_tracker$samples_frame[, 1]
            
            theObject@l2$mcl_tracker$result <- mcl_result
            
            return(theObject)
          }
)

setGeneric(name="sorter_class_labels_vector_to_class_labels_frame",
           def=function(theObject)
           {
             standardGeneric("sorter_class_labels_vector_to_class_labels_frame")
           }
)

setMethod(f="sorter_class_labels_vector_to_class_labels_frame",
          # _Takes a vector with the class labels that were modified by the user as the input. Converts it to a data frame that is in the format that is used by the input_data_table class
          signature="preprocessing_sorter",
          definition=function(theObject)
          {
            class_labels_vector_f2 <- theObject@l2$mcl_tracker$result
            class_labels_frame_f2 <- class_labels_vector_to_class_labels_frame(class_labels_vector_f2)
            return(class_labels_frame_f2)
          }
)

