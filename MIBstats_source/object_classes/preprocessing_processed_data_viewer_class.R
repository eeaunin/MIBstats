# _This class is used for viewing preprocessed data (after preprocessing)

preprocessing_processed_data_viewer <- setClass("preprocessing_processed_data_viewer",
                              slots = c(t = "input_data_table", l = "list"),
                              contains = c("panel_tracker", "preprocessing_shared"),
                              prototype=list(
                                t = input_data_table(), l = list()
                              )
)

