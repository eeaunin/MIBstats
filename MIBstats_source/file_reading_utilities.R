read_csv_file <- function(input_file_path, header_boolean){
  # _Function to read csv files with error trapping
  
  out <- NULL
  if(file.exists(input_file_path)){
    
    mtry <- try(read.csv(file = input_file_path, header = header_boolean, sep = ","), 
                silent <- TRUE)
    
    if (class(mtry) != "try-error") {
      out <- read.csv(file = input_file_path, header = header_boolean, sep = ",")
    } else {
      out <- NULL
    }
  } else {
    out <- NULL
  }
  
  # _Returns empty output if the loading of the file did not work
  return(out)
}

read_plain_text_file <- function(input_file_path){
  # _A function to read plain text from files for outputting in text fields with error trapping
  out <- NULL
  if(file.exists(input_file_path)){
    mtry <- try(readLines(input_file_path), 
                silent <- TRUE)
    
    if (class(mtry) != "try-error") {
      out <- readLines(input_file_path)
    } else {
      out <- ("Error: file could not be loaded")
      # _The error message will appear in the text field of the user interface
    }
  } else {
    out <- ("Error: file not found")
    # _The error message will appear in the text field of the user interface
  }
  return(out)
}

file_is_properly_loaded_to_filebrowser <- function(file_f, extensions_f) {
  # _Checks if a file has been loaded into the filebrowser and if the file has a correct type of extension
  # _The correct file extensions are as a list in extensions_f
  # _If extensions_f is an empty list, it means that any extension is OK
  file_is_loaded_f <- FALSE
  extension_is_ok_f <- FALSE
  conclusion_f <- FALSE
  if (is.null(file_f) || is.na(file_f)) {
    return(FALSE)
  } else {
    file_is_loaded_f <- TRUE
    if(length(extensions_f)>0) {
      file_extension <- tools::file_ext(file_f)[[1]]
      if(is.element(file_extension, extensions_f)) {
        extension_is_ok_f <- TRUE
      } else {
        extensions_string <- ""
        counter_f <- 1
        for(extension_f in extensions_f) {
          extensions_string <- paste(extensions_string, ".", extension_f, sep="")
          if(counter_f < length(extensions_f)) {
            extensions_string <- paste(extensions_string, ", ", sep="")
          }
          counter_f <- counter_f + 1
        }
        showModal(modalDialog(
          title = "Error", paste("The file selected for loading does not have the correct extension (", extensions_string, ")", sep="")
        ))
      }
    } else {
      extension_is_ok_f <- TRUE
    }
    
  }
  if(file_is_loaded_f && extension_is_ok_f) {
    conclusion_f <- TRUE
  }
  return(conclusion_f)
}