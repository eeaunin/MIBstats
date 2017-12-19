# _Functions for loading help files that are used by help modules in the app

get_html_header <- function() {
  html_header_file <- file("help_files/html_header.txt", open="r")
  html_header <- readLines(html_header_file)
  close(html_header_file)
  
  html_header_string_f <- ""
  for(item in html_header) {
    html_header_string_f <- paste(html_header_string_f, item, "\n", sep="")
  }
  return(html_header_string_f)
}

load_help_html <- function() {
  help_file <- file("help_files/help.0html", open="r")
  in_html_f <- readLines(help_file)
  close(help_file)
  return(in_html_f)
}