#' Cleans the data of any NAs, deleting entire row where the NAs are
#' 
#' Uses the given data to clean up any NAs that are in the data.
#' 
#' @param data The csv needs to contain NAs for the function to do what it is meant for. 
#' @return clean It will return the the data file without NAs.
#' 
#' @export 

clean_data <- function(data) {
  clean <- data %>%
    na.omit()
if (anyNA(clean) == FALSE) {
    return(clean)
} else {
  print("NA values not dropped!")
}
}
  
#clean_data(surveys)


