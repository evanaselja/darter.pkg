#' Filters out any sex in a data set.
#' 
#' Filters out any sexes, such as male, female, or others by inputting the data name, the variable that the sex is called, and which sex to filter out.
#' 
#' @param data Needs to be a data set that contains sexes in the form of strings.
#' @param  s_vars The variable that the list of sexes is connect to. Must be placed in quotation marks.
#' @param s The sex that is to be filtered out. Must be placed in quotation marks.
#' @return filt The complete data set without the sex that has been filtered out.
#' 
#' @export 

filter_sexes <- function(data, s_vars, s) {
    filt <- data %>% 
      filter(data[[s_vars]] != {{s}}) 
    if (any(data[[s_vars]] != {{s}})) {
          return(filt) 
      } else {
        print("Sex not filtered.")
      }
}

#filter_sexes(mydata, "Sex", "m")

