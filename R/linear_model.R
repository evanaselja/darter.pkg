#' Creates a linear model.
#' 
#' Use the data to create a linear model that compares x and y directly to each other
#' 
#' @param df Data frame needs to have two numeric values to be compared against
#' @param x The first value to be evaluated against the second variable, must be numeric, must be quotation marks.
#' @param y The second value to be evaluated against the first variable, must be numeric, must be quotation marks.
#' @return line Produces a lineral model of x ~ y.
#' 
#' @export 

linear_model <- function(df, x, y) {
  if (is.numeric(df[[x]]) && is.numeric(df[[y]])) {
    line <- df %>% 
      na.omit() %>% 
      lm(formula = as.formula(paste(y, "~", x)))
    summary(line)
  } else {
    print("X needs to be numeric")
  }
}


# linear_model(mydata, "LL", "SL")