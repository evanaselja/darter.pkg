#' Summarizes the values of the mean of any variable of the data set, and groups it by any variable.
#' 
#' Uses the inputted data and variable to group by the chosen variable to give a summation of the mean for each categorical variable from the numerical variable inputted.
#' 
#' @param data Must be in a data set that contains numerical data that can be mutated.
#' @param vars The variable that is being summarized via finding the mean. Must be numerical.
#' @param s_var The variable that the summarized data is being grouped by. Must be categorical.
#' @return mutation Returns the data set with just the selected categorical variable and the means of the selected numeric variable.
#' 
#' @export 

summarize_value <- function(data, vars, s_var) {
  if (is.numeric(data[[vars]])) {
    mut <- data %>% 
      group_by({{s_var}}) %>% 
      na.omit() %>% 
      summarize(mean_var = mean({{vars}}, na.rm = TRUE))
  return(mut)
  }    else {
    print("no")
  }
}
