#'Plots a jitter plot using any data that you put into the function.
#' 
#' Uses a data table to make a scatter plot that uses the required x and y that is supplied. Made to compare two different variables.
#' 
#' @param data Data must be in the form of a data frame, needs to have at least one numeric value.
#' @param x The variable graphed on the x-axis of the graph, needs to be in quotation marks.
#' @param y The variable graphed on the y-axis of the graph, needs to be in quotation marks, must be numeric.
#' @return plot Returns a scatter plot that graphs the x and y, with hot pink scatter plots.
#' 
#' @export 

plot_data <- function(data, x, y) {
  if (is.numeric(data[[y]])) { 
    plot <- data %>% 
      ggplot(mapping = aes({{x}}, {{y}})) + geom_jitter(color = "hotpink")
    return(plot)
  } else {
  print("lol")
}
}



