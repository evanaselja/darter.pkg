#' Creates a bar graph from any data that is put into the function.
#' 
#' Use the data from a data file to create a bar graph that represents the data and the counts of that data.
#' 
#' @param data The csv being graphed, needs to be in a data frame or data table 
#' @param x The variable that will be on the x-axis of the graph, the bar graph counts the amount of this variable. Must put in quotation marks.
#' @return plot A bar graph that shows the number of counts of the x-axis. 
#' 
#' @export 

bar_graph <- function(data, x) {
  if (is.numeric(data[[x]])) {
  plot <- data %>%
    na.omit() %>% 
    ggplot(mapping = aes(x = .data[[x]])) + geom_bar()
  return(plot)
} else {
  print("Please put in numeric values for x and try again.")
}
}

#bar_graph(surveys, "weight")
