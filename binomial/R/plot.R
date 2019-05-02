#' @export
plot.bindis <- function(data){
  library(ggplot2)
  graph <- ggplot(data,aes(x=success,y=probability)) +
  geom_bar(stat="identity") +
  labs(x="successes",y="probability") +
  theme_bw()
  return(graph)
}

#' @export
plot.bincum <- function(data){
  library(ggplot2)
  graph <- ggplot(data,aes(x=data$success,y=data$cumulative)) +
    geom_line() +
    geom_point(shape=21,col="black",fill="white") +
    theme_bw() +
    labs(x="successes",y="probability")
  return(graph)
}
