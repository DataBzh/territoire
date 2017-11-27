get.graph <- function(graph.df) {
  dg <- graph.data.frame(
    graph.df$edges,
    vertices = graph.df$vertices,
    directed = F
  )
  
  return(dg)
}
