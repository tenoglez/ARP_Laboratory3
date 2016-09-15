#' Dijkstra algorithm
#' @export
#' 
#' @description 
#' Algorithm that takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph.
#' 
#' 
#' @param graph A \code{data.frame} with three columns that describes the graph: \code{v1} (nodes where edges start), \code{v2} (nodes where edges end) and \code{w} (weigth of the edges)
#' @param init_node A positive scalar number for the initial node
#' 
#' @return \code{vector} with the shortest path from the initial node to every other node
#' 
#' @references 
#'Dijkstra algorithm - \url{https://en.wikipedia.org/wiki/Dijkstras_algorithm}
#' 
#' @examples
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' 
#' dijkstra(wiki_graph, 3)
#' 
dijkstra <- function(graph, init_node) {
  # stop in case graph is not a data frame with numeric columns (v1, v2, w) and init_node is a numeric scalar
  stopifnot(is.data.frame(graph), sapply(wiki_graph, length) > 0, is.numeric(init_node), colnames(graph) == c("v1", "v2", "w"), sapply(wiki_graph, is.numeric))
  
  # vector Q with all the vertex
  Q <- unique(c(graph$v1, graph$v2))
  # vector dist with the distances from the initial node to each vertex
  dist <- rep(Inf, length(Q))
  # set distance 0 for the initial node
  dist[init_node] <- 0

  # until Q is empty
  while(length(Q) != 0) {
    # vertex in Q with minimun distance value
    u <- Q[which(Q == which(dist == min(dist[Q])))]
    # index of the value u in the distance vector
    u_dist_index <- which(dist == min(dist[Q]))
    
    # remove u from Q
    Q <- Q[-which(Q == u)]
    # neighbors of u indices in neighbors column v1
    neighbors_u <- which(graph$v1 == u)

    # for each neighbor of u, 'v'
    for(v in neighbors_u) {
      # neighbor v should be in Q
      if (graph$v2[v] %in% Q) {
        # total distance from the initial node to the neighbor is the accumulated distance + distance from u to v
        alt <- dist[u_dist_index] + graph$w[v]
        # if the new distance is shorter than the previous path, the path is updated
        if (alt < dist[graph$v2[v]]) {
          dist[graph$v2[v]] <- alt
        }
      }
    }
  }
  return(dist)
}