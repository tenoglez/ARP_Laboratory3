# euclidean

# dijkstra
dijkstra <- function(graph, init_node) {
  # stop in case graph is not a data frame with numeric columns (v1, v2, w) and init_node is a numeric scalar
  stopifnot(is.data.frame(graph), is.numeric(init_node), colnames(graph) == c("v1", "v2", "w"), sapply(wiki_graph, is.numeric))
  
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