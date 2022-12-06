#' Returns all kinds of graph-level network measures for the netfacs network
#'
#' Calculates graph level summary measures from the network object
#'
#' @param netfacs.net igraph network object resulting from
#'   \link{netfacs_network} function
#'
#' @return Function returns a dataframe with the number of elements in the
#'   graph, the number of connected edges, mean strength of connections,
#'   transitivity (mean number of closed triads), diameter (furthest path
#'   between two elements), degree centralization, and mean distance between
#'   elements
#'   
#' @export
#'
#' @examples
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 10,
#'   combination.size = 2
#' )
#'
#' anger.net <- netfacs_network(
#'   netfacs.data = angry.face,
#'   link = "unweighted",
#'   significance = 0.01,
#'   min.count = 1
#' )
#'
#' network_summary_graph(anger.net)
network_summary_graph <- function(netfacs.net) {
  # set result data frame

  net.from.igraph <-
    data.frame(
      nr.elements = length(igraph::vertex.attributes(netfacs.net)$name),
      nr.edges = length(igraph::edge.attributes(netfacs.net)$weight),
      density = igraph::edge_density(igraph::simplify(netfacs.net), loops = FALSE),
      transitivity = igraph::transitivity(igraph::simplify(netfacs.net), type = "global"),
      diameter = igraph::diameter(igraph::simplify(netfacs.net), directed = FALSE),
      degree_centralization = igraph::centr_degree(igraph::simplify(netfacs.net))$centralization,
      mean_distance = igraph::mean_distance(igraph::simplify(netfacs.net))
    )
  
  net.from.igraph[sapply(net.from.igraph, is.numeric)] <-
    lapply(net.from.igraph[sapply(net.from.igraph, is.numeric)], round, 3)
  
  net.from.igraph
}

#' @rdname network_summary_graph
#' @export
network.summary.graph <- function(netfacs.net) {
  network_summary_graph(netfacs.net)
}