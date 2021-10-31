#' Plots networks for multiple conditions
#'
#' The function takes multiple network objects and plots them next to each other while keeping the element positions etc constant. Uses igraph.plot function
#'
#'
#' @param netfacs.graphs list of network objects resulting from netfacs.network() function or multiple.netfacs.networks() function
#'
#' @return Function returns a igraph.plot connections between nodes in the different networks. Elements that are significantly more likely to occur than expected are large, non-significant elements are small, and absent elements are absent.
#' @importFrom grDevices dev.control dev.off pdf recordPlot colors
#' @importFrom igraph layout_in_circle
#' @importFrom igraph %u%
#' @importFrom igraph graph.empty
#' @importFrom igraph add_vertices
#' @importFrom igraph edge.attributes
#' @importFrom igraph vertex.attributes
#' @importFrom igraph V<-
#' @importFrom igraph V
#' @importFrom graphics plot par plot.new
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces <- multiple.netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   duration = NULL,
#'   ran.trials = 10, # only for example
#'   control = NULL,
#'   random.level = NULL,
#'   combination.size = 2
#' )
#'
#' emo.nets <- multiple.netfacs.network(emo.faces, min.count = 5)
#' multiple.network.plot(emo.nets)
multiple.network.plot <- function(netfacs.graphs) {
  all.nodes <- sort(unique(unlist(lapply(netfacs.graphs, function(x) {
    return(V(x)$name)
  }))))

  netfacs.graphs <- lapply(netfacs.graphs, function(x) {
    missing.nodes <- setdiff(all.nodes, V(x)$name)
    x <-
      add_vertices(x, length(missing.nodes), attr = list(name = missing.nodes))
    return(x)
  })

  net <- graph.empty(n = length(all.nodes), directed = F)
  V(net)$name <- all.nodes

  for (i in 1:length(netfacs.graphs)) {
    net <- net %u% netfacs.graphs[[i]]
  }
  l <- layout_in_circle(net, order = V(net))

  pdf(NULL)
  dev.control(displaylist = "enable")
  plot.new()
  oldpar <- par(mfrow = c(2, ceiling(length(netfacs.graphs) / 2)), 
                mar = c(1, 1, 1, 1), bg = "white")
  on.exit(par(oldpar), add = TRUE) # restore user state
  
  for (i in 1:length(netfacs.graphs)) {
    net.i <- graph.empty(n = length(all.nodes), directed = F)
    V(net.i)$name <- all.nodes
    net.i <- net.i %u% netfacs.graphs[[i]]
    node.size <- vertex.attributes(net.i)$element.significance
    node.size[node.size > 0.01] <- 30
    node.size[node.size <= 0.01] <- 50
    node.size[is.na(node.size)] <- 0

    edge.weight <- edge.attributes(net.i)$weight*10
    edge.size <- edge.weight
    # edge.size <- cut(edge.weight, 3)
    # edge.size.char <- as.character(edge.size)
    # edge.size.char[edge.size == levels(edge.size)[1]] <- 1
    # edge.size.char[edge.size == levels(edge.size)[2]] <- 3
    # edge.size.char[edge.size == levels(edge.size)[3]] <- 5
    # edge.size <- as.numeric(edge.size.char)
    if (length(unique(edge.size)) == 1) {
      edge.size <- edge.size / edge.size
    }

    plot(
      net.i,
      vertex.color = colors()[i * 3],
      layout = l,
      main = names(netfacs.graphs)[i],
      edge.width = edge.size,
      vertex.size = node.size,
      vertex.label.size = 5,
      vertex.label.cex = 1.5
    )
  }
  p <- recordPlot()
  invisible(dev.off())

  return(p)
}
