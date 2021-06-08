#' Plots a network object
#'
#' Plots the network created using the \code{\link{netfacs.network}} function;
#' for networks with clear clusterin of elements, clusters can get different
#' colours
#'
#' @param netfacs.graph igraph network object resulting from
#'        \code{\link{netfacs.network}}
#' @param title string of the graph's main title
#' @param clusters if \code{TRUE}, \code{\link[igraph]{cluster_fast_greedy}} is
#'        used to establish possible clusters in the dataset
#' @param plot.bubbles if TRUE, then the nodes in the network plots will be surrounded by bubbles; if FALSE, the edges connect the names directly
#' @param hide_unconnected if TRUE, then the nodes that do not have any significant connections will be hidden in the plot
#'
#' @return Function returns a ggnet plot of the network, where the size of nodes
#' indicates how often they occur on their own, and edges indicate significant
#' co-occurrance between them
#'
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph geom_edge_link
#' @importFrom ggraph theme_graph
#' @importFrom ggraph circle
#' @importFrom ggplot2 scale_size
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 unit
#' @importFrom grDevices rainbow
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' anger.net <- netfacs.network(
#'   netfacs.data = angry.face,
#'   link = "unweighted",
#'   significance = 0.01,
#'   min.count = 1,
#'   min.prob = 0,
#'   min.specificity = 0,
#'   ignore.element = NULL
#' )
#'
#' anger.plot <- network.plot(anger.net,
#'   title = "Angry Faces",
#'   clusters = FALSE,
#'   plot.bubbles = TRUE
#' )
network.plot <- function(netfacs.graph,
                         title = "network",
                         clusters = TRUE,
                         plot.bubbles = FALSE,
                         hide_unconnected = TRUE) {
  net.graph <- netfacs.graph
  if (hide_unconnected == T) {
    unused <- which(
      vertex.attributes(net.graph)$element.prob == 0 |
        is.na(vertex.attributes(net.graph)$element.prob)
    )
    net.graph <- delete_vertices(net.graph, unused)
  }

  # prepare node and edge information
  node.label <-
    vertex.attributes(net.graph)$name # nodes are named as they were in the original network object
  node.size <-
    vertex.attributes(net.graph)$element.prob # size of nodes is determined by their probability to occur
  node.size[is.na(node.size)] <- min(node.size, na.rm = TRUE)

  edge.weight <-
    edge.attributes(net.graph)$weight # weight of edges is determined by their weight attribute
  edge.size <-
    cut(edge.weight, 3) # the line width of the edges is assinged to either weak, medium, strong
  edge.size.char <- as.character(edge.size)
  edge.size.char[edge.size == levels(edge.size)[1]] <- 1
  edge.size.char[edge.size == levels(edge.size)[2]] <- 3
  edge.size.char[edge.size == levels(edge.size)[3]] <- 5
  edge.size <- as.numeric(edge.size.char)
  if (length(unique(edge.size)) == 1) {
    edge.size <- edge.size / edge.size
  }

  # if 'cluster' is not selected, the graph is plotted in black and white
  if (!clusters) {
    p <- ggraph(
      graph = net.graph,
      layout = "igraph",
      algorithm = 'kk'
    ) + # algorithm could be changed, e.g. to 'graphopt'
      geom_edge_link(
        mapping = aes(label = round(.data$observed.prob, 2)),
        # this creates and changes the edges
        arrow = NULL,
        colour = "grey",
        fontface = 'bold',
        end_cap = circle(2, "mm"),
        start_cap = circle(2, "mm"),
        label_dodge = unit(2, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_node_text(
        mapping = aes(
          label = .data$name,
          # this creates and changes the nodes
          size = node.size,
          fontface = "bold"
        ),
        show.legend = FALSE
      ) +
      scale_size(range = c(4, 12)) +
      ggtitle(title) +
      theme_graph(base_family = "sans")

    if (plot.bubbles == T) {
      p <- p +
        geom_node_point(
          mapping = aes(size = node.size + 3, alpha = 0.01),
          color = "lightblue",
          show.legend = F
        ) +
        coord_fixed() +
        geom_node_text(
          mapping = aes(
            label = .data$name,
            # this creates and changes the nodes
            size = node.size,
            fontface = "bold"
          ),
          show.legend = FALSE
        )
    }
  }

  # if 'clusters' == T, then the fast and greedy algorithm is used to detect clusters and color the nodes accordingly
  if (clusters) {
    net.un <- net.graph
    net.community <-
      cluster_fast_greedy(net.un) # other clustering algorithms exist, eg walktrap
    modular <-
      round(modularity(net.community), 2) # modularity measure. Above 0.3 is good modularity
    net.com <- data.frame(
      element = net.community$names,
      community = net.community$membership
    )
    color <- rainbow(length(unique(net.com$community)))
    shape <- sample(rep(15:19, 10))

    p <- ggraph(
      graph = net.graph,
      # see above
      layout = "igraph",
      algorithm = 'kk'
    ) +
      geom_edge_link(
        mapping = aes(label = round(.data$observed.prob, 2)),
        arrow = NULL,
        colour = "grey",
        fontface = 'bold',
        end_cap = circle(5, "mm"),
        start_cap = circle(5, "mm"),
        label_dodge = unit(2, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_node_text(
        mapping = aes(
          label = .data$name,
          color = color[net.com$community],
          size = node.size,
          fontface = "bold"
        ),
        show.legend = FALSE
      ) +
      scale_size(range = c(4, 12)) +
      ggtitle(paste(c(title, "; Modularity = ", modular), collapse = "")) +
      theme_graph(base_family = "sans")

    if (plot.bubbles == T) {
      p <- p +
        geom_node_point(
          mapping = aes(size = node.size + 3, alpha = 0.01),
          color = color[net.com$community],
          shape = shape[net.com$community],
          show.legend = F
        ) +
        coord_fixed() +
        geom_node_text(
          mapping = aes(
            label = .data$name,
            size = node.size,
            fontface = "bold"
          ),
          show.legend = FALSE
        )
    }
  }
  return(p)
}
