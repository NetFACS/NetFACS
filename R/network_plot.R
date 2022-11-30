#' Plots a network object
#'
#' Plots the network created using the \code{\link{netfacs_network}} function;
#' for networks with clear clusterin of elements, clusters can get different
#' colours
#'
#' @param netfacs.graph igraph network object resulting from
#'   \code{\link{netfacs_network}}
#' @param title string of the graph's main title
#' @param clusters if \code{TRUE}, \code{\link[igraph]{cluster_fast_greedy}} is
#'   used to establish possible clusters in the dataset
#' @param plot.bubbles if TRUE, then the nodes in the network plots will be
#'   surrounded by bubbles; if FALSE, the edges connect the names directly
#' @param hide.unconnected if TRUE, then the nodes that do not have any
#'   significant connections will be hidden in the plot
#'
#' @return Function returns a \code{\link[ggraph]{ggraph}} plot of the network,
#'   where the size of nodes indicates how often they occur on their own, and
#'   edges indicate significant co-occurrence between them
#'
#' @importFrom magrittr `%>%`
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
#' network_plot(anger.net,
#'              title = "Angry Faces",
#'              clusters = FALSE,
#'              plot.bubbles = TRUE)

network_plot <- function(netfacs.graph,
                         title = "network",
                         clusters = FALSE,
                         plot.bubbles = FALSE,
                         hide.unconnected = TRUE) {
  g <- netfacs.graph
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  
  if (hide.unconnected) {
    g <- 
      g %>% 
      tidygraph::activate(nodes) %>% 
      dplyr::filter(.data$element.prob != 0 | !is.na(.data$element.prob))
  }
  
  # if 'cluster' is not selected, the graph is plotted in black and white
  if (!clusters) {
    p <- 
      g %>% 
      ggraph::ggraph(layout = "igraph",
                     algorithm = 'kk') +
      ggraph::geom_edge_link(
        ggplot2::aes(label = round(.data$observed.prob, 2)),
        arrow = NULL,
        colour = "grey",
        fontface = 'bold',
        end_cap = ggraph::circle(2, "mm"),
        start_cap = ggraph::circle(2, "mm"),
        label_dodge = grid::unit(2, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(label = .data$name,
                     size = .data$element.prob,
                     fontface = "bold"),
        show.legend = FALSE
      ) +
      ggplot2::scale_size(range = c(4, 12)) +
      ggplot2::ggtitle(title) +
      ggraph::theme_graph(base_family = "sans")
    
    if (plot.bubbles) {
      
      p <- p +
        ggraph::geom_node_point(
          ggplot2::aes(size = .data$element.prob + 3, alpha = 0.01),
          color = "lightblue",
          show.legend = FALSE
        ) +
        ggplot2::coord_fixed() +
        ggraph::geom_node_text(
          ggplot2::aes(label = .data$name,
                       size = .data$element.prob,
                       fontface = "bold"
          ),
          show.legend = FALSE
        )
    }
  }
  
  # if 'clusters' == T, then the fast and greedy algorithm is used to detect clusters and color the nodes accordingly
  if (clusters) {
    net.un <- g
    # other clustering algorithms exist, eg walktrap
    net.community <- igraph::cluster_fast_greedy(net.un) 
    # modularity measure. Above 0.3 is good modularity
    modular <- round(igraph::modularity(net.community), 2) 
    
    net.com <- data.frame(
      name = net.community$names,
      community = net.community$membership
    )
    color <- grDevices::rainbow(length(unique(net.com$community)))
    shape <- sample(rep(15:19, 10))
    
    p <- 
      g %>% 
      ggraph::ggraph(layout = "igraph", 
             algorithm = 'kk') +
      ggraph::geom_edge_link(aes(label = round(.data$observed.prob, 2)),
                     arrow = NULL,
                     colour = "grey",
                     fontface = 'bold',
                     end_cap = ggraph::circle(5, "mm"),
                     start_cap = ggraph::circle(5, "mm"),
                     label_dodge = grid::unit(2, "mm"),
                     angle_calc = "along",
                     show.legend = FALSE) +
      ggraph::geom_node_text(aes(label = .data$name,
                         color = color[net.com$community],
                         size = .data$element.prob,
                         fontface = "bold"),
                     show.legend = FALSE) +
      ggplot2::scale_size(range = c(4, 12)) +
      ggplot2::ggtitle(
        paste(c(title, "; Modularity = ", modular), collapse = "")
      ) +
      ggraph::theme_graph(base_family = "sans")
    
    if (plot.bubbles) {
      p <- p +
        ggraph::geom_node_point(
          ggplot2::aes(size = .data$element.prob + 3, alpha = 0.01),
          color = color[net.com$community],
          shape = shape[net.com$community],
          show.legend = F
        ) +
        ggplot2::coord_fixed() +
        ggraph::geom_node_text(
          ggplot2::aes(label = .data$name,
                       size = .data$element.prob,
                       fontface = "bold"),
          show.legend = FALSE
        )
    }
  }
  return(p)
}

#' @rdname network_plot
#' @export
network.plot <- function(netfacs.graph,
                         title = "network",
                         clusters = FALSE,
                         plot.bubbles = FALSE,
                         hide.unconnected = TRUE) {
  network_plot(netfacs.graph,
               title,
               clusters,
               plot.bubbles,
               hide.unconnected)
}
