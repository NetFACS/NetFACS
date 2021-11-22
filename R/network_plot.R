#' Plots a network object
#'
#' Plots the network created using the \code{\link{netfacs.network}} function;
#' for networks with clear clusterin of elements, clusters can get different
#' colours
#'
#' @param netfacs.graph igraph network object resulting from
#'   \code{\link{netfacs.network}}
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
#' @importFrom dplyr filter
#' @importFrom igraph cluster_fast_greedy
#' @importFrom ggplot2 scale_size
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 unit
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph geom_edge_link
#' @importFrom ggraph theme_graph
#' @importFrom ggraph circle
#' @importFrom grDevices rainbow
#' @importFrom tidygraph activate
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
                         hide.unconnected = TRUE) {
  g <- netfacs.graph
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  
  if (hide.unconnected) {
    g <- 
      g %>% 
      activate(nodes) %>% 
      filter(.data$element.prob != 0 | !is.na(.data$element.prob))
  }
  
  # if 'cluster' is not selected, the graph is plotted in black and white
  if (!clusters) {
    p <- 
      g %>% 
      ggraph(layout = "igraph",
             algorithm = 'kk') +
      geom_edge_link(
        aes(label = round(.data$observed.prob, 2)),
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
        aes(label = .data$name,
            size = .data$element.prob,
            fontface = "bold"),
        show.legend = FALSE
      ) +
      scale_size(range = c(4, 12)) +
      ggtitle(title) +
      theme_graph(base_family = "sans")
    
    if (plot.bubbles) {
      
      p <- p +
        geom_node_point(
          aes(size = .data$element.prob + 3, alpha = 0.01),
          color = "lightblue",
          show.legend = FALSE
        ) +
        coord_fixed() +
        geom_node_text(
          aes(label = .data$name,
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
    net.community <- cluster_fast_greedy(net.un) 
    # modularity measure. Above 0.3 is good modularity
    modular <- round(modularity(net.community), 2) 
    
    net.com <- data.frame(
      name = net.community$names,
      community = net.community$membership
    )
    color <- rainbow(length(unique(net.com$community)))
    shape <- sample(rep(15:19, 10))
    
    p <- 
      g %>% 
      ggraph(layout = "igraph", 
             algorithm = 'kk') +
      geom_edge_link(aes(label = round(.data$observed.prob, 2)),
                     arrow = NULL,
                     colour = "grey",
                     fontface = 'bold',
                     end_cap = circle(5, "mm"),
                     start_cap = circle(5, "mm"),
                     label_dodge = unit(2, "mm"),
                     angle_calc = "along",
                     show.legend = FALSE) +
      geom_node_text(aes(label = .data$name,
                         color = color[net.com$community],
                         size = .data$element.prob,
                         fontface = "bold"),
                     show.legend = FALSE) +
      scale_size(range = c(4, 12)) +
      ggtitle(paste(c(title, "; Modularity = ", modular), collapse = "")) +
      theme_graph(base_family = "sans")
    
    if (plot.bubbles) {
      p <- p +
        geom_node_point(aes(size = .data$element.prob + 3, alpha = 0.01),
                        color = color[net.com$community],
                        shape = shape[net.com$community],
                        show.legend = F) +
        coord_fixed() +
        geom_node_text(aes(label = .data$name,
                           size = .data$element.prob,
                           fontface = "bold"),
                       show.legend = FALSE)
    }
  }
  return(p)
}
