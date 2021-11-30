#' Plots networks for multiple conditions
#'
#' The function takes multiple network objects and plots them next to each other
#' while keeping the element positions etc constant. Uses \code{\link{ggraph}}
#' function
#'
#' @param netfacs.graphs List of network objects resulting from
#'   \code{\link{netfacs.network}} function or
#'   \code{\link{multiple.netfacs.network}} function
#' @param sig.level Numeric between 0 and 1. P value used to determine whether
#'   nodes are significant. Default = 0.01.
#' @param sig.nodes.only Logical. Should only nodes that were significant in _at
#'   least_ one of the networks be included in the plots? Default = FALSE.
#'
#' @return Function returns a \code{\link{ggraph}} plot showing connections
#'   between nodes in the different networks. Elements that are significantly
#'   more likely to occur than expected are large, non-significant elements are
#'   small, and absent elements are absent.
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr full_join
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 theme
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph geom_edge_link
#' @importFrom ggraph ggraph
#' @importFrom ggraph theme_graph
#' @importFrom grDevices colors
#' @importFrom igraph V
#' @importFrom magrittr %>%
#' @importFrom tidygraph activate
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
multiple.network.plot <- function(netfacs.graphs,
                                  sig.level = 0.01,
                                  sig.nodes.only = FALSE) {
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  edges <- NULL # to avoid R CMD check when calling activate(edges)
  
  
  if (sig.level > 1 | sig.level < 0 | !is.numeric(sig.level)) {
    stop("sig.level must be a number between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(sig.nodes.only)) {
    stop("sig.nodes.only must be a logical: TRUE or FALSE.", call. = FALSE)
  }
  
  if (sig.nodes.only) {
    netfacs.graphs <-
      netfacs.graphs %>%
      lapply(function(x){
        x %>%
          activate(nodes) %>% 
          filter(.data$element.significance <= sig.level)
      })
    
    plot.nodes <- sort(unique(unlist(lapply(netfacs.graphs, function(x) {
      return(V(x)$name)
    }))))
    
    netfacs.graphs <-
      netfacs.graphs %>%
      lapply(function(x){
        x %>%
          activate(nodes) %>% 
          full_join(data.frame(name = plot.nodes), by = "name")
      })
  } else {
    plot.nodes <- sort(unique(unlist(lapply(netfacs.graphs, function(x) {
      return(V(x)$name)
    }))))
  }
  
  netfacs.graphs <- 
    netfacs.graphs %>% 
    lapply(function(x){
      x %>% 
        activate(nodes) %>% 
        mutate(
          node.size = case_when(.data$element.significance  > sig.level ~ 50,
                                .data$element.significance <= sig.level ~ 150,
                                is.na(.data$element.significance) ~ 0)) %>% 
        activate(edges) %>% 
        mutate(edge.weight = .data$observed.prob * 3,
               edge.size = .data$edge.weight)
    })
  
  plot_graphs <- function(g, node.order, node.color, .title) {
    g %>%
      ggraph(layout = "circle",
             order = node.order) +
      geom_edge_link(aes(edge_width = .data$edge.size),
                     color = "lightgrey",
                     show.legend = FALSE) +
      geom_node_point(aes(size = .data$node.size),
                      color = node.color,
                      show.legend = FALSE) +
      geom_node_text(aes(label = .data$name,
                         size = 20),
                     color = "black",
                     show.legend = FALSE) +
      ggtitle(.title) +
      theme_graph(base_family = "sans") +
      theme(plot.margin = unit(c(2, 2, 2, 2), "mm"),
            plot.title = element_text(size = 12))
  }
  
  plot.titles <- names(netfacs.graphs)
  p.list <- vector(mode = "list", length = length(netfacs.graphs))
  for (i in 1:length(netfacs.graphs)) {
    
    p.list[[i]] <- 
      plot_graphs(netfacs.graphs[[i]], 
                  node.order = plot.nodes, 
                  node.color = colors()[i * 3], 
                  .title = plot.titles[i])
  }
  
  p <- patchwork::wrap_plots(p.list)
  
  return(p)
}
