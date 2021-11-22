#' Plots networks for multiple conditions
#'
#' The function takes multiple network objects and plots them next to each other
#' while keeping the element positions etc constant. Uses \code{\link{ggraph}}
#' function
#'
#' @param netfacs.graphs list of network objects resulting from
#'   \code{\link{netfacs.network}} function or
#'   \code{\link{multiple.netfacs.networks}} function
#'
#' @return Function returns a \code{\link{ggraph}} plot showing connections
#'   between nodes in the different networks. Elements that are significantly
#'   more likely to occur than expected are large, non-significant elements are
#'   small, and absent elements are absent.
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
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
multiple.network.plot <- function(netfacs.graphs) {
  all.nodes <- sort(unique(unlist(lapply(netfacs.graphs, function(x) {
    return(V(x)$name)
  }))))
  
  netfacs.graphs <- 
    netfacs.graphs %>% 
    lapply(function(x){
      x %>% 
        activate(nodes) %>% 
        mutate(node.size = case_when(element.significance  > 0.01 ~ 50,
                                     element.significance <= 0.01 ~ 150,
                                     is.na(element.significance) ~ 0)) %>% 
        activate(edges) %>% 
        mutate(edge.weight = observed.prob * 3,
               edge.size = edge.weight)
    })
  
  plot_graphs <- function(g, node.order, node.color, .title) {
    g %>%
      ggraph(layout = "circle",
             order = all.nodes) +
      geom_edge_link(aes(edge_width = edge.size),
                     color = "lightgrey",
                     show.legend = FALSE) +
      geom_node_point(aes(size = node.size),
                      color = node.color,
                      show.legend = FALSE) +
      geom_node_text(aes(label = name,
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
                  node.order = all.nodes, 
                  node.color = colors()[i * 3], 
                  .title = plot.titles[i])
  }
  
  p <- patchwork::wrap_plots(p.list)
  
  return(p)
}
