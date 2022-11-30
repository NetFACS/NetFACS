#' Plots networks for multiple conditions
#'
#' The function takes multiple network objects and plots them next to each other
#' while keeping the element positions etc constant. Uses \code{\link{ggraph}}
#' function
#'
#' @param netfacs.graphs List of network objects resulting from
#'   \code{\link{netfacs_multiple}} function or
#'   \code{\link{multiple_netfacs_network}} function
#' @param sig.level Numeric between 0 and 1. P value used to determine whether
#'   nodes are significant. Default = 0.01.
#' @param sig.nodes.only Logical. Should only nodes that were significant in _at
#'   least_ one of the networks be included in the plots? Default = FALSE.
#'
#' @return Function returns a \code{\link{ggraph}} plot showing connections
#'   between nodes in the different networks. Elements that are significantly
#'   more likely to occur than expected are large, non-significant elements are
#'   small, and absent elements are absent.
#'   
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces <- netfacs_multiple(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   duration = NULL,
#'   ran.trials = 10, # only for example
#'   control = NULL,
#'   random.level = NULL,
#'   combination.size = 2
#' )
#'
#' emo.nets <- multiple_netfacs_network(emo.faces, min.count = 5)
#' multiple_network_plot(emo.nets)
multiple_network_plot <- function(netfacs.graphs,
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
          tidygraph::activate(nodes) %>% 
          dplyr::filter(.data$element.significance <= sig.level)
      })
    
    plot.nodes <- sort(unique(unlist(lapply(netfacs.graphs, function(x) {
      return(igraph::V(x)$name)
    }))))
    
    netfacs.graphs <-
      netfacs.graphs %>%
      lapply(function(x){
        x %>%
          tidygraph::activate(nodes) %>% 
          dplyr::full_join(data.frame(name = plot.nodes), by = "name")
      })
  } else {
    plot.nodes <- sort(unique(unlist(lapply(netfacs.graphs, function(x) {
      return(igraph::V(x)$name)
    }))))
  }
  
  netfacs.graphs <- 
    netfacs.graphs %>% 
    lapply(function(x){
      x %>% 
        tidygraph::activate(nodes) %>% 
        dplyr::mutate(
          node.size = dplyr::case_when(
            .data$element.significance  > sig.level ~ 50,
            .data$element.significance <= sig.level ~ 150,
            is.na(.data$element.significance) ~ 0
          )
        ) %>% 
        tidygraph::activate(edges) %>% 
        dplyr::mutate(edge.weight = .data$observed.prob * 3,
                      edge.size = .data$edge.weight)
    })
  
  plot_graphs <- function(g, node.order, node.color, .title) {
    g %>%
      ggraph::ggraph(layout = "circle",
                     order = node.order) +
      ggraph::geom_edge_link(
        ggplot2::aes(edge_width = .data$edge.size),
        color = "lightgrey",
        show.legend = FALSE
      ) +
      ggraph::geom_node_point(
        ggplot2::aes(size = .data$node.size),
        color = node.color,
        show.legend = FALSE
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(label = .data$name,
                     size = 20),
        color = "black",
        show.legend = FALSE
      ) +
      ggplot2::ggtitle(.title) +
      ggraph::theme_graph(base_family = "sans") +
      ggplot2::theme(
        plot.margin = grid::unit(c(2, 2, 2, 2), "mm"),
        plot.title = ggplot2::element_text(size = 12)
      )
  }
  
  plot.titles <- names(netfacs.graphs)
  p.list <- vector(mode = "list", length = length(netfacs.graphs))
  for (i in 1:length(netfacs.graphs)) {
    
    p.list[[i]] <- 
      plot_graphs(netfacs.graphs[[i]], 
                  node.order = plot.nodes, 
                  node.color = grDevices::colors()[i * 3], 
                  .title = plot.titles[i])
  }
  
  p <- patchwork::wrap_plots(p.list)
  
  return(p)
}

#' @rdname multiple_network_plot
#' @export
multiple.network.plot <- function(netfacs.graphs,
                                  sig.level = 0.01,
                                  sig.nodes.only = FALSE) {
  multiple_network_plot(netfacs.graphs, sig.level, sig.nodes.only)
}
