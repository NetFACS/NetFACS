#' Create a network based on conditional probabilities of dyads of elements
#'
#' This is a convenience function to create and visualize a network of
#' conditional probabilities for all dyadic element combinations of a
#' \code{\link{netfacs}} object. Conditional probabilities are calculated using
#' the \code{\link{conditional_probabilities}} function.
#'
#'
#' @param netfacs.data object resulting from \code{\link{netfacs}} or
#'   \code{\link{conditional_probabilities}} functions.
#' @param min.prob minimum conditional probability that should be shown in the
#'   graph
#' @param min.count minimum number of times that a combination should occur
#'   before being included in the graph
#' @param ignore.element string vector, can be used to exclude certain elements
#'   when creating the plots
#' @param plot.bubbles if TRUE (default), then the nodes in the network plots
#'   will be surrounded by bubbles; if FALSE, the edges connect the names
#'   directly
#'
#' @return Function returns named list that includes a
#'   \code{\link[tidygraph:tbl_graph]{tbl_graph}} network and a
#'   \code{\link[ggraph:ggraph]{ggraph}} plot.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @seealso \code{\link{netfacs}}, \code{\link{conditional_probabilities}}
#'   
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' conditional.probs <- conditional_probabilities(angry.face)
#'
#' network_conditional(
#'   netfacs.data = conditional.probs,
#'   min.prob = 0.01,
#'   min.count = 3,
#'   ignore.element = "25",
#'   plot.bubbles = FALSE
#' )
#' @export
network_conditional <- function(netfacs.data,
                                min.prob = 0,
                                min.count = 0,
                                ignore.element = NULL,
                                plot.bubbles = TRUE) {
  if (
    isFALSE(
      is.netfacs(netfacs.data) | 
      is.netfacs_multiple(netfacs.data) |
      inherits(netfacs.data, "netfacs_conditional")
    )
  ) {
    stop("'Argument 'netfacs.data' must be of class 'netfacs', 'netfacs_multiple' or 'netfacs_conditional'.")
  }
  UseMethod("network_conditional")
}

#' @export
network_conditional.default <- function(netfacs.data,
                                        min.prob = 0,
                                        min.count = 0,
                                        ignore.element = NULL,
                                        plot.bubbles = TRUE) {
  
  g <- conditional_graph(netfacs.data,
                         min.prob = min.prob,
                         min.count = min.count,
                         ignore.element = ignore.element)
  
  p <- conditional_plot(g, plot.bubbles = plot.bubbles)
  
  return(
    list(
      network = g,
      plot = p
    )
  )
}

#' @export
network_conditional.netfacs <- function(netfacs.data,
                                        min.prob = 0,
                                        min.count = 0,
                                        ignore.element = NULL,
                                        plot.bubbles = TRUE) {
  cp <- conditional_probabilities(netfacs.data)  
  network_conditional.default(cp,
                              min.prob = min.prob,
                              min.count = min.count,
                              ignore.element = ignore.element,
                              plot.bubbles = plot.bubbles)
}

#' @export
network_conditional.netfacs_multiple <- function(netfacs.data,
                                                 min.prob = 0,
                                                 min.count = 0,
                                                 ignore.element = NULL,
                                                 plot.bubbles = TRUE) {
  
  mcp <- conditional_probabilities(netfacs.data)
  
  mcp2 <- 
    split(mcp, mcp$condition) %>% 
    lapply(function(x) x %>% select(-.data$condition))
  
  mcp3 <- 
    lapply(mcp2, function(x) {
      network_conditional.default(x,
                                  min.prob = min.prob,
                                  min.count = min.count,
                                  ignore.element = ignore.element,
                                  plot.bubbles = plot.bubbles)
    })
  
  return(mcp3)
}

#' (Deprecated) Produce conditional probabilities of dyads of elements, and
#' graph object based on conditional probabilities
#'
#' This function is deprecated. Please see \code{\link{network_conditional}}
#' instead
#'
#' @inheritParams network_conditional
#'
#' @return Function returns a dataframe that includes all dyadic combinations
#'   and their observed and conditional probabilities
#'
#' @export
network.conditional <- function(netfacs.data,
                                min.prob = 0,
                                min.count = 0,
                                ignore.element = NULL,
                                plot.bubbles = FALSE) {
  
  .Deprecated("network_conditional")
  
  cp <- conditional_probabilities(netfacs.data)
  ncp <- network_conditional.default(cp,
                                     min.prob = min.prob,
                                     min.count = min.count,
                                     ignore.element = ignore.element,
                                     plot.bubbles = plot.bubbles)
  
  return(
    list(
      conditional.probalities = cp,
      network.graph = ncp$network,
      plot = ncp$plot
    )
  )
  
  # conditional_foo <- function(indata) {
  #   # indata is the raw data matrix (either all events or only test condition)
  #   # pairs of units
  #   xpairs <- combn(colnames(indata), 2)
  #   # PA and PB
  #   pa_and_pb <-
  #     apply(xpairs, 2, function(x) {
  #       colSums(indata[, x])
  #     }) / nrow(indata)
  #   # PAB
  #   pab <-
  #     apply(xpairs, 2, function(x) {
  #       sum(rowSums(indata[, x]) == 2)
  #     }) / nrow(indata)
  #   # PA|B
  #   pa_b <-
  #     apply(xpairs, 2, function(x) {
  #       sum(rowSums(indata[, x]) == 2) / sum(indata[, x[2]])
  #     })
  #   # PB|A
  #   pb_a <-
  #     apply(xpairs, 2, function(x) {
  #       sum(rowSums(indata[, x]) == 2) / sum(indata[, x[1]])
  #     })
  #   
  #   # put together
  #   out <- 
  #     data.frame(elementA = as.character(xpairs),
  #                elementB = as.character(xpairs[2:1, ])) %>% 
  #     unite("combination", remove = FALSE) %>% 
  #     mutate(count = rep(pab * nrow(indata), each = 2),
  #            Probability_A = round(as.numeric(pa_and_pb), 2),
  #            Probability_B = round(as.numeric(pa_and_pb[2:1, ]), 2),
  #            Probability_AandB = round(rep(pab, each = 2), 2),
  #            Probability_AgivenB = round(as.numeric(rbind(pa_b, pb_a)), 2)) %>% 
  #     filter(.data$count > 0) %>% 
  #     relocate(.data$combination, .after = .data$elementB) %>% 
  #     arrange(desc(.data$count), .data$combination)
  # }
}



# helpers -----------------------------------------------------------------

conditional_graph <- function(cp,
                              min.prob = 0,
                              min.count = 0,
                              ignore.element = NULL) {
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  
  rs.1 <- 
    cp %>% 
    dplyr::select(.data$element_A, .data$probability_A) %>% 
    dplyr::distinct() %>% 
    dplyr::rename(name = .data$element_A,
                  element.prob = .data$probability_A)
  
  cp2 <- 
    cp %>% 
    dplyr::filter(.data$probability_AgivenB >= min.prob,
                  .data$count >= min.count,
                  !.data$element_A %in% ignore.element,
                  !.data$element_B %in% ignore.element)
  
  g <-
    cp2 %>% 
    igraph::graph_from_data_frame(directed = TRUE, vertices = NULL) %>% 
    tidygraph::as_tbl_graph() %>% 
    tidygraph::activate(nodes) %>% 
    dplyr::left_join(rs.1, by = "name")
}

conditional_plot <- function(g, 
                             plot.bubbles = TRUE) {
  p <- 
    g %>% 
    ggraph::ggraph(layout = "igraph", algorithm = "kk") +
    ggraph::geom_edge_link(
      ggplot2::aes(label = .data$probability_AgivenB),
      label_size = 3,
      arrow = ggplot2::arrow(type = "closed",
                             angle = 15,
                             length = grid::unit(3, "mm")),
      end_cap = ggraph::circle(4, "mm"),
      start_cap = ggraph::circle(4, "mm"),
      colour = "grey",
      fontface = 'bold',
      label_dodge = grid::unit(2, "mm"),
      angle_calc = "along",
      show.legend = FALSE) +
    ggraph::geom_node_text(
      ggplot2::aes(label = .data$name,
                   size = .data$element.prob,
                   fontface = "bold"),
      show.legend = FALSE) +
    ggplot2::scale_size(range = c(4, 12)) +
    ggraph::theme_graph()
  
  
  if (plot.bubbles) {
    p <- 
      p +
      ggraph::geom_node_point(
        ggplot2::aes(size = .data$element.prob + 3, 
                     alpha = 0.01),
        color = "lightblue",
        show.legend = FALSE) +
      ggplot2::coord_fixed() +
      ggraph::geom_node_text(
        ggplot2::aes(label = .data$name,
                     size = .data$element.prob,
                     fontface = "bold"),
        show.legend = FALSE)
  }
  return(p)
}