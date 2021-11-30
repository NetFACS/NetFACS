#' Produce conditional probabilities of dyads of elements, and graph object
#' based on conditional probabilities
#'
#' For all dyadic combinations that appear in the test dataset, this function
#' returns the probability of A occurring (P(A)), the probability of B occurring
#' (P(B)), the probability of A and B occurring simultaneously (P(A+B)), and the
#' probability of A occurring if B is given (P(A|B)). It also creates a graph
#' object that can be plotted
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#' @param min.prob minimum conditional probability that should be shown in the
#'   graph
#' @param min.count minimum number of times that a combination should occur
#'   before being included in the graph
#' @param ignore.element string vector, can be used to exclude certain elements
#'   when creating the plots
#' @param plot.bubbles if TRUE, then the nodes in the network plots will be
#'   surrounded by bubbles; if FALSE, the edges connect the names directly
#'
#' @return Function returns a dataframe that includes all dyadic combinations
#'   and their observed and conditional probabilities
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph geom_edge_link
#' @importFrom ggraph ggraph
#' @importFrom ggraph theme_graph
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 aes
#' @importFrom ggraph circle
#' @importFrom ggplot2 scale_size
#' @importFrom ggplot2 coord_fixed
#' @importFrom utils combn
#' @importFrom tidyr unite
#' @export
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
#' conditional.net <- network.conditional(
#'   netfacs.data = angry.face,
#'   min.prob = 0.01,
#'   min.count = 3,
#'   ignore.element = "25",
#'   plot.bubbles = FALSE
#' )
#'
#' conditional.net$conditional.probalities
network.conditional <- function(netfacs.data,
                                min.prob = 0,
                                min.count = 0,
                                ignore.element = NULL,
                                plot.bubbles = FALSE) {
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  
  # calculate conditional probabilities
  if(is.null(netfacs.data$used.parameters$test.condition)){
    netfacs.data$used.parameters$test.condition <- 'all'
  }
  
  if(is.null(netfacs.data$used.data$condition)){
    netfacs.data$used.data$condition <- 
      rep('all', nrow(netfacs.data$used.data$data))
  }
  
  test.data <-
    netfacs.data$used.data$data[netfacs.data$used.data$condition == netfacs.data$used.parameters$test.condition, ]
  
  
  conditional_foo <- function(indata) {
    # indata is the raw data matrix (either all events or only test condition)
    # pairs of units
    xpairs <- combn(colnames(indata), 2)
    # PA and PB
    pa_and_pb <-
      apply(xpairs, 2, function(x) {
        colSums(indata[, x])
      }) / nrow(indata)
    # PAB
    pab <-
      apply(xpairs, 2, function(x) {
        sum(rowSums(indata[, x]) == 2)
      }) / nrow(indata)
    # PA|B
    pa_b <-
      apply(xpairs, 2, function(x) {
        sum(rowSums(indata[, x]) == 2) / sum(indata[, x[2]])
      })
    # PB|A
    pb_a <-
      apply(xpairs, 2, function(x) {
        sum(rowSums(indata[, x]) == 2) / sum(indata[, x[1]])
      })
    
    # put together
    out <- 
      data.frame(elementA = as.character(xpairs),
                 elementB = as.character(xpairs[2:1, ])) %>% 
      unite("combination", remove = FALSE) %>% 
      mutate(count = rep(pab * nrow(indata), each = 2),
             Probability_A = round(as.numeric(pa_and_pb), 2),
             Probability_B = round(as.numeric(pa_and_pb[2:1, ]), 2),
             Probability_AandB = round(rep(pab, each = 2), 2),
             Probability_AgivenB = round(as.numeric(rbind(pa_b, pb_a)), 2)) %>% 
      filter(.data$count > 0) %>% 
      relocate(.data$combination, .after = .data$elementB) %>% 
      arrange(desc(.data$count), .data$combination)
  }
  
  rs <- conditional_foo(test.data) 
  
  rs.1 <- 
    rs %>% 
    select(.data$elementA, .data$Probability_A) %>% 
    distinct() %>% 
    rename(name = .data$elementA,
           element.prob = .data$Probability_A)
  
  compare.mat <- 
    rs %>% 
    filter(.data$Probability_AgivenB >= min.prob,
           .data$count >= min.count,
           !.data$elementA %in% ignore.element,
           !.data$elementB %in% ignore.element)
  
  descriptive.graph <-
    compare.mat %>% 
    graph_from_data_frame(directed = TRUE, vertices = NULL) %>% 
    as_tbl_graph() %>% 
    activate(nodes) %>% 
    left_join(rs.1, by = "name")
  
  p <- 
    descriptive.graph %>% 
    ggraph(layout = "igraph", algorithm = "kk") +
    geom_edge_link(aes(label = .data$Probability_AgivenB),
                   label_size = 3,
                   arrow = arrow(type = "closed",
                                 angle = 15,
                                 length = unit(3, "mm")),
                   end_cap = circle(4, "mm"),
                   start_cap = circle(4, "mm"),
                   colour = "grey",
                   fontface = 'bold',
                   label_dodge = unit(2, "mm"),
                   angle_calc = "along",
                   show.legend = FALSE) +
    geom_node_text(aes(label = .data$name,
                       size = .data$element.prob,
                       fontface = "bold"),
                   show.legend = FALSE) +
    scale_size(range = c(4, 12)) +
    theme_graph()
  
  
  if (plot.bubbles) {
    p <- 
      p +
      geom_node_point(aes(size = .data$element.prob + 3, 
                          alpha = 0.01),
                      color = "lightblue",
                      show.legend = FALSE) +
      coord_fixed() +
      geom_node_text(aes(label = .data$name,
                         size = .data$element.prob,
                         fontface = "bold"),
                     show.legend = FALSE)
  }
  
  return(
    list(
      conditional.probalities = compare.mat,
      network.graph = descriptive.graph,
      plot = p
    )
  )
}
