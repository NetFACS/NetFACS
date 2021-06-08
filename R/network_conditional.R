#' Produce conditional probabilities of dyads of elements, and graph object based on conditional probabilities
#'
#' For all dyadic combinations that appear in the test dataset, this function returns the probability of A occurring (P(A)), the probability of B occurring (P(B)), the probability of A and B occurring simultaneously (P(A+B)), and the probability of A occurring if B is given (P(A|B)). It also creates a graph object that can be plotted
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#' @param min.prob minimum conditional probability that should be shown in the graph
#' @param min.count minimum number of times that a combination should occur before being included in the graph
#' @param ignore.element string vector, can be used to exclude certain elements when creating the plots
#' @param plot.bubbles if TRUE, then the nodes in the network plots will be surrounded by bubbles; if FALSE, the edges connect the names directly
#'
#' @return Function returns a dataframe that includes all dyadic combinations and their observed and conditional probabilities
#' @importFrom igraph delete_vertices
#' @importFrom igraph add_vertices
#' @importFrom igraph vertex.attributes
#' @importFrom igraph edge.attributes
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph degree
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
#' @importFrom rlang .data
#' @importFrom utils combn
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
  # extract conditional probabilities from arules object in netfacs object
  if(is.null(netfacs.data$used.parameters$test.condition)){netfacs.data$used.parameters$test.condition = 'all'}
  if(is.null(netfacs.data$used.data$condition)){netfacs.data$used.data$condition = rep('all', nrow(netfacs.data$used.data$data))}
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
    out <- data.frame(
      elementA = as.character(xpairs),
      elementB = as.character(xpairs[2:1, ])
    )
    out$combination <-
      rep(apply(xpairs, 2, paste, collapse = "_"), each = 2)
    out$count <- rep(pab * nrow(indata), each = 2)
    out$Probability_A <- round(as.numeric(pa_and_pb), 2)
    out$Probability_B <- round(as.numeric(pa_and_pb[2:1, ]), 2)
    out$Probability_AandB <- round(rep(pab, each = 2), 2)
    out$Probability_AgivenB <-
      round(as.numeric(rbind(pa_b, pb_a)), 2)

    out <- out[out$count > 0, ]
    out[order(out$count), ]
  }

  rs <- conditional_foo(test.data)
  rs <- rs[order(-1 * (rs$count), rs$combination), ]
  rs.1 <- lapply(unique(rs$elementA), function(x) {
    xx <- rs[rs$elementA == x, ]
    xx <-
      data.frame(
        rules = xx$elementA[1],
        support = round(xx$Probability_A[1], 2)
      )
    return(xx)
  })

  rs.1 <- do.call(rbind, rs.1)

  compare.mat <- rs[rs$Probability_AgivenB >= min.prob &
    rs$count >= min.count &
    !rs$elementA %in% ignore.element &
    !rs$elementB %in% ignore.element, ]

  descriptive.graph <-
    graph_from_data_frame(compare.mat, directed = T, vertices = NULL)
  vertex.attributes(descriptive.graph)$element.prob <-
    rs.1$support[match(vertex.attributes(descriptive.graph)$name, rs.1$rules)]
  edge.attributes(descriptive.graph)$weight <-
    edge.attributes(descriptive.graph)$Probability_AgivenB

  missing.nodes <-
    base::setdiff(rs.1$rules, V(descriptive.graph)$name)
  descriptive.graph <- add_vertices(descriptive.graph,
    length(missing.nodes),
    attr = list(name = missing.nodes)
  )
  descriptive.graph <-
    delete_vertices(descriptive.graph, degree(descriptive.graph) ==
      0)

  net.graph <- descriptive.graph

  node.label <- vertex.attributes(net.graph)$name
  vertex.attributes(net.graph)$node.size <-
    vertex.attributes(net.graph)$element.prob

  p <- ggraph(
    graph = net.graph,
    layout = "igraph",
    algorithm = "kk"
  ) +
    geom_edge_link(
      mapping = aes(label = .data$weight),
      label_size = 3,
      arrow = arrow(
        type = "closed",
        angle = 15,
        length = unit(3, "mm")
      ),
      end_cap = circle(4, "mm"),
      start_cap = circle(4, "mm"),
      colour = "grey",
      fontface = 'bold',
      label_dodge = unit(2, "mm"),
      angle_calc = "along",
      show.legend = FALSE
    ) +
    geom_node_text(
      mapping = aes(
        label = .data$name,
        size = .data$node.size,
        fontface = "bold"
      ),
      show.legend = FALSE
    ) +
    scale_size(range = c(4, 12)) +
    theme_graph()

  if (plot.bubbles == T) {
    p <- p +
      geom_node_point(
        mapping = aes(size = .data$node.size + 3, alpha = 0.01),
        color = "lightblue",
        show.legend = F
      ) +
      coord_fixed() +
      geom_node_text(
        mapping = aes(
          label = .data$name,
          size = .data$node.size,
          fontface = "bold"
        ),
        show.legend = FALSE
      )
  }

  return(
    list(
      conditional.probalities = compare.mat,
      network.graph = descriptive.graph,
      plot = p
    )
  )
}
