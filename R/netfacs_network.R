#' Creates a network object out of the netfacs data
#'
#' Takes the results of the nefacs object for combinations of 2 elements and turns them into a network object (igraph or sna/network) that can be used for further plotting and analyses
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#' @param link determines how nodes/elements are connected. 'unweighted' gives a 1 to significant connections and 0 to all others; 'weighted' gives the difference between observed and expected probability of co-occurrence; 'raw' just uses the observed probability of co-occurrence; 'SRI' uses the simple ratio index/affinity (probability of co-occurrence/ (probabilities of each element and the combination))
#' @param min.count numeric value, suggesting how many times a combination should at least occur to be displayed
#' @param min.prob numeric value, suggesting the probability at which a combination should at least occur to be displayed
#' @param min.specificity numeric value, suggesting the specificity a combination should at least have for the test condition to be displayed
#' @param significance numeric value, determining the p-value below which combinations are considered to be dissimilar enough from the null distribution
#' @param ignore.element vector of elements that will not be considered for the network, e.g. because they are too common or too rare or their interpretation is not relevant here
#'
#' @return Function returns a network object where the nodes are the elements, edges represent their co-occurrence, and the vertex and edge attributes contain all additional information from the netfacs object
#'
#' @importFrom igraph vertex.attributes
#' @importFrom igraph vertex.attributes<-
#' @importFrom igraph add_vertices V
#' @importFrom igraph edge.attributes
#' @importFrom igraph edge.attributes<-
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph graph.adjacency
#' @importFrom igraph delete_edges
#' @importFrom igraph add_vertices
#' @importFrom igraph get.data.frame
#' @importFrom stats complete.cases
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
netfacs.network <- function(netfacs.data,
                            link = "unweighted",
                            significance = 0.01,
                            min.count = 1,
                            min.prob = 0,
                            min.specificity = 0,
                            ignore.element = NULL) {
  ### create dyadic association using dyad.comparison
  compare.mat <- netfacs.data$result[netfacs.data$result$combination.size ==
    2, ]
  node.weight <- netfacs.data$result[netfacs.data$result$combination.size ==
    1, ]
  node.weight$pvalue[node.weight$effect.size < 0] <- 1
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    compare.mat$specificity <- 1
    compare.mat$prob.increase <- 1
  }
  compare.mat <- compare.mat[compare.mat$observed.prob >= min.prob &
    compare.mat$count >= min.count &
    compare.mat$specificity >= min.specificity &
    compare.mat$observed.prob > compare.mat$expected.prob, ]
  compare.mat$prob.increase[is.na(compare.mat$prob.increase)] <- 10
  compare.mat <- compare.mat[complete.cases(compare.mat), ]
  compare.mat$element1 <- sapply(compare.mat$combination, function(x) {
    xx <- sort(unlist(strsplit(as.character(x), split = "_")))
    return(xx[1])
  })
  compare.mat$element2 <- sapply(compare.mat$combination, function(x) {
    xx <- sort(unlist(strsplit(as.character(x), split = "_")))
    return(xx[2])
  })

  compare.mat <- compare.mat[!(compare.mat$element1 %in% ignore.element) &
    !(compare.mat$element2 %in% ignore.element), ]

  compare.mat <- compare.mat[, c(
    "element1",
    "element2",
    "prob.increase",
    "observed.prob",
    "expected.prob",
    "effect.size",
    "pvalue",
    "specificity",
    "count",
    "combination"
  )]

  descriptive.graph <- graph_from_data_frame(compare.mat, directed = F, vertices = NULL)
  vertex.attributes(descriptive.graph)$element.prob <- node.weight$observed.prob[match(
    vertex.attributes(descriptive.graph)$name,
    node.weight$combination
  )]
  vertex.attributes(descriptive.graph)$element.significance <- node.weight$pvalue[match(
    vertex.attributes(descriptive.graph)$name,
    node.weight$combination
  )]

  if (link == "unweighted") {
    edge.attributes(descriptive.graph)$unweighted <- (
      edge.attributes(descriptive.graph)$pvalue <= significance &
        edge.attributes(descriptive.graph)$effect.size > 0
    )
    descriptive.graph <- delete_edges(descriptive.graph, edges = which(!edge.attributes(descriptive.graph)$unweighted))
    edge.attributes(descriptive.graph)$weight <- as.numeric(edge.attributes(descriptive.graph)$unweighted)
    edge.attributes(descriptive.graph)$association <- edge.attributes(descriptive.graph)$observed.prob - edge.attributes(descriptive.graph)$expected.prob
  }
  if (link == "weighted") {
    edge.attributes(descriptive.graph)$weight <- edge.attributes(descriptive.graph)$observed.prob - edge.attributes(descriptive.graph)$expected.prob
  }
  if (link == "raw") {
    edge.attributes(descriptive.graph)$weight <- edge.attributes(descriptive.graph)$observed.prob
  }
  if (link == "SRI") {
    xx.sri <- graph.adjacency(netfacs.data$affinity, weighted = TRUE)
    xx.sri <- get.data.frame(xx.sri)
    xx.sri.dyad <- unlist(lapply(1:nrow(xx.sri), function(x) {
      paste(sort(xx.sri[x, 1:2]), collapse = "_")
    }))
    edge.attributes(descriptive.graph)$weight <- xx.sri$weight[match(
      edge.attributes(descriptive.graph)$combination,
      xx.sri.dyad
    )]
  }

  missing.nodes <- setdiff(node.weight$combination, V(descriptive.graph)$name)
  missing.nodes <- setdiff(missing.nodes, ignore.element)
  descriptive.graph <- add_vertices(descriptive.graph,
    length(missing.nodes),
    attr = list(name = missing.nodes)
  )

  return(descriptive.graph)
}
