#' Creates a network object out of the netfacs data
#'
#' Takes the results of the nefacs object for combinations of 2 elements and
#' turns them into a network object (igraph or sna/network) that can be used for
#' further plotting and analyses
#'
#'
#' @param netfacs.data object resulting from \code{\link{netfacs}} function
#' @param link determines how nodes/elements are connected. 'unweighted' gives a
#'   1 to significant connections and 0 to all others; 'weighted' gives the
#'   difference between observed and expected probability of co-occurrence;
#'   'raw' just uses the observed probability of co-occurrence
#' @param min.count numeric value, suggesting how many times a combination
#'   should at least occur to be displayed
#' @param min.prob numeric value, suggesting the probability at which a
#'   combination should at least occur to be displayed
#' @param min.specificity numeric value, suggesting the specificity a
#'   combination should at least have for the test condition to be displayed
#' @param significance numeric value, determining the p-value below which
#'   combinations are considered to be dissimilar enough from the null
#'   distribution
#' @param ignore.element vector of elements that will not be considered for the
#'   network, e.g. because they are too common or too rare or their
#'   interpretation is not relevant here
#'
#' @return Function returns a network object where the nodes are the elements,
#'   edges represent their co-occurrence, and the vertex and edge attributes
#'   contain all additional information from the netfacs object
#'
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph V
#' @importFrom tidygraph activate
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidyr separate
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
  
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  edges <- NULL # to avoid R CMD check when calling activate(edges)
  
  compare.mat <-
    netfacs.data$result %>%
    filter(.data$combination.size == 2)
  
  node.weight <-
    netfacs.data$result %>%
    filter(.data$combination.size == 1) %>%
    mutate(across(.data$pvalue, ~ifelse(.data$effect.size < 0, 1, .)))
  
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    compare.mat <-
      compare.mat %>%
      mutate(specificity = 1,
             prob.increase = 1)
  }
  
  compare.mat <-
    compare.mat %>%
    filter(.data$observed.prob >= min.prob,
           .data$count         >= min.count,
           .data$specificity   >= min.specificity,
           .data$observed.prob > .data$expected.prob) %>%
    mutate(across(.data$prob.increase, ~ifelse(is.na(.), 10, .))) %>%
    separate(.data$combination, into = c("element1", "element2"), 
             sep = "_", remove = FALSE)
  
  compare.mat <-
    compare.mat %>%
    filter(!.data$element1 %in% ignore.element,
           !.data$element2 %in% ignore.element) %>%
    select("element1",
           "element2",
           "prob.increase",
           "observed.prob",
           "expected.prob",
           "effect.size",
           "pvalue",
           "specificity",
           "count",
           "combination")
  
  g <- 
    compare.mat %>% 
    graph_from_data_frame(directed = FALSE) %>% 
    as_tbl_graph()
  
  # edit node attributes
  g2 <- 
    g %>% 
    activate(nodes) %>% 
    left_join(node.weight %>% select("combination", "observed.prob", "pvalue"),
              by = c("name" = "combination")) %>% 
    rename(element.prob = .data$observed.prob,
           element.significance = .data$pvalue)
  
  # edit edge attributes
  if (link == "unweighted") {
    g3 <- 
      g2 %>% 
      activate(edges) %>% 
      mutate(unweighted = .data$pvalue <= significance & .data$effect.size > 0) %>% 
      filter(.data$unweighted) %>% 
      mutate(association = .data$observed.prob - .data$expected.prob)
  }
  if (link == "weighted") {
    g3 <- 
      g2 %>% 
      activate(edges) %>% 
      mutate(weight = .data$observed.prob - .data$expected.prob)
  }
  if (link == "raw") {
    g3 <- 
      g2 %>% 
      activate(edges) %>% 
      mutate(weight = .data$observed.prob)
  }
  
  all.nodes <- node.weight %>% pull("combination")
  missing.nodes <- setdiff(all.nodes, V(g3)$name)
  missing.nodes2 <- setdiff(missing.nodes, ignore.element)
  
  g4 <- 
    g3 %>% 
    activate(nodes) %>% 
    full_join(data.frame(name = missing.nodes2), by = "name")
  
  return(g4)
}
