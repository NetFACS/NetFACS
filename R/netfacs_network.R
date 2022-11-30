#' Creates a network object out of the netfacs data
#'
#' Takes the results of the nefacs object for combinations of 2 elements and
#' turns them into a network object (igraph) that can be used for
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
#' @importFrom magrittr `%>%`
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
#' anger.net <- netfacs_network(
#'   netfacs.data = angry.face,
#'   link = "unweighted",
#'   significance = 0.01,
#'   min.count = 1
#' )
netfacs_network <- function(
    netfacs.data,
    link = "unweighted",
    significance = 0.01,
    min.count = 1,
    min.prob = 0,
    ignore.element = NULL
) {
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  edges <- NULL # to avoid R CMD check when calling activate(edges)
  
  compare.mat <-
    netfacs.data %>% 
    netfacs_extract(combination.size = 2)
  
  node.weight <-
    netfacs.data %>%
    netfacs_extract(combination.size = 1) %>% 
    dplyr::mutate(dplyr::across(
      "pvalue", ~ifelse(.data$effect.size < 0, 1, .)
    )) %>% 
    dplyr::select("combination", "observed.prob", "pvalue")
  
  compare.mat <-
    compare.mat %>%
    dplyr::filter(.data$observed.prob >= min.prob,
                  .data$count         >= min.count,
                  .data$observed.prob > .data$expected.prob) %>%
    dplyr::mutate(dplyr::across("prob.increase", ~ifelse(is.na(.), 10, .))) %>%
    tidyr::separate("combination", into = c("element1", "element2"), 
                    sep = "_", remove = FALSE)
  
  compare.mat <-
    compare.mat %>%
    dplyr::filter(!.data$element1 %in% ignore.element,
                  !.data$element2 %in% ignore.element) %>%
    dplyr::select(
      "element1", "element2", "prob.increase", "observed.prob", "expected.prob",
      "effect.size", "pvalue", "count", "combination"
    )
  
  g <- 
    compare.mat %>% 
    igraph::graph_from_data_frame(directed = FALSE) %>% 
    tidygraph::as_tbl_graph()
  
  # edit node attributes
  g2 <- 
    g %>% 
    tidygraph::activate(nodes) %>% 
    dplyr::left_join(node.weight, by = c("name" = "combination")) %>% 
    dplyr::rename(element.prob = "observed.prob",
                  element.significance = "pvalue")
  
  # edit edge attributes
  if (link == "unweighted") {
    g3 <- 
      g2 %>% 
      tidygraph::activate(edges) %>% 
      dplyr::filter(.data$pvalue <= significance, 
                    .data$effect.size > 0)
  }
  if (link == "weighted") {
    g3 <- 
      g2 %>% 
      tidygraph::activate(edges) %>% 
      dplyr::mutate(weight = .data$effect.size)
  }
  if (link == "raw") {
    g3 <- 
      g2 %>% 
      tidygraph::activate(edges) %>% 
      dplyr::mutate(weight = .data$observed.prob)
  }
  
  all.nodes <- node.weight[["combination"]]
  missing.nodes <- setdiff(all.nodes, igraph::V(g3)$name)
  missing.nodes2 <- setdiff(missing.nodes, ignore.element)
  
  g4 <- 
    g3 %>% 
    tidygraph::activate(nodes) %>% 
    dplyr::full_join(data.frame(name = missing.nodes2), by = "name")
  
  return(g4)
}

#' @rdname netfacs_network
#' @export
netfacs.network <- function(
    netfacs.data,
    link = "unweighted",
    significance = 0.01,
    min.count = 1,
    min.prob = 0,
    ignore.element = NULL
) {
  netfacs_network(
    netfacs.data,
    link,
    significance,
    min.count,
    min.prob,
    ignore.element
  )
}
