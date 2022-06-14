#' Summarise dyadic combination of elements
#'
#' For all dyadic combinations that appear in the test dataset, this function
#' returns the probability of A occurring (P(A)), the probability of B occurring
#' (P(B)), the probability of A and B occurring simultaneously (P(AandB)), the
#' probability of A given B (P(A|B)), pointwise mutual information (pmi) and
#' normalized pointwise mutual information (norm_pmi). The normalized pmi ranges
#' between -1 and 1; -1 for never occurring together, 0 for independence, and +1
#' for complete co-occurrence.
#'
#' @param netfacs.data An object of class \code{\link{netfacs}}
#'
#' @return A summary data.frame
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
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
#' summarise_pairs(angry.face)
summarise_pairs <- function(netfacs.data) {
  
  if (class(netfacs.data) != "netfacs") {
    stop("Argument 'netfacs.data' must be of class 'netfacs'.")
  }
  
  if (attr(netfacs.data, "stat_method") == "bootstrap") {
    m <- netfacs.data$used.data$data[netfacs.data$used.data$condition == netfacs.data$used.parameters$test.condition, ]  
  } else {
    m <- netfacs.data$used.data$data
  }
  
  elements <- colnames(m)
  
  # get dyadic combination of elements
  d <- 
    tidyr::expand_grid(elementA = elements,
                       elementB = elements) %>% 
    filter(.data$elementA != .data$elementB)
  
  
  # Total number of events will be different for each element if there are NAs
  N.events <- colSums(!is.na(m))
  N.obs <- colSums(m, na.rm = TRUE)
  p.elements <- N.obs / N.events
  
  # count_AandB
  N.pairs <-
    apply(d, 1, function(x) {
      sum(rowSums(m[, x], na.rm = TRUE) == 2)
    }) 
  
  pA <- 
    data.frame(elementA = elements,
               p_A      = p.elements)
  pB <- 
    data.frame(elementB = elements,
               p_B      = p.elements)
  
  d2 <- 
    d %>% 
    mutate(count = N.pairs) %>% 
    left_join(pA, by = "elementA") %>% 
    left_join(pB, by = "elementB")
  
  d3 <- 
    d2 %>% 
    mutate(
      p_AandB   = .data$count / N.events,
      p_AgivenB = .data$p_AandB / .data$p_B,
      p_BgivenA = .data$p_AandB / .data$p_A,
      pmi       = pmi(.data$p_AandB, .data$p_A, .data$p_B),
      norm_pmi  = npmi(.data$pmi, .data$p_AandB)
    )
  
  # tidy
  out <- 
    d3 %>% 
    unite("combination", c("elementA", "elementB"), remove = FALSE) %>% 
    relocate(.data$combination, .after = .data$elementB) %>% 
    filter(.data$count > 0) %>% 
    dplyr::mutate_if(is.double, round, 3) %>% 
    arrange(desc(.data$count), .data$combination)
  
  return(out)
}


# helpers -----------------------------------------------------------------

# Pointwise Mutual Information: 
pmi <- function(pAandB, pA, pB) {
  log2(pAandB / (pA * pB))
}

# normalized pmi; ranges between -1 and 1
# -1 for never occurring together, 0 for independence, and +1 for complete co-occurrence.
npmi <- function(pmiAB, pAandB) {
  pmiAB / (-log2(pAandB))
}

