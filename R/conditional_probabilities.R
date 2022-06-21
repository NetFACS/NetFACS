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
#' @importFrom dplyr left_join
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
#' conditional_probabilities(angry.face)
conditional_probabilities <- function(netfacs.data) {
  
  if (class(netfacs.data) != "netfacs") {
    stop("Argument 'netfacs.data' must be of class 'netfacs'.")
  }
  
  if (attr(netfacs.data, "stat_method") == "bootstrap") {
    m <- netfacs.data$used.data$data[netfacs.data$used.data$condition == netfacs.data$used.parameters$test.condition, ]  
  } else {
    m <- netfacs.data$used.data$data
  }
  
  # single elements
  elements <- colnames(m)
  # Total number of events will be different for each element if there are NAs
  N.events <- colSums(!is.na(m))
  N.obs <- colSums(m, na.rm = TRUE)
  p.elements <- N.obs / N.events


  # get dyadic combination of elements
  d <-
    tidyr::expand_grid(elementA = elements,
                       elementB = elements)%>%
    dplyr::filter(.data$elementA != .data$elementB)
  
  pA <-
    data.frame(
      elementA = elements,
      n_events_A = N.events,
      p_A = p.elements
    )
  pB <-
    data.frame(
      elementB = elements,
      n_events_B = N.events,
      p_B = p.elements
    )
  
  d2 <-
    d %>%
    dplyr::left_join(pA, by = "elementA") %>%
    dplyr::left_join(pB, by = "elementB")
  
  d3 <-
    d2 %>%
    dplyr::mutate(combination = paste(.data$elementA, .data$elementB, sep = "_"))
  
  # conditional probabilities/pmi must be calculated separately from complete cases of both elements
  res <- 
    lapply(1:nrow(d), function(x) {
      conditional_probs(m, d$elementA[x], d$elementB[x])
    }) %>% 
    dplyr::bind_rows()
  
  d4 <- 
    d3 %>% 
    dplyr::left_join(res, by = c("elementA", "elementB"))
  
  # tidy
  out <- 
    d4 %>% 
    dplyr::filter(.data$n_AandB > 0) %>% 
    dplyr::mutate_if(is.double, round, 3) %>% 
    dplyr::arrange(dplyr::desc(.data$n_AandB), .data$combination) %>%
    dplyr::select(
      "elementA", "elementB", "combination", "n_AandB", "p_A", "p_B", "p_AandB",
      "p_AgivenB","p_BgivenA", "pmi", "norm_pmi"
    )
  
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

conditional_probs <- function(m, A, B) {
  mAB <- m[complete.cases(m[, c(A, B)]), c(A, B)]
  
  n_events <- nrow(mAB)
  n_obs <- colSums(mAB)
  p <- n_obs / n_events
  pA <- as.numeric(p[A])
  pB <- as.numeric(p[B])
  
  n_AandB <- sum(rowSums(mAB) == 2)
  p_AandB <- n_AandB / n_events
  
  p_AgivenB <- p_AandB / pB
  p_BgivenA <- p_AandB / pA
  pmi_AandB <- pmi(p_AandB, pA, pB)
  norm_pmi_AandB <- npmi(pmi_AandB, p_AandB)
  
  tibble::tibble(
    elementA  = A,
    elementB  = B,
    n_AandB   = n_AandB,
    p_AandB   = p_AandB,
    p_AgivenB = p_AgivenB,
    p_BgivenA = p_BgivenA,
    pmi       = pmi_AandB,
    norm_pmi  = norm_pmi_AandB
  )
} 
