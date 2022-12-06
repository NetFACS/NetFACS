#' Summarise dyadic combination of elements
#'
#' For all dyadic combinations that appear in the test dataset, this function
#' returns the probability of A occurring (P(A)), the probability of B occurring
#' (P(B)), the probability of A and B occurring simultaneously (P(AandB)) and, the
#' probability of A given B (P(A|B)).
#'
#' @param netfacs.data An object of class \code{\link{netfacs}} or
#'   \code{\link{netfacs_multiple}}
#'
#' @return A summary \code{\link[tibble:tibble]{tibble}}
#' @export
#'
#' @seealso \code{\link{network_conditional}}
#' 
#' @examples
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 50,
#'   combination.size = 2
#' )
#'
#' conditional_probabilities(angry.face)
conditional_probabilities <- function(netfacs.data) {
  if (isFALSE(is.netfacs(netfacs.data) | is.netfacs_multiple(netfacs.data))) {
    stop("'Argument 'netfacs.data' must be of class 'netfacs' or 'netfacs_multiple'.")
  }
  UseMethod("conditional_probabilities")
}

#' @export
conditional_probabilities.netfacs <- function(netfacs.data) {
  
  # if (attr(netfacs.data, "stat_method") == "bootstrap") {
  #   m <- netfacs.data$used.data$data[netfacs.data$used.data$condition == netfacs.data$used.parameters$test.condition, ]
  # } else {
  #   m <- netfacs.data$used.data$data
  # }
  
  m <- get_data(netfacs.data, condition = "test")
  
  epairs <- arrangements::combinations(colnames(m), 2)
  epairs2 <- rbind(epairs, epairs[, 2:1])
  
  # pA and pB
  pA_pB <- 
    apply(epairs2, 2, function(x) {
      colSums(m[, x], na.rm = TRUE) / colSums(!is.na(m[, x]))
    })
  
  # nAandB
  n_AandB <- 
    apply(epairs2, 1, function(x) {
      sum(rowSums(m[, x, drop = FALSE], na.rm = TRUE) == 2)
    })
  
  # pAandB = n_AandB / n_events
  p_AandB <-
    apply(epairs2, 1, function(x) {
      mAB <- m[stats::complete.cases(m[, x]), x, drop = FALSE]
      sum(rowSums(mAB[, x, drop = FALSE]) == 2) / nrow(mAB)
    }) 
  
  # PA|B = p_AandB / pB
  p_AgivenB <-
    apply(epairs2, 1, function(x) {
      mAB <- m[stats::complete.cases(m[, x]), x, drop = FALSE]
      sum(rowSums(mAB[, x, drop = FALSE]) == 2) / sum(mAB[, x[2]])
    })
  
  # PB|A = p_AandB / pA
  p_BgivenA <-
    apply(epairs2, 1, function(x) {
      mAB <- m[stats::complete.cases(m[, x]), x, drop = FALSE]
      sum(rowSums(mAB[, x, drop = FALSE]) == 2) / sum(mAB[, x[1]])
    })
  
  out <- 
    tibble::tibble(
      element_A           = epairs2[, 1],
      element_B           = epairs2[, 2],
      combination         = paste(epairs2[, 1], epairs2[, 2], sep = "_"),
      count               = n_AandB,
      probability_A       = pA_pB[, 1],
      probability_B       = pA_pB[, 2],
      probability_AandB   = p_AandB,
      probability_AgivenB = p_AgivenB,
      probability_BgivenA = p_BgivenA
    ) %>% 
    dplyr::arrange(dplyr::desc(.data$count)) %>% 
    dplyr::mutate_if(is.double, round, 2) %>% 
    dplyr::filter(.data$count > 0) 
  
  class(out) <- c("netfacs_conditional", class(out))
  return(out)
}

#' @export
conditional_probabilities.netfacs_multiple <- function(netfacs.data) {
  out <- 
    lapply(netfacs.data, function(x) {
      conditional_probabilities.netfacs(x)
    }) %>% 
    dplyr::bind_rows(.id = "condition")
  
  class(out) <- c("netfacs_conditional", class(out))
  return(out)
}


