#' Take vector of elements and calculate probabilities of elements and combinations occurring
#'
#' Underlying code is C++ based
#'
#'
#' @param elements list with vectors for all elements observed together at each event
#' @param maxlen maximum size of combinations to be considered
#'
#' @return Function returns a dataframe with observed probabilities for each combination in the dataset
#' @author Alex Mielke
#' @importFrom Rfast comb_n Table

create_rule_set3 <- function(elements, maxlen) {
  xtab <- Table(rapply(elements, function(x) {
    yy <- unlist(x, FALSE, FALSE)
    yl <- length(yy)
    xx <- lapply(1:min(yl, maxlen), function(y) {
      apply(matrix(yy[comb_n(n = yl, k = y, simplify = T)], nrow = y), 2, paste, collapse = "_")
    })
    xx
  }))
  xtab2 <- as.numeric(xtab)
  res <- data.frame(
    combination = names(xtab),
    observed.prob = xtab2 / length(elements),
    count = xtab2
  )
  res
}
