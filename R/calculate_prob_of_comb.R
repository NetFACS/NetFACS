#' Calculate probabilities of single elements and combinations occurring
#' 
#' 
#'
#' @param elements list with vectors for all elements observed together at each event
#' @param maxlen maximum size of combinations to be considered
#'
#' @return Function returns a dataframe with observed probabilities for each combination in the dataset
#' 
#' @importFrom Rfast Table
#' @importFrom arrangements combinations
calculate_prob_of_comb <- function(elements, maxlen) {
  # calculate all possible AU combinations for each element/observation
  zz <- rapply(elements, 
               function(x){
                 yy <- unlist(x, recursive = FALSE, use.names = FALSE)
                 xx <- compute_possible_combs(aus = yy, max_comb_len = maxlen)
               }, 
               how = "unlist")
  # count how many times each AU combination occurred
  xtab <- Table(zz)
  
  # put results in a data frame
  xtab2 <- as.numeric(xtab)
  res <- data.frame(
    combination = names(xtab),
    observed.prob = xtab2 / length(elements),
    count = xtab2
  )
  res
}


# helper ------------------------------------------------------------------

compute_possible_combs <- function(aus, max_comb_len) {
  # given a vector of AUs, computes all possible unique combinations among them
  lapply(1:min(length(aus), max_comb_len), function(comb_len) {
    apply(
      combinations(x = aus, k = comb_len),
      MARGIN =  1,
      FUN = paste, collapse = "_")})
}

