# These functions are used internally by NetFACS and are not intended for end
# users.

#' Calculate expected probability from single bootstrap
#'
#' @param subject A character vector of unique subjects present in the data
#' @param subject.weight A numeric vector of weights to be used when sampling
#'   subjects
#' @param null.subjects A denoting the subject of null.elements
#' @param test.combinations A vector denoting AU combinations that are present
#'   in the test data
#' @param max.combination.size A positive integer indicating the maximum AU
#'   combination size considered in the bootstrap
#' @param max.event.size A positive integer indicating the maximum event size to
#'   be considered
#' @param null.data A matrix with data from the null condition 
#'
#' @return A list of bootstrapped probabilities for combinations and event sizes
#'
#' @importFrom Rfast Table
netfacs_bootstrap_na <- function(subject,
                                 subject.weight,
                                 null.subjects,
                                 test.combinations,
                                 max.combination.size,
                                 max.event.size,
                                 null.data) {
  subj.boot <- sample(
    x = subject,
    replace = TRUE,
    prob = subject.weight
  )
  
  # elements.boot <- null.elements[null.subjects %in% unique(subj.boot)]
  m.boot <- null.data[null.subjects %in% unique(subj.boot), ]
  
  # calculate probabilities for random dataset
  rs.boot <-
    probability_of_combination_na(
      m = m.boot,
      maxlen = max.combination.size
    )
  
  # ################## If there are NAs in the data, the following function re-calculates the number of possible events that combinations could have occurred in
  # if (any(is.na(null.data))) {
  #   
  #   max_possible_count_boot <- 
  #     count_complete_cases(
  #       rs.boot$combination, 
  #       null.data[null.subjects %in% unique(subj.boot), ]
  #     )
  #   
  #   rs.boot$observed.prob <- rs.boot$count/max_possible_count_boot
  # }
  
  # order to match real data
  boot.prob <-
    rs.boot$observed.prob[match(test.combinations, rs.boot$combination)]
  boot.prob[is.na(boot.prob)] <- 0
  # names(boot.prob) <- test.combinations
  
  # Calculate probability of event sizes (how many AUs active at once) occurring
  elements.boot <- get_active_elements_na(m.boot)
  event.size.prob <- probability_of_event_size(elements.boot, max.event.size)
  
  out <-
    list(
      combination.prob = boot.prob,
      event.size.prob = event.size.prob
    )
  
  return(out)
}


#' Calculate probabilities from single randomization
#'
#' @param m A numeric matrix
#' @param test.combinations A vector of AU combinations observed in test data
#' @param max.combination.size A positive integer
#' @param max.event.size A Positive integer
#'
#' @return A list of randomized probabilities for combinations and event sizes
#'
#' @importFrom picante randomizeMatrix
#' @importFrom Rfast rowsums Table
netfacs_randomize_na <- function(m,
                                 test.combinations,
                                 max.combination.size,
                                 max.event.size) {
  # randomly assign 0 and 1 to NAs
  m_na <- as.data.frame(
    lapply(1:ncol(m), function(x){
      xx <- m[,x]
      xx[is.na(xx)] <- rand_replace_na(xx)
      return(xx)
    })
  )
  colnames(m_na) = colnames(m)
  m_na <- as.matrix(m_na)
  
  # To establish the expected probabilities for SINGLE elements, the data is compared to permutations that keep the number of elements per row constant but allow for the element probability to vary
  m1 <-
    randomizeMatrix(
      samp = m_na,
      null.model = "richness",
      iterations = 100
    )
  
  elements.m1 <- get_active_elements(m1)
 
  rs.boot.1 <- probability_of_combination(
    elements = elements.m1,
    maxlen = 1
  )
  
  # for element combinations >= 2, do permutations that keep the number of cases per row and per column the same
  m2 <-
    randomizeMatrix(
      samp = m_na,
      null.model = "independentswap",
      iterations = 100
    )
  
  elements.m2 <- get_active_elements(m2)
 
  rs.boot.2 <- probability_of_combination(
    elements = elements.m2,
    maxlen = max.combination.size
  )
  
  # keep only combinations >1
  comb.size <- calculate_combination_size(rs.boot.2$combination)
  rs.boot.2 <- rs.boot.2[comb.size > 1, ]
  
  # combine the datasets
  rs.rand <- rbind(rs.boot.1, rs.boot.2)
  
  rand.prob <-
    rs.rand$observed.prob[match(test.combinations, rs.rand$combination)]
  rand.prob[is.na(rand.prob)] <- 0
  # names(rand.prob) <- test.combinations
  
  ##### combination size information per event; shuffle so that the number of time each element appears is kept constant, but number of elements per row differs
  m.es <-
    randomizeMatrix(
      samp = m_na,
      null.model = "frequency",
      iterations = 100
    )
  
  event.size.prob <- probability_of_event_size(m.es, max.event.size)
  
  out <-
    list(
      combination.prob = rand.prob,
      event.size.prob = event.size.prob
    )
  
  return(out)
}


#' Count complete cases
#'
#' If there are NAs in the data, the following function re-calculates the number
#' of possible events that combinations could have occurred in
#'
#' @param combination A character vector of AU combinations, separated by "_"
#' @param m A binary matrix
#'
#' @return A numeric vector
count_complete_cases <- function(combination, m) {
  sapply(combination, function(k){
    sum(
      stats::complete.cases(
        m[, split_combination(k)] 
      )
    )
  })
}

#' Split string separated by "_"
#'
#' @param x A string of AUs separated by "_"
#'
#' @return A character vector
split_combination <- function(x) {
  unlist(strsplit(as.character(x), "_", fixed = TRUE), FALSE, FALSE)
}

#' Randomly assign 0s and 1s to NA
#'
#' @param x A numeric vector
rand_replace_na <- function(x) {
  x[is.na(x)] <- sample(c(0,1), 
                        size = sum(is.na(x)), 
                        replace = TRUE, 
                        prob = c(as.numeric(table(x)/length(!is.na(x)))))
}

#' Extract active elements from matrix
#' 
#' Removes NA from result
#'
#' @param m A binary matrix where 1 indicates an element was active. colnames(m)
#'   must contain the element names
#'
#' @return A list of vectors
get_active_elements_na <- function(m) {
  lapply(1:nrow(m), function(x) {
    e <- colnames(m)[m[x, ] == 1]
    e[!is.na(e)]
  })
}

#' Calculate probabilities of single elements and combinations occurring
#' @param m A binary matrix where 1 indicates an element was active. colnames(m)
#'   must contain the element names
#' @param maxlen maximum size of combinations to be considered
#' @param sep String. Separator used for showing combinations of elements
#'
#' @return Function returns a dataframe with observed probabilities for each
#'   combination in the dataset
#'
#' @importFrom Rfast Table
probability_of_combination_na <- function(m, maxlen, sep = "_") {
  elements <- get_active_elements_na(m)
  
  # calculate all combinations per observation
  combs <- unlist(
    lapply(elements, function(x) {
      possible_combinations(x, maxlen, sep)
    })
  )  
  
  # count how many times each AU combination occurred
  n.combs <- Table(combs)
  combination <- names(n.combs)
  # max number of times that a combination could have occurred 
  max.count <- count_complete_cases(combination, m)
  observed.prob <- n.combs / max.count
  
  # put results in a data frame
  data.frame(
    combination = combination,
    observed.prob = as.numeric(observed.prob),
    count = as.numeric(n.combs),
    row.names = NULL
  )
}