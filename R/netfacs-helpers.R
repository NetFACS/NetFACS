# These functions are used internally by NetFACS and are not intended for end users.

#' Calculate expected probability from single bootstrap
#'
#' @param subject A character vector of unique subjects present in the data
#' @param subject.weight A numeric vector of weights to be used when sampling
#'   subjects
#' @param null.subjects A denoting the subject of null.elements
#' @param null.elements A list of active elements in the null condition
#' @param test.combinations A vector denoting AU combinations that are present
#'   in the test data
#' @param max.combination.size A positive integer indicating the maximum AU
#'   combination size considered in the bootstrap
#' @param max.event.size A positive integer indicating the maximum event size to
#'   be considered
#'
#' @return A list of bootstrapped probabilities for combinations and event sizes
#'
#' @importFrom Rfast Table
netfacs_bootstrap <- function(subject,
                              subject.weight,
                              null.subjects,
                              null.elements,
                              test.combinations,
                              max.combination.size,
                              max.event.size) {
  subj.boot <- sample(
    x = subject,
    replace = TRUE,
    prob = subject.weight
  )
  
  elements.boot <- null.elements[null.subjects %in% unique(subj.boot)]
  
  # calculate probabilities for random dataset
  rs.boot <-
    probability_of_combination(
      elements = elements.boot,
      maxlen = max.combination.size
    )
  
  # order to match real data
  boot.prob <-
    rs.boot$observed.prob[match(test.combinations, rs.boot$combination)]
  boot.prob[is.na(boot.prob)] <- 0
  # names(boot.prob) <- test.combinations
  
  # Calculate probability of event sizes (how many AUs active at once) occurring
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
netfacs_randomize <- function(m,
                              test.combinations,
                              max.combination.size,
                              max.event.size) {
  # To establish the expected probabilities for SINGLE elements, the data is compared to permutations that keep the number of elements per row constant but allow for the element probability to vary
  m1 <-
    randomizeMatrix(
      samp = m,
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
      samp = m,
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
      samp = m,
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

#' Summarise combination results from bootstrap
#'
#' @param combination A vector of AU combinations
#' @param combination.size A vector denoting the number of active AUs in combination
#' @param boot.prob A matrix with boot probabilities of a given combination in
#'   columns
#' @param observed.prob A vector with probability of combination in test data
#' @param tail upper.tail or lower.tail,
#' @param test.count Number of times a combination occurs in test dataset
#' @param null.count Number of times a combination occurs in null dataset
#'
#' @importFrom Rfast rowsums Table
#' @return A dataframe
summarise_combination <- function(combination,
                                  combination.size,
                                  observed.prob,
                                  boot.prob,
                                  tail,
                                  test.count,
                                  null.count = NULL) {
  # check that arguments have same number of observations
  if (!equal_observations(combination, boot.prob, observed.prob)) {
    stop("combination, boot.prob, and observed.prob arguments must have the same number of observations.")
  }
  
  expected.prob <- Rfast::rowmeans(boot.prob)
  effect.size <- observed.prob - expected.prob
  
  # create probability increase by comparing the observed probability with the
  # mean probability of the randomization process
  prob.increase <- observed.prob / expected.prob
  prob.increase[is.infinite(prob.increase)] <- NA # Inf occurs when dividing 0/0
  
  if (tail == "upper.tail") {
    pvalue <- Rfast::rowmeans(boot.prob >= observed.prob)
  }
  if (tail == "lower.tail") {
    pvalue <- Rfast::rowmeans(boot.prob <= observed.prob)
  }
  
  out <-
    data.frame(
      combination = combination,
      combination.size = combination.size,
      count = test.count,
      expected.prob = expected.prob,
      observed.prob = observed.prob,
      effect.size = effect.size,
      pvalue = pvalue,
      prob.increase = prob.increase
    )
  
  # specificity is calculated only when a condition vector was specified
  # specificity: divide how often the combination occurs in the test condition by
  # the total count (test + null condition)
  if (!is.null(null.count)) {
    if (!equal_observations(test.count, null.count)) {
      stop("test.count and null.count arguments must have the same number of observations.")
    }
    out$specificity <- test.count / (test.count + null.count)
  } 
  return(out)
}


#' Summarise event size probabilities
#'
#' @param observed.prob A named vector with probabilities of event sizes.
#' @param boot.prob A matrix with boot probabilities of a given event
#'   size. Combination size in rows, trials in columns.
#'
#' @return A dataframe
summarise_event_size <- function(observed.prob,
                                 boot.prob) {
  
  # check that arguments have same number of observations
  if (!equal_observations(boot.prob, observed.prob)) {
    stop("Arguments must have the same number of observations.")
  }
  
  # observed.prob <- event.size.count / sum(event.size.count)
  expected.prob <- Rfast::rowmeans(boot.prob)
  
  # effect size here is the difference between the observed and expected values
  effect.size <- observed.prob - expected.prob
  
  # p-value: two-sided, testing how many of the permutation results for that combination size are more extreme than the observed value
  p.u <- Rfast::rowmeans(boot.prob >= observed.prob)
  p.l <- Rfast::rowmeans(boot.prob <= observed.prob)
  pvalue <- pmin(p.u, p.l)
  
  out <-
    data.frame(
      combination.size = names(observed.prob),
      observed.prob = observed.prob,
      expected.prob = expected.prob,
      effect.size = effect.size,
      pvalue = pvalue
    )
  out
}

#' Count number of event sizes
#'
#' @param elements A list of vectors containing active elements or a binary
#'   matrix with events in rows
#' @param max.event.size A positive integer
#'
#' @return A named vector, including probabilities for event sizes that were not
#'   observed in the data
#'
#' @importFrom Rfast rowsums Table
probability_of_event_size <- function(elements,
                                      max.event.size) {
  event.sizes <- 1:max.event.size
  
  # probability of event sizes in test data
  if (is.list(elements)) {
    es.count <- Table(sapply(elements, FUN = length))
  } else {
    es.count <- Table(rowsums(elements))
  }
  
  # calculate prob before filtering event.sizes
  es.prob <- es.count / sum(es.count)
  es.prob <- es.prob[names(es.prob) %in% event.sizes]
  
  # missing event sizes have count of 0
  es.prob.all <- rep(0, times = max.event.size)
  names(es.prob.all) <- event.sizes
  es.prob.all[match(names(es.prob), event.sizes)] <- es.prob
  
  return(es.prob.all)
}


#' Calculate probabilities of single elements and combinations occurring
#' @param elements list with vectors for all elements observed together at each
#'   event
#' @param maxlen maximum size of combinations to be considered
#'
#' @return Function returns a dataframe with observed probabilities for each
#'   combination in the dataset
#'
#' @importFrom Rfast Table
#' @importFrom arrangements combinations
probability_of_combination <- function(elements, maxlen) {
  # calculate all combinations per observation
  combs <- unlist(
    lapply(elements, function(x) {
      possible_combinations(x, maxlen)
    })
  )
  
  # count how many times each AU combination occurred
  n.combs <- Table(combs)
  observed.prob <- n.combs / length(elements)
  
  # put results in a data frame
  data.frame(
    combination = names(observed.prob),
    observed.prob = observed.prob,
    count = n.combs,
    row.names = NULL
  )
}

possible_combinations <- function(elements, maxlen) {
  unlist(
    lapply(
      1:min(length(elements), maxlen),
      function(comb_len) {
        apply(
          combinations(x = elements, k = comb_len),
          MARGIN = 1,
          FUN = paste, collapse = "_"
        )
      }
    )
  )
}

#' Extract active elements from matrix
#'
#' @param m A binary matrix where 1 indicates an element was active. colnames(m)
#'   must contain the element names
#'
#' @return A list of vectors
get_active_elements <- function(m) {
  elements <- lapply(1:nrow(m), function(x) {
    colnames(m)[m[x, ] == 1]
  })
}

#' Calculate combination size
#'
#' @param x A character vector of AU combinations, sep by _
#'
#' @return A vector
calculate_combination_size <- function(x) {
  unlist(
    lapply(
      strsplit(
        as.character(x), "_",
        fixed = TRUE
      ),
      length
    ),
    FALSE, FALSE
  )
}

#' Add inactive (missing) single units
#'
#' @param d A dataframe, result of \code{\link{probability_of_combination}}
#' @param single.units A character vector of single AUs
add_inactive_single_units <- function(d, single.units) {
  innactive.single.units <-
    single.units[!single.units %in% d$combination]
  
  if (length(innactive.single.units) > 0) {
    res <- data.frame(
      combination = innactive.single.units,
      observed.prob = 0,
      count = 0,
      combination.size = 1
    )
    return(rbind(res, d))
  }
  d
}

#' Check that ALL objects have the same number of observations
#'
#' lenght(vector), nrow(matrix), nrow(dataframe)
#' @param x Object to compare number of observations
#' @param ... Additional objects to compare number of observations
#'
#' @return Logical
equal_observations <- function(x, ...) {
  all(
    sapply(
      list(...),
      function(z) vctrs::vec_size(z) == vctrs::vec_size(x)
    )
  )
}
