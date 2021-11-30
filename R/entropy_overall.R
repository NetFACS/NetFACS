#' Compares the observed and expected information content of the dataset
#'
#' Establishes how'ordered' the data is: values close to 0 indicate that
#' combinations are highly repetitive and predictable, while values close to 1
#' indicate that combinations are equiprobable and prediction of future
#' combinations is difficult
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns the ratio of observed entropy/expected entropy.
#'   Expected entropy is based on randomization (shuffling the observed elements
#'   while maintaining the number of elements per row) and represents the
#'   maximum entropy a dataset with the same properties as this one can reach.
#'   Ratios closer to 0 are more ordered; ratios closer to 1 are more random.
#' @importFrom picante randomizeMatrix
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' entropy.overall(angry.face)
entropy.overall <- function(netfacs.data) {
  # take data for the test condition only
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    netfacs.data$used.parameters$test.condition <- "all"
    netfacs.data$used.data$condition <-
      rep("all", times = nrow(netfacs.data$used.data$data))
  }
  data <-
    as.matrix(netfacs.data$used.data$data[netfacs.data$used.data$condition ==
      netfacs.data$used.parameters$test.condition, ])

  # create all observed combinations as combinations of 0 and 1
  data_mat <- unlist(lapply(1:nrow(data), function(x) {
    return(paste(data[x, ], collapse = ""))
  }))

  # Count the number of occurrences for each unique combination
  t <- table(data_mat) / length(data_mat)

  # Calculate the information content of the observed data
  data.entropy <- round(-sum(t * log2(t)), 2)

  # create random entropy by shuffling which elements are observed in each row while keeping the number of elements the same
  ran.entropy <- lapply(1:100, function(b) {
    ran.data <-
      randomizeMatrix(samp = as.matrix(data), null.model = "richness") # select outcome of shuffle
    ran_mat <- unlist(lapply(1:nrow(ran.data), function(x) {
      # reduce to combinations
      return(paste(ran.data[x, ], collapse = ""))
    }))
    t <- table(ran_mat) / length(ran_mat)
    return(round(-sum(t * log2(t)), 2)) # calculate random entropy
  })

  ran.entropy <-
    round(mean(unlist(ran.entropy)), 2) # use mean of randomised entropy values to create 'expected' entropy
  entropy.ratio <-
    round(data.entropy / ran.entropy, 2) # create entropy ratio
  return(
    data.frame(
      observed.entropy = data.entropy,
      expected.entropy = ran.entropy,
      entropy.ratio = entropy.ratio
    )
  )
}
