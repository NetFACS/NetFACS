#' Calculate information content of the dataset
#'
#' Compares the observed and expected information content of the dataset.
#'
#' @param x An object of class \code{\link{netfacs}} or simply a binary matrix
#'   of 0s and 1s, with elements in columns and events in rows.
#'
#' @return Function returns a summary \code{\link[tibble:tibble]{tibble}}
#'   containing the observed entropy, expected entropy and entropy ratio
#'   (observed / expected) of the dataset. Observed entropy is calculated using
#'   Shannon's information entropy formula \eqn{- \sum_{i = 1}^n p_i \log
#'   (p_i)}. Expected entropy is based on randomization (shuffling the observed
#'   elements while maintaining the number of elements per row) and represents
#'   the maximum entropy that a dataset with the same properties as this one can
#'   reach. Ratios closer to 0 are more ordered; ratios closer to 1 are more
#'   random.
#'
#' @references Shannon, C. E. (1948). A Mathematical Theory of Communication.
#'   \emph{Bell System Technical Journal}.
#'   \code{https://doi.org/10.1002/j.1538-7305.1948.tb01338.x}
#'
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
#' entropy_overall(angry.face)
entropy_overall <- function(x) {
  UseMethod("entropy_overall")
}

#' @export
entropy_overall.default <- function(x) {
  
  m <- as.matrix(x)
  
  if (!all(m %in% c(0,1))) {
    stop("Argument 'x' must a matrix of 0s, 1s")
  }
  
  data.entropy <- information_entropy(m)
  
  # create random entropy by shuffling which elements are observed in each row while keeping the number of elements the same
  ran.entropy <- lapply(1:100, function(b) {
    ran.data <- randomizeMatrix(samp = m, null.model = "richness") 
    return(information_entropy(ran.data)) 
  })
  
  # use mean of randomized entropy values to create 'expected' entropy
  ran.entropy <-round(mean(unlist(ran.entropy)), 2) 
  # create entropy ratio
  entropy.ratio <- round(data.entropy / ran.entropy, 2) 
  
  return(
    tibble::tibble(
      observed.entropy = data.entropy,
      expected.entropy = ran.entropy,
      entropy.ratio = entropy.ratio
    )
  )
}

#' @export
entropy_overall.netfacs <- function(x) {
  m <- get_data(x, condition = "test")
  m <- as.matrix(m)
  entropy_overall.default(m)
}

#' @export
entropy_overall.netfacs_multiple <- function(x) {
  lapply(x, function(d) {
    entropy_overall.netfacs(d)  
  }) %>% 
    dplyr::bind_rows(.id = "condition")
}


#' (Deprecated) Calculate information content of the dataset
#' 
#' This function is deprecated. Please see \code{\link{entropy_overall}}
#' instead
#' 
#' @inheritParams entropy_overall
#' @param netfacs.data deprecated. Please use x instead.
#' @export
entropy.overall <- function(x, netfacs.data) {
  .Deprecated("netfacs_extract")
  
  if (!missing(netfacs.data)) {
    x <- netfacs.data
  }
  entropy_overall(x)
}

# helper ------------------------------------------------------------------

information_entropy <- function(m) {
  # m: binary matrix
  # create all observed combinations as combinations of 0 and 1
  mcombs <- unlist(lapply(1:nrow(m), function(x) {
    return(paste(m[x, ], collapse = ""))
  }))
  
  # Probability for each unique combination
  p <- table(mcombs) / length(mcombs)
  
  # Calculate the information content of the observed data
  round(-sum(p * log2(p)), 2)
}

