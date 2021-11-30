#' Joint probability distribution of AUs
#'
#' @param aus A character vector of AUs
#' @param n_jp Number of joint probabilities >0
#' @param min_jp Minimum joint probability. Must be between 0 and 1
define_joint_prob <- function(aus, n_jp = 2, min_jp = 0.5) {
  jp <- matrix(0, nrow = length(aus), ncol = length(aus), 
               dimnames = list(aus, aus))
  diag(jp) <- NA
  p <- sample(seq(from = min_jp, to = 1, by = 0.1), 
              size = n_jp, replace = TRUE)
  i <- sample((1:length(jp))[!is.na(jp)], 
              size = length(p), replace = FALSE)
  jp[i] <- p
  jp
}
