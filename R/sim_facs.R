#' Simulate FACS data
#'
#' @param m A matrix with condition as\code{rownames}, elements as
#'   \code{colnames}, and probabilities of observing an element as values.
#' @param jp An optional list of matrices, the same length as \code{nrow(m)}
#'   with the joint probabilities of elements
#' @param n_obs Number of observations per condition to simulate
#'   
#' @importFrom stats rbinom
#' @importFrom stats setNames
#' @export
#' 
#' @examples
#' elements <- as.character(1:10)
#' conditions <- letters[1:2]
#' # randomly generate probability of elements
#' probabilities <-
#'   sapply(elements, function(x) {
#'     p <- runif(length(conditions))
#'     setNames(round(p, 1), nm = conditions)
#'   })
#' sim_facs(probabilities)
sim_facs <- function(m, n_obs = 10, jp = NULL) {
  if (is.vector(m)) m <- matrix(m, nrow = 1, dimnames = list(NULL, names(m)))
  if (is.matrix(jp) & nrow(m) == 1) jp <- list(jp)
  
  if (!is.null(jp) & !is.list(jp)) {
    rlang::abort("jp must be a list of matrices the same length as nrow(m)")  
  }
  
  res <- vector("list", length = nrow(m))
  for (i in 1:nrow(m)) {
    res[[i]] <- sim_obs(p = m[i, ], 
                        jp = jp[[i]], 
                        context = rownames(m)[i],
                        n_obs = n_obs)
  }
  do.call(rbind, res)
}

# helpers -----------------------------------------------------------------

sample_single_prob <- function(p, context = NULL, n_obs = 10) {
  # p: a named vector aus as names, and probabilities as values
  res <- matrix(nrow = n_obs, ncol = length(p))
  for (i in 1:n_obs) {
    res[i, ] <- rbinom(length(p), size = 1, prob = p)
  }
  colnames(res) <- names(p)
  if (!is.null(context)) rownames(res) <- rep(context, times = n_obs)
  res
}

sample_joint_prob <- function(d, jp) {
  # d: A named binary matrix with AUs in cols and observations in rows. (res of sample_single_prob)
  # jp: A symmetrical matrix with conditional probabilities of AUs
  for (i in seq_len(nrow(d))) {
    obs <- d[i, ]
    active <- names(obs[obs == 1])
    inactive <- names(obs[obs == 0])
    
    # get joint prob of active and inactive AUs
    xx <- jp[active, inactive, drop = FALSE]
    res <- rbinom(length(xx), size = 1, prob = xx)
    if (is.matrix(xx) & length(res) != 0) {
      res <- matrix(res, nrow = nrow(xx), dimnames = list(NULL, colnames(xx)))
      res2 <- colSums(res)
    } else {
      res2 <- setNames(res, names(xx))
    }
    # update inactive AUs based on joint prob
    if (length(res2) != 0) d[i, inactive] <- res2
  }
  # when one AU has high joint prob with >=2 other AUs, it may be sampled to be active >1 per row. If so, set value to 1
  apply(d, 2, function(x) ifelse(x>1, 1, x))
}

sim_obs <- function(p, jp = NULL, context = NULL, n_obs = 10) {
  d <- sample_single_prob(p = p, context = context, n_obs = n_obs)
  
  if (!is.null(jp)) {
    d2 <- sample_joint_prob(d, jp)
    return(d2)
  } 
  d
}
