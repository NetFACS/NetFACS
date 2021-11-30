#' Define truth for AUs active in different contexts
#'
#' @param aus A character vector of AUs
#' @param contexts A character vector of contexts
#' @param n_active_aus A numeric vector, the same length as contexts, indicating
#'   the number of AUs active per context.
#' @param au_fidelity A number between 1 and 0.5, indicating the probability
#'   that an AU is active in a context.
#'
#' @return A matrix of probabilities with contexts in rows and AUs in columns
define_contexts <- function(aus, n_active_aus, 
                            contexts = NULL, au_fidelity = 1) {
  
  if(is.null(contexts)) lc <- 1 else lc <- length(contexts)
  if (lc != length(n_active_aus)) {
    stop("contexts and n_active_aus must be the same length.") 
  }
  if (max(au_fidelity)>1 | min(au_fidelity)<0.5) {
    stop("au_fidelity must be between 1 (perfect) and 0.5 (random).")  
  }
  
  # sample which aus are active per condition, observing number of active ones 
  active_aus <- setNames(vector("list", length = lc), contexts)
  
  for (i in seq_along(active_aus)) {
    active_aus[[i]] <- sample(aus, size = n_active_aus[i], replace = FALSE)
  }
  
  # create 1,0 matrix based on which AUs should be active per context
  active_aus2 <- lapply(active_aus, function(x) aus %in% x)
  
  active_aus3 <- do.call(rbind, active_aus2)
  colnames(active_aus3) <- aus
  
  active_aus4 <- apply(active_aus3, 2, function(x) ifelse(x, 1, 0))
  
  if(is.matrix(active_aus2)){
    apply(active_aus4, 2, function(x) ifelse(x==1, au_fidelity, 1-au_fidelity))
  } else {
    active_aus4 <- ifelse(active_aus4==1, au_fidelity, 1-au_fidelity)
  }
  
  active_aus4
}
