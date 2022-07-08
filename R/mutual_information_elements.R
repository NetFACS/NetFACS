#' Calculates the pointwise mutual information of units with each other
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a dataframe that includes all combinations, their
#'   occurrence counts and probabilities, and the pointwise mutual information
#'   (standardised between -1 and 1). 1 means seeing one necessitates seeing the
#'   other, -1 means one precludes the other
#'
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' \donttest{
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = NULL,
#'   test.condition = NULL,
#'   ran.trials = 100,
#'   combination.size = 4
#' )
#'
#' mutual.information(angry.face)
#' }

mutual.information <- function(netfacs.data){
  # select only combinations
  net.data <- netfacs.data$result[netfacs.data$result$combination.size>1,]
  
  # go through each combination, calculate mutual informations for all iterations
  results <- do.call(rbind, lapply(1:nrow(net.data), function(z){
    #select elements
    elements <- unlist(strsplit(net.data$combination[z], split = '_'))
    
    # make all possible combinations of one of the elements against all the others
    element.lists <- lapply(1:length(elements), function(x){
      A <- paste(elements[-x], collapse = '_')
      B <- paste(elements[x], collapse = '_')
      return(list(A = A, B = B))
    })
    # unlist it all
    A <- as.vector(unlist(sapply(element.lists, function(x){x[1]})))
    B <- as.vector(unlist(sapply(element.lists, function(x){x[2]})))
    A <- ifelse(A=='', NA, A)
    B[is.na(A)] <- NA
    
    # probability they occur together
    AandB <- rep(net.data$observed.prob[z], length(A))
    
    # probability for first part only
    Aall <- as.vector(sapply(A, function(x){
      if(is.na(x)){return(NA)}
      return(netfacs.data$result$observed.prob[netfacs.data$result$combination == x])
    }))
    
    # probability of second part
    Ball <- as.vector(sapply(B, function(x){
      if(is.na(x)){return(NA)}
      return(netfacs.data$result$observed.prob[netfacs.data$result$combination == x])
    }))
    
    # calculate Mutual Information: log2(p(A&B)/(p(A) * p(B))) / (-1 * log2(p(A&B))
    mi <- round(log2(
      (AandB)/
        (Aall * Ball))/
      (-1 * log2(AandB)), 3)
    
    mi <- ifelse(is.na(mi), -1, mi)
    
    return(data.frame(combination1 = A, 
                      element2 = B, 
                      combination.size = net.data$combination.size[z], 
                      count = net.data$count[z], 
                      observed.prob = AandB, 
                      mutual.information =  mi))
  }))
  
  results <- results[order(results$mutual.information, decreasing = T),]
  return(results)
}
