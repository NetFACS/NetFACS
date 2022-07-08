#' Tests how much each element increases the specificity of all combinations it
#' is in
#'
#' The function takes all elements and dyadic combinations of elements in a
#' netfacs object, goes through all combinations these elements are in, and
#' compares the specificity (strength with which the combination identifies the
#' test condition) of all combinations with the element and the same
#' combinations without the element, to test how much specificity the element
#' adds when added to a signal. Only works for netfacs objects based on
#' comparison between conditions.
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a list with two data frames that include all
#'   elements and first-order combinations that occur at all, the number of
#'   combinations that each element/combination is part of, and how much adding
#'   this element to a combination adds on average to its specificity, and how
#'   often it occurs
#'
#' @importFrom Rfast rowmeans
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' \donttest{
#' data(emotions_set)
#' angry.face <- netfacs(
#'  data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   null.condition = NULL,
#'   ran.trials = 100,
#'   combination.size = 4
#' )
#'
#' head(mutual.information.condition(angry.face), 20)
#' }

mutual.information.condition <- function(netfacs.data){ 
  
  used.data <- netfacs.data$used.data$data
  condition <- netfacs.data$used.data$condition
  combinations <- netfacs.data$result$combination
  combinations.unlist <- unlist(lapply(netfacs.data$result$combination, 
                                       strsplit, split = '_'), recursive = F)
  
  condition.probs <- do.call(rbind, lapply(unique(condition), function(z){
    
    Aall <- mean(condition == z)
    combination.probs <- do.call(
      rbind, lapply(1:length(combinations), function(x){
        
        if(length(combinations.unlist[[x]]) == 1){
          Ball <- mean(used.data[,combinations.unlist[[x]]] == 1)
          AandB <- sum(used.data[condition == z,combinations.unlist[[x]]] == 1)/nrow(used.data)
          BgivenA <- mean(used.data[condition == z,combinations.unlist[[x]]] == 1)
          count <- sum(used.data[condition == z,combinations.unlist[[x]]] == 1)
        }
        if(length(combinations.unlist[[x]]) > 1){
          Ball <- mean(rowmeans(used.data[,combinations.unlist[[x]]]) == 1)
          AandB <- sum(rowmeans(used.data[condition == z,combinations.unlist[[x]]]) == 1)/nrow(used.data)
          BgivenA <- mean(rowmeans(used.data[condition == z,combinations.unlist[[x]]]) == 1)
          count <- sum(rowmeans(used.data[condition == z,combinations.unlist[[x]]]) == 1)
        }
        
        
        mi <- round(
          log(
        (AandB)/
          (Aall * Ball))/
          (-1 * log(AandB)), 3)
      mi <- ifelse(is.na(mi), -1, mi)
      
      
      return(data.frame(condition = z, 
                        combination = combinations[x],
                        combination.size = length(combinations.unlist[[x]]),
                        count = count,
                        conditional.probability = round(BgivenA, 3),
                        mutual.information = mi))
    }))
    
    return(combination.probs)
  }))
  
  condition.probs <- condition.probs[order(condition.probs$mutual.information, decreasing = T),]
  return(condition.probs) 
}