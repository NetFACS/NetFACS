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
#' @param x object resulting from \code{\link{specificity}} function
#'
#' @return Function returns a list with two data frames that include all
#'   elements and first-order combinations that occur at all, the number of
#'   combinations that each element/combination is part of, and how much adding
#'   this element to a combination adds on average to its specificity, and how
#'   often it occurs
#'   
#' @importFrom magrittr %>%
#' 
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 10,
#'   combination.size = 2
#' )
#'
#' spec <- specificity(angry.face)
#' specificity_increase(spec)
specificity_increase <- function(x) {
  if (!is.netfacs_specificity(x)) {
    rlang::abort("x must be of class netfacs_specificity")
  }
  
  ed <- 
    x %>% 
    dplyr::filter(.data$combination.size %in% 1:2, 
                  .data$count >= 1)
  
  # make list of all possible
  all.combinations <-
    lapply(x$combination, function(z) {
      unlist(strsplit(as.character(z), split = "_", fixed = TRUE))
    })
  ed.combinations <- 
    lapply(ed$combination, function(z) {
      unlist(strsplit(as.character(z), split = "_", fixed = TRUE))
    })
  
  # helpers
  combination_in_vector <- function(comb, vec) {
    length(intersect(comb, vec)) == length(comb)
  }
  combination_in_list <- function(comb, l) {
    unlist(
      lapply(l, function(l.comb){
        combination_in_vector(comb, l.comb)
      })
    )
  }
  
  number.combinations <- 
    lapply(ed.combinations, function(t.comb) {
      combinations.i <- combination_in_list(t.comb, all.combinations)
      combinations.with <- all.combinations[combinations.i]
      length(combinations.with)
    })
  
  specificity.increase <- 
    lapply(ed.combinations, function(t.comb) {
      
      combinations.i <- combination_in_list(t.comb, all.combinations)
      elements.with <- x[combinations.i, ]
      combinations.with <- all.combinations[combinations.i]
      
      specificity.with <- elements.with$specificity
      
      combinations.without <-
        lapply(combinations.with, function(z) {
          xx <- z[!z %in% t.comb] 
          if (length(xx) > 1) {
            xx <- paste(xx, collapse = "_")
          }
          return(xx)
        })
      
      specificity.without <-
        x$specificity[x$combination %in% unlist(combinations.without)]
      
      spec.increase <- 
        mean(specificity.with, na.rm = T) - mean(specificity.without, na.rm = T)
      
      return(spec.increase)
    })
  
  # create new dataframe only for those elements
  es <-
    tibble::tibble(
      condition = ed$condition,
      element = ed$combination,
      number.combinations = unlist(number.combinations),
      specificity.increase = unlist(specificity.increase)
    ) %>% 
    dplyr::arrange(dplyr::desc(specificity.increase))
  
  es2 <- 
    es %>% 
    dplyr::left_join(
      x %>% dplyr::select("condition", "combination", "count", 
                          "combination.size"), 
      by = c("condition", "element" = "combination")
    )
  
  esl <- 
    es2 %>% 
    split(es2$combination.size) %>% 
    stats::setNames(c("element", "dyad"))
  
  return(esl)
}

#' (Defunct) Tests how much each element increases the specificity of all
#' combinations it is in
#'
#' This function is defunct Please see \code{\link{specificity_increase}}
#' instead
#'
#' @param netfacs.data object resulting from \code{\link{netfacs}} function
#' @export
element.specificity <- function(netfacs.data) {
    .Defunct("specificity_increase")
}