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
#' @param netfacs.data object resulting from \code{\link{netfacs}} function
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
#' \donttest{
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   null.condition = NULL,
#'   ran.trials = 100,
#'   combination.size = 4
#' )
#'
#' element.specificity(angry.face)$element
#' }
#' 

element.specificity <- function(netfacs.data) {
  # if test data was compared against random, specificity is meaningless
  if (attr(netfacs.data, "stat_method") == "permutation") {
    return(
      print(
        "Results are not part of comparison and do not reveal anything about specificity."
      )
    )
  }
  
  d <- netfacs_extract(netfacs.data)
  ed <- 
    netfacs.data %>% 
    netfacs_extract(combination.size = 1:2, 
                    min.count = 1)
  
  # make list of all possible
  all.combinations <-
    lapply(d$combination, function(x) {
      unlist(strsplit(as.character(x), split = "_", fixed = TRUE))
    })
  ed.combinations <- 
    lapply(ed$combination, function(x) {
      unlist(strsplit(as.character(x), split = "_", fixed = TRUE))
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
      elements.with <- d[combinations.i, ]
      combinations.with <- all.combinations[combinations.i]
      
      specificity.with <- elements.with$specificity
      
      combinations.without <-
        lapply(combinations.with, function(x) {
          xx <- x[!x %in% t.comb] 
          if (length(xx) > 1) {
            xx <- paste(xx, collapse = "_")
          }
          return(xx)
        })
      
      specificity.without <-
        d$specificity[d$combination %in% unlist(combinations.without)]
      
      spec.increase <- 
        mean(specificity.with, na.rm = T) - mean(specificity.without, na.rm = T)
      
      return(spec.increase)
    })
  
  # create new dataframe only for those elements
  es <-
    tibble::tibble(
      element = ed$combination,
      number.combinations = unlist(number.combinations),
      specificity.increase = unlist(specificity.increase)
    ) %>% 
    dplyr::arrange(dplyr::desc(specificity.increase))
  
  es2 <- 
    es %>% 
    dplyr::left_join(
      d %>% dplyr::select("combination", "count", "combination.size"), 
      by = c("element" = "combination")
    )
  
  esl <- 
    es2 %>% 
    split(es2$combination.size) %>% 
    stats::setNames(c("element", "dyad"))
  
  return(esl)
}
