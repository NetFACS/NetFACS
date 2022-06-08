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
  # create dataset
  data <- netfacs.data$result

  # if no specificity exists (i.e. the test data was compared against random), show error message
  if (!"specificity" %in% colnames(data)) {
    return(
      print(
        "Results are not part of comparison and do not reveal anything about specificity."
      )
    )
  }

  # select only elements and first order combinations
  xx <-
    data$combination[data$combination.size %in% c(1, 2) &
      data$observed.prob > 0]

  # create new dataframe only for those elements
  element.specificity <-
    data.frame(
      element = xx,
      number.combinations = 0,
      specificity.increase = 0
    )
  rownames(element.specificity) <- xx

  # make list of all possible
  all.combinations <-
    lapply(data$combination, function(x) {
      unlist(strsplit(as.character(x), split = "_", fixed = T))
    })

  # for each element and first order combination, count how many combinations this one is part of
  ii <- lapply(rownames(element.specificity), function(i) {
    x.i <- unlist(strsplit(as.character(i), split = "_", fixed = T))
    elements.with <-
      data[unlist(lapply(all.combinations, function(z) {
        length(intersect(x.i, unlist(z))) == length(x.i)
      })), ]
    combinations.with <-
      all.combinations[unlist(lapply(all.combinations, function(z) {
        length(intersect(x.i, unlist(z))) == length(x.i)
      }))]
    return(length(combinations.with))
  })
  element.specificity$number.combinations <- unlist(ii)

  # calculate for each element/combination the specificity of all combinations that have this element/combination, and those which are composed of all the same other elements minus the one in question
  ii <- lapply(rownames(element.specificity), function(i) {
    x.i <- unlist(strsplit(as.character(i), split = "_", fixed = T))
    elements.with <-
      data[unlist(lapply(all.combinations, function(z) {
        length(intersect(x.i, unlist(z))) == length(x.i)
      })), ]
    combinations.with <-
      all.combinations[unlist(lapply(all.combinations, function(z) {
        length(intersect(x.i, unlist(z))) == length(x.i)
      }))]
    specificity.with <- elements.with$specificity
    combinations.without <- sapply(combinations.with, function(x) {
      xx <- x[!x %in% x.i]
      if (length(xx) > 1) {
        xx <- paste(xx, collapse = "_")
      }
      return(xx)
    })
    specificity.without <-
      data$specificity[data$combination %in% unlist(combinations.without)]
    return(mean(specificity.with, na.rm = T) - mean(specificity.without, na.rm = T))
  })
  element.specificity$specificity.increase <- unlist(ii)

  # order by increase in specificity
  element.specificity <-
    element.specificity[order(-1 * element.specificity$specificity.increase), ]
  element.specificity$count <-
    data$count[match(element.specificity$element, data$combination)]

  combinations <-
    grepl(element.specificity$element,
      pattern = "_",
      fixed = T
    ) # this is used to differentiate between single elements and combinations

  element.specificity <-
    list(element = element.specificity[!combinations, ], dyad = element.specificity[combinations, ])

  return(element.specificity)
}
