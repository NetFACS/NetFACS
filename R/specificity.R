#' Specificity
#'
#' Calculate specificity of element combinations to a given condition
#'
#' @param x A binary matrix, with AUs as \code{colnames()}
#' @param condition A character condition vector
#' @param test_condition A string, denoting the test condition
#' @param null_condition A string, denoting the null condition. If \code{NULL}
#'   (default) all observations not part of the test_condition will be
#'   considered part of the null.
#' @param balance_conditions Logical. Should minority condition(s) be
#'   \link{upsample}d?
#'
#' @return Tibble
#' @export
#' 
#' @importFrom magrittr `%>%`
#'
#' @examples
#' specificity(
#'   x = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test_condition = "anger"
#' )
specificity <- function(x,
                        condition,
                        test_condition = NULL,
                        null_condition = NULL,
                        balance_conditions = TRUE) {
  UseMethod("specificity")
}

#' @export
specificity.matrix <- function(x,
                               condition,
                               test_condition = NULL,
                               null_condition = NULL,
                               balance_conditions = TRUE) {
  stopifnot(vctrs::vec_size(x) == vctrs::vec_size(condition))
  
  # upsample minority condition(s) to have same number of observations as majority condition
  if (balance_conditions) {
    d <- upsample(x, condition)  
    x <- d[colnames(x)]
    condition <- d$condition
  }
  
  if (is.null(test_condition)) {
    res <- specificity_multiple(x, condition)
    
  } else {
    res <-
      specificity_single(
        x,
        condition,
        test_condition,
        null_condition
      )
  }
  return(res)
}


#' @export
specificity.netfacs <- function(x, 
                                balance_conditions = TRUE) {
  # if test data was compared against random, specificity is meaningless
  if (attr(x, "stat_method") == "permutation") {
    return(
      print(
        "Specificity can only be calculated when a null condition is specified."
      )
    )
  }
  m <- get_data(x, condition = "all")
  condition <- x$used.data$condition
  test_condition <- x$used.parameters$test.condition
  null_condition <- x$used.parameters$null.condition
  if (null_condition == "all") {
    null_condition <- NULL
  }
  
 if (balance_conditions) {
    d <- upsample(m, condition)  
    x <- d[colnames(m)]
    condition <- d$condition
  }
  
  specificity_single(
    x,
    condition,
    test_condition,
    null_condition
  )
}

#' @export
specificity.netfacs_multiple <- function(x, 
                                         balance_conditions = TRUE) {
  # if test data was compared against random, specificity is meaningless
  if (attr(x, "stat_method") == "permutation") {
    return(
      print(
        "Specificity can only be calculated when a null condition is specified."
      )
    )
  }
  
  out <- lapply(x, function(nf) {
    specificity.netfacs(nf, balance_conditions)
  })
  
  dplyr::bind_rows(out)
}


# helpers -----------------------------------------------------------------

specificity_single <- function(x,
                               condition,
                               test_condition,
                               null_condition = NULL) {
  # if null condition is not supplied, test is compared to all other conditions
  if (is.null(null_condition)) {
    null_condition <- "all"
    condition <-
      ifelse(condition == test_condition, test_condition, null_condition)
  }
  
  d <-
    x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(condition = condition, .before = 1) %>%
    dplyr::filter(condition %in% c(test_condition, null_condition))
  
  d2 <-
    d %>%
    tidyr::nest(m = -condition) %>%
    dplyr::mutate(m = lapply(.data$m, as.matrix))
  
  d3 <-
    d2 %>%
    dplyr::mutate(
      max_comb_size = sapply(.data$m, function(x) max(Rfast::rowsums(x))),
      active_elements = lapply(.data$m, get_active_elements)
    )
  
  d4 <-
    d3 %>%
    dplyr::mutate(
      rs = mapply(
        FUN = probability_of_combination,
        elements = .data$active_elements,
        maxlen = .data$max_comb_size,
        SIMPLIFY = FALSE
      )
    )
  
  rs.test <-
    d4 %>%
    dplyr::filter(.data$condition == test_condition) %>%
    dplyr::select(c("condition", "rs")) %>%
    tidyr::unnest("rs") %>%
    dplyr::rename(test_count = "count")
  
  rs.null <-
    d4 %>%
    dplyr::filter(.data$condition == null_condition) %>%
    dplyr::select(c("condition", "rs")) %>%
    tidyr::unnest("rs") %>%
    dplyr::rename(null_count = "count")
  
  rt <-
    rs.test %>%
    dplyr::left_join(
      rs.null %>% dplyr::select("combination", "null_count"),
      by = "combination"
    ) %>%
    dplyr::mutate(dplyr::across("null_count", ~ifelse(is.na(.), 0, .))) %>%
    dplyr::mutate(
      specificity = .data$test_count / (.data$test_count + .data$null_count),
      combination_size = calculate_combination_size(.data$combination)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$test_count)) %>%
    dplyr::relocate("condition")
  
  return(rt)
}

specificity_multiple <- function(x,
                                 condition) {
  
  conditions <- as.character(sort(unique(condition)))
  
  out <- lapply(conditions, function(tc) {
    xx <- specificity_single(
      x = x,
      condition = condition,
      test_condition = tc
    )
    return(xx)
  })
  dplyr::bind_rows(out)
}

