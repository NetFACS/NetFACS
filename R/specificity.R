#' Specificity
#'
#' Calculate specificity of element combinations to a given condition
#'
#' @param x A binary matrix, with AUs as \code{colnames}, or an object of class
#'   \code{\link{netfacs}}
#' @param condition A character condition vector
#' @param test.condition A string, denoting the test condition. If \code{NULL}
#'   (default) specificity is calculated for all conditions.
#' @param null.condition A string, denoting the null condition. If \code{NULL}
#'   (default) all observations not part of the test.condition will be
#'   considered part of the null.
#' @param combination.size A positive integer, indicating the maximum
#'   combination size of element combinations. If \code{NULL} (default), the
#'   maximum combination size observed in the x is used.
#' @param upsample Logical. Should minority condition(s) be
#'   \code{\link{upsample}}d? \code{TRUE} by default.
#'
#' @details Specificity values are biased when the number of observations per
#'   condition is highly imbalanced. When \code{upsample} = \code{TRUE}
#'   (recommended), the condition(s) with fewer observations are randomly
#'   \code{\link{upsample}}d to match the number of observations in the most
#'   common condition prior to the specificity calculation. This procedure
#'   minimizes the bias in the specificity results.
#'
#' @return A data frame
#' @export
#' 
#' @importFrom magrittr `%>%`
#'
#' @examples
#' specificity(
#'   x = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger"
#' )
specificity <- function(x,
                        condition,
                        test.condition = NULL,
                        null.condition = NULL,
                        combination.size = NULL,
                        upsample = TRUE) {
  UseMethod("specificity")
}

#' @export
specificity.matrix <- function(x,
                               condition,
                               test.condition = NULL,
                               null.condition = NULL,
                               combination.size = NULL,
                               upsample = TRUE) {
  stopifnot(vctrs::vec_size(x) == vctrs::vec_size(condition))
  
  max_comb_size <- min(max(Rfast::rowsums(x)), combination.size)
  
  if (upsample) {
    # probability of combination in original data
    if (is.null(test.condition)) {
      conditions <- as.character(sort(unique(condition)))
      
      comb_prob <- 
        lapply(conditions, function(cond) {
          test_m <- x[condition == cond, , drop = FALSE]
          probability_of_combination(test_m, maxlen = max_comb_size)
        }) %>% 
        stats::setNames(conditions) %>% 
        dplyr::bind_rows(.id = "condition") 
      
    } else {
      test_m <- x[condition == test.condition, , drop = FALSE]
      comb_prob <- 
        probability_of_combination(test_m, maxlen = max_comb_size) %>% 
        dplyr::mutate(condition = test.condition, .before = 1)
    }
    
    # upsample minority condition(s) to have same number of observations as majority condition
    d <- upsample(x, condition)  
    
    # specificity on upsampled data
    sp <- specificity_default(d, 
                              condition,
                              test.condition, 
                              null.condition, 
                              max_comb_size)
    
    out <- 
      sp %>% 
      dplyr::select(-c("count", "observed.prob")) %>% 
      dplyr::left_join(comb_prob, by = c("condition", "combination")) %>% 
      dplyr::select("condition", "combination", "count", "observed.prob", 
                    "specificity", "combination.size") %>% 
      dplyr::arrange(
        .data$condition, .data$combination.size, dplyr::desc(.data$count)
      ) 
    
  } else {
    # calculate specificity on original data
    d <- dplyr::bind_cols(condition = condition, x)
    out <- specificity_default(d, 
                               condition, 
                               test.condition, 
                               null.condition, 
                               max_comb_size) %>% 
      dplyr::arrange(
        .data$condition, .data$combination.size, dplyr::desc(.data$count)
      )
  }
  
  class(out) <- c("netfacs_specificity", class(out))
  
  return(out)
}


#' @export
specificity.netfacs <- function(x,
                                condition = NULL,
                                test.condition = NULL,
                                null.condition = NULL,
                                combination.size = NULL,
                                upsample = TRUE) {
  # if test data was compared against random, specificity is meaningless
  if (attr(x, "stat_method") == "permutation") {
    return(print(
      "Specificity can only be calculated when a null condition is specified."
    ))
  }
  
  nf_res <- netfacs_extract(x)
  m <- get_data(x, condition = "all")
  condition <- x$used.data$condition
  test.cond <- x$used.parameters$test.condition
  null.cond <- x$used.parameters$null.condition
  
  sp <- 
    specificity.matrix(
      m, condition, test.cond, null.cond, combination.size, upsample
    )
  
  sp2 <- 
    sp %>% 
    dplyr::select("combination", "specificity")
  
  out <- 
    nf_res %>% 
    dplyr::left_join(sp2, by = "combination") %>% 
    dplyr::mutate(dplyr::across("specificity", ~ifelse(is.na(.), 0, .))) %>% 
    mutate(condition = test.cond, .before = 1)
  
  class(out) <- c("netfacs_specificity", class(out))
  return(out)
}

#' @export
specificity.netfacs_multiple <- function(x,
                                         condition = NULL,
                                         test.condition = NULL,
                                         null.condition = NULL,
                                         combination.size = NULL,
                                         upsample = TRUE) {
  # if test data was compared against random, specificity is meaningless
  if (attr(x, "stat_method") == "permutation") {
    return(
      print(
        "Specificity can only be calculated when a null condition is specified."
      )
    )
  }
  
  out <- lapply(x, function(nf) {
    specificity.netfacs(
      nf, condition, test.condition, null.condition, combination.size, upsample
    )
  })
  
  dplyr::bind_rows(out, .id = "condition")
}


# helpers -----------------------------------------------------------------

specificity_default <- function(d,
                                condition,
                                test.condition = NULL,
                                null.condition = NULL,
                                combination.size = NULL) {
  
  if (is.null(test.condition)) {
    res <- specificity_multiple(d, 
                                condition, 
                                null.condition,
                                combination.size)
    
  } else {
    res <-specificity_single(d, 
                             test.condition, 
                             null.condition, 
                             combination.size)
  }
  return(res)
}

specificity_single <- function(d,
                               test.condition,
                               null.condition = NULL,
                               combination.size = NULL) {
  
  # if null condition is not supplied, test is compared to all other conditions
  if (is.null(null.condition)) {
    null.condition <- "all"

    d <-
      d %>%
      dplyr::mutate(across(
        "condition",
        ~ifelse(. == test.condition, test.condition, null.condition)
      ))
  }
  
  d2 <-
    d %>%
    dplyr::filter(.data$condition %in% c(test.condition, null.condition))
  
  d3 <-
    d2 %>%
    tidyr::nest(m = -"condition") %>%
    dplyr::mutate(m = lapply(.data$m, as.matrix))
  
  d4 <-
    d3 %>%
    dplyr::mutate(
      rs = mapply(
        FUN = probability_of_combination,
        elements = .data$m,
        maxlen = combination.size,
        SIMPLIFY = FALSE
      )
    )
  
  rs.test <-
    d4 %>%
    dplyr::filter(.data$condition == test.condition) %>%
    dplyr::select(c("condition", "rs")) %>%
    tidyr::unnest("rs")
  
  rs.null <-
    d4 %>%
    dplyr::filter(.data$condition == null.condition) %>%
    dplyr::select(c("rs")) %>%
    tidyr::unnest("rs") %>%
    dplyr::select("combination", "count") %>% 
    dplyr::rename(null_count = "count")
  
  rs <-
    rs.test %>%
    dplyr::left_join(rs.null, by = "combination") %>%
    dplyr::mutate(dplyr::across("null_count", ~ifelse(is.na(.), 0, .))) %>%
    dplyr::mutate(
      specificity = .data$count / (.data$count + .data$null_count),
      combination.size = calculate_combination_size(.data$combination)
    ) %>% 
    dplyr::select("condition", "combination", "count", "observed.prob", 
                  "specificity", "combination.size") %>% 
    dplyr::arrange(dplyr::desc(.data$count)) 
  
  return(rs)
}

specificity_multiple <- function(d,
                                 condition,
                                 null.condition = NULL,
                                 combination.size = NULL) {
  
  conditions <- as.character(sort(unique(condition)))
  
  out <- lapply(conditions, function(x) {
    xx <- specificity_single(
      d = d,
      test.condition = x,
      null.condition = null.condition,
      combination.size = combination.size
    )
    return(xx)
  })
  dplyr::bind_rows(out)
}