#' Up sample
#'
#' Randomly up-sample the minority condition(s) to have the same number of
#' observations as the majority condition. Random samples are added to the
#' existing observations of the minority conditions
#'
#' @param x A data.frame or something coercible to one
#' @param condition A character vector the same length as 'x' denoting which
#'   condition each observation belongs to
#' @param .name A string used to name the condition column
#'
#' @return A tibble
#' @export
#' 
#' @examples
#' d <- data.frame(
#'   condition = c(rep("a", times = 7), rep("b", times = 3)),
#'   x = sample(0:1, size = 10, replace = TRUE),
#'   y = sample(0:1, size = 10, replace = TRUE)
#' )
#'
#' upsample(x = d, condition = d$condition)
upsample <- function(x,
                     condition,
                     .name = "condition") {
  max_condition <- max(table(condition))
  l <- split(tibble::as_tibble(x), condition)
  
  out <- lapply(l, function(z, max_rows = max_condition) {
    if (nrow(z) < max_rows) {
      # sample difference between max condition and current condition
      rowid <- sample(1:nrow(z),
                      size = max_rows - nrow(z),
                      replace = TRUE)
      # add sampled data
      rowid <- c(1:nrow(z), rowid)
      z <- z[rowid, , drop = FALSE]
    }
    z
  })
  dplyr::bind_rows(out, .id = .name)
}
