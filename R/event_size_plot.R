#' Plots the probability that a combination of a certain size appears
#'
#' The function takes all combination size in a netfacs object, and plots the distribution of ratios between the observed value and all randomisations
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggplot showing for each combination size the observed and expected probabilities of occurrance
#'
#' @importFrom ggplot2 geom_point ylim xlab ylab theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' event.size.plot(angry.face)
event.size.plot <- function(netfacs.data) {
  # extract event size information from netfacs object
  plot.netfacs <-
    netfacs.data$event.size.information

  plot.netfacs <-
    plot.netfacs[plot.netfacs$observed.prob > 0 |
      plot.netfacs$expected.prob > 0, ]

  # if comparison was with permutation, set conditions to 'all cases'
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    netfacs.data$used.parameters$test.condition <- "all cases"
  }
  if (is.null(netfacs.data$used.parameters$null.condition)) {
    netfacs.data$used.parameters$null.condition <- "random"
  }

  # make a plot
  plot.data <- data.frame(
    combination.size = c(
      plot.netfacs$combination.size,
      plot.netfacs$combination.size
    ),
    prob = c(
      plot.netfacs$expected.prob,
      plot.netfacs$observed.prob
    ),
    type = c("expected probability", "observed probability")
  )
  plot.data$type <- sort(plot.data$type)
  plot.data$combination.size <-
    as.factor(plot.data$combination.size)

  p <- ggplot(
    plot.data,
    aes(
      x = .data$combination.size,
      y = .data$prob,
      color = .data$type
    )
  ) +
    xlab("element size") +
    ylab("event size probability") +
    ggtitle(paste(
      c(
        "Comparison of event sizes between ",
        netfacs.data$used.parameters$test.condition,
        " and ",
        netfacs.data$used.parameters$null.condition
      ),
      collapse = ""
    )) +
    geom_point(size = 3, alpha = 0.7) +
    ylim(0, 1) +
    theme_bw()

  return(p)
}
