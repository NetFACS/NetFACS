#' Plots the observed and expected probabilities for the basic elements based on the condition
#'
#' The function takes all single elements in a netfacs object, and plots the observed value and the expected value based on all randomisations
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggplot showing for each element the observed probability and expected probability
#'
#' @importFrom ggplot2 geom_point ylim xlab ylab theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
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
#' # plot all
#' element.plot(netfacs.data = angry.face)
element.plot <- function(netfacs.data) {
  # take only the result part of the netfacs object
  plot.netfacs <- netfacs.data$result

  # reduce to single elements
  plot.netfacs <- plot.netfacs[plot.netfacs$combination.size == 1, ]

  # random probabilities
  random.prob <- netfacs.data$used.data$random.prob
  # random.prob[random.prob == 0] <- NA

  # create title and significance stars
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    netfacs.data$used.parameters$test.condition <- "all cases"
  }
  if (is.null(netfacs.data$used.parameters$null.condition)) {
    netfacs.data$used.parameters$null.condition <- "random"
  }
  plot.netfacs$star <- ""
  plot.netfacs$star[plot.netfacs$pvalue <= 0.01 &
    plot.netfacs$observed.prob >= plot.netfacs$expected.prob] <-
    "*"


  plot.data <- data.frame(
    combination = c(plot.netfacs$combination, plot.netfacs$combination),
    prob = c(
      plot.netfacs$expected.prob,
      plot.netfacs$observed.prob
    ),
    type = c("expected probability", "observed probability")
  )
  plot.data$type <- sort(plot.data$type)

  # create plot
  p <- ggplot(
    plot.data,
    aes(
      x = .data$combination,
      y = .data$prob,
      color = .data$type
    )
  ) +
    xlab("element") +
    ylab("element probability") +
    annotate(
      "text",
      x = plot.netfacs$combination,
      y = 1.05,
      label = plot.netfacs$star,
      size = 8
    ) +
    geom_point(size = 3, na.rm = TRUE, alpha = 0.7) +
    ggtitle(paste(
      c(
        "Comparison of ",
        netfacs.data$used.parameters$test.condition,
        " and ",
        netfacs.data$used.parameters$null.condition
      ),
      collapse = ""
    )) +
    ylim(-0.01, 1.05) +
    theme_bw()

  return(p)
}
