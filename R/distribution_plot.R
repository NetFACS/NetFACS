#' Plots the observed probability for an element against the distribution of the null model
#'
#' The function takes all single elements in a netfacs object, and plots the distribution of probabilities under the null hypothesis, marking where the observed probability falls
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggplot showing for each element the distribution of expected probabilities (blue) and the observed probability (black line)
#'
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
#'
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
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' # show distribution of AU4
#' distribution.plot(netfacs.data = angry.face)$"4"

distribution.plot <- function(netfacs.data) {

  # take only the result part of the netfacs object
  plot.netfacs <- netfacs.data$result
  plot.random <- netfacs.data$used.data$random.prob

  # reduce to single elements
  plot.random <- plot.random[plot.netfacs$combination.size == 1, ]
  plot.netfacs <- plot.netfacs[plot.netfacs$combination.size == 1, ]

  # make one plot per single element
  distribution.plots <- lapply(1:nrow(plot.netfacs), function(y) {
    x.data <- plot.netfacs[y, ]
    ran.data <- data.frame(ran = plot.random[y, ])
    xx.plot <- ggplot(
      data = ran.data,
      mapping = aes(x = .data$ran)
    ) +
      geom_density(color = "black", fill = "lightblue") +
      xlim(0, max(c(plot.random, plot.netfacs$observed.prob)) + 0.05) +
      theme_bw() +
      geom_vline(xintercept = x.data$observed.prob, size = 1.2) +
      ggtitle(x.data$combination)
    return(xx.plot)
  })

  names(distribution.plots) <- plot.netfacs$combination

  return(distribution.plots)
}
