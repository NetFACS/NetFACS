#' Plots the probability that a combination of a certain size appears
#'
#' The function takes all combination size in a netfacs object, and plots the
#' distribution of ratios between the observed value and all randomisations
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggplot showing for each combination size the
#'   observed and expected probabilities of occurrence
#'
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom ggplot2 geom_point ylim xlab ylab theme_bw
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggtitle
#' @importFrom magrittr %>%
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
#' event_size_plot(angry.face)
event_size_plot <- function(netfacs.data) {
  # extract event size information from netfacs object
  plot.netfacs <-
    netfacs.data$event.size.information

  # if comparison was with permutation, set conditions to 'all cases'
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    netfacs.data$used.parameters$test.condition <- "all cases"
  }
  if (is.null(netfacs.data$used.parameters$null.condition)) {
    netfacs.data$used.parameters$null.condition <- "random"
  }

  # make a plot
  plot.data <-
    plot.netfacs %>%
    filter(.data$observed.prob > 0 | .data$expected.prob > 0) %>%
    select(.data$combination.size, .data$observed.prob, .data$expected.prob) %>%
    tidyr::pivot_longer(c("observed.prob", "expected.prob"),
                        names_to = "type", values_to = "prob") %>%
    mutate(
      across(.data$type, 
             ~ifelse(. == "expected.prob",
                     "expected probability", "observed probability"))) %>% 
    mutate(
      across(.data$combination.size, 
             ~factor(., 
                     levels = sort(unique(as.numeric(.data$combination.size))))))
  
  p <- ggplot(
    plot.data,
    aes(x = .data$combination.size,
        y = .data$prob,
        color = .data$type)) +
    xlab("element size") +
    ylab("event size probability") +
    ggtitle(paste(
      c(
        "Comparison of event sizes between ",
        netfacs.data$used.parameters$test.condition,
        " and ",
        netfacs.data$used.parameters$null.condition
      ),
      collapse = "")) +
    geom_point(size = 3, alpha = 0.7) +
    ylim(0, 1) +
    theme_bw()

  return(p)
}

#' @rdname event_size_plot
#' @export
event.size.plot <- function(netfacs.data) {
  event_size_plot(netfacs.data)
}