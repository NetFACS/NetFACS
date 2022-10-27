#' Calculate reciprocity of probabilities that two elements appear together
#'
#' For all dyadic combinations that ever appear, this function calculates how
#' reciprocal the conditional probabilities (i.e. probability of A given B, and
#' B given A) of the two elements are. Combinations that are highly reciprocal
#' indicate that the two elements always occur together and might represent a
#' fixed combination, while low reciprocity might indicate that one element is
#' an extension of the other. Values approaching -1 indicate that one element is
#' strongly dependent on the other, but this is not reciprocated; values around
#' 0 indicate that neither is conditional on the other; and values approaching 1
#' indicate that both values are conditional on each other. If P[A|B] is the
#' larger conditional probability, the reciprocity is calculated as reciprocity
#' = ((P[B|A]/P[A|B]) - (P[A|B] - P[B|A])) * P[A|B].
#'
#'
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a data frame with each combination, the reciprocity
#'   of conditional occurrence from -1 (one element entirely depends on the
#'   other, but not vice versa) to 1 (both elements always occur together)
#' @return The directions and conditional probabilities of both elements are
#'   also returned
#' @export
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
#' netfacs.reciprocity(angry.face)
netfacs.reciprocity <- function(netfacs.data) {
  # extract conditional probabilities from arules object in netfacs object
  if(is.null(netfacs.data$used.parameters$test.condition)){netfacs.data$used.parameters$test.condition = 'all'}
  if(is.null(netfacs.data$used.data$condition)){netfacs.data$used.data$condition = rep('all', nrow(netfacs.data$used.data$data))}
  test.data <- netfacs.data$used.data$data[netfacs.data$used.data$condition == netfacs.data$used.parameters$test.condition, ]
  conditional_foo <- function(indata) {
    # indata is the raw data matrix (either all events or only test condition)
    # pairs of units
    xpairs <- utils::combn(colnames(indata), 2)
    # PA and PB
    pa_and_pb <-
      apply(xpairs, 2, function(x) {
        colSums(indata[, x])
      }) / nrow(indata)
    # PAB
    pab <-
      apply(xpairs, 2, function(x) {
        sum(rowSums(indata[, x]) == 2)
      }) / nrow(indata)
    # PA|B
    pa_b <-
      apply(xpairs, 2, function(x) {
        sum(rowSums(indata[, x]) == 2) / sum(indata[, x[2]])
      })
    # PB|A
    pb_a <-
      apply(xpairs, 2, function(x) {
        sum(rowSums(indata[, x]) == 2) / sum(indata[, x[1]])
      })

    # put together
    out <- data.frame(
      elementA = as.character(xpairs),
      elementB = as.character(xpairs[2:1, ])
    )
    out$combination <-
      rep(apply(xpairs, 2, paste, collapse = "_"), each = 2)
    out$count <- rep(pab * nrow(indata), each = 2)
    out$Probability_A <- as.numeric(pa_and_pb)
    out$Probability_B <- as.numeric(pa_and_pb[2:1, ])
    out$Probability_AandB <- rep(pab, each = 2)
    out$Probability_AgivenB <- as.numeric(rbind(pa_b, pb_a))

    out <- out[out$count > 0, ]
    out[order(out$count), ]
  }

  rs <- conditional_foo(test.data)
  reci <- rs[order(-1 * (rs$count), rs$combination), ]

  reci <- lapply(unique(reci$combination), function(x) {
    xx <- reci[reci$combination == x, ]
    xx.reci <- as.numeric(xx$Probability_AgivenB)
    reci.res <- ((min(xx.reci) / max(xx.reci)) - (max(xx.reci) - min(xx.reci))) * max(xx.reci)
    xx.reci <- data.frame(
      combination = x,
      reciprocity = as.numeric(reci.res),
      minimum.direction = paste(unique(c(
        xx$elementB[xx$Probability_AgivenB == min(xx.reci)], xx$elementA[xx$Probability_AgivenB ==
          min(xx.reci)]
      )), collapse = "|"),
      minimum.conditional = min(xx.reci),
      maximum.direction = paste(unique(c(
        xx$elementB[xx$Probability_AgivenB == max(xx.reci)], xx$elementA[xx$Probability_AgivenB ==
          max(xx.reci)]
      )), collapse = "|"),
      maximum.conditional = max(xx.reci),
      count = reci$count[reci$combination == x][1]
    )
  })
  reci <- do.call(rbind, reci)
  reci <- data.frame(reci)
  reci$reciprocity <- round(as.numeric(as.character(reci$reciprocity)), 2)
  reci$minimum.conditional <- round(as.numeric(as.character(reci$minimum.conditional)), 2)
  reci$maximum.conditional <- round(as.numeric(as.character(reci$maximum.conditional)), 2)
  reci <- reci[order(reci$reciprocity), ]


  return(reci)
}
