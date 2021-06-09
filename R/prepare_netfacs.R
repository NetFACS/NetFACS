#' Take data that are not currently in format and turn them into the correct format for netfacs function
#'
#' The \code{\link{netfacs}} function requires data to be entered with the element data as a matrix of each element by each event,
#' with occurrence marked as 1 and non-occurrence marked as 0.\cr
#' This is often not the case, so this function transforms data in other routine formats to have the right look.\cr
#' Specifically, users can define whether they want to enter 'photos', which indicates that all elements in an event are simply strung together in a vector; or they define 'video', in which case it is assumed that each element has a start and an end point in a specified video
#'
#'
#' @param elements vector with either one element per index (for videos) or all elements that occurred in the whole event (for photos)
#' @param type either 'video' or 'photo'. If 'photo', the function separates the string and returns a matrix of the correct dimensions. If 'video', the function creates a matrix using the highest common factor of all 'durations' and for each of those 'frames' assigns whether each element was present or absent
#' @param video.id name of the video, so all cases are treated together. For photos, can be entered so that photos can be matched to IDs after
#' @param start.time for videos, time when the element is first active
#' @param duration for videos, how long is the element active for
#' @param separator for photos, how are elements separated in the list
#' @param frame.duration for videos, how long is a 'frame' supposed to last? If NULL, frame duration is the shortest 'duration' of any element specified
#'
#' @details The assumption for this function is that for photos, elements are stored like this: \cr
#' 'AU1/AU2/AU3/AU4'\cr
#' 'AU1/AU3/AU4'\cr
#' 'AU1/AU2'\cr
#' \cr
#' For videos, the assumption is that they are stored in a data frame like this: \cr
#' element = AU1, video.id = 1, start.time = 0.5, duration = 2sec
#'
#' @return Function returns a list with element.matrix (the matrix of elements and when they occurred) and video.info (the supporting information, e.g. video names, durations, frames etc)
#'
#' @export
#'
#' @examples
#' # for a photo
#' au.photos <- c(
#'   "AU1/AU5/AU9",
#'   "AU1/AU2",
#'   "AU1/AU2/AU10",
#'   "AU1/AU2",
#'   "AU5/AU17/AU18",
#'   "AU6/AU12"
#' )
#' au.names <- c("photo1", "photo2", "photo3", "photo4", "photo5", "photo6")
#' au.prepared <- prepare.netfacs(
#'   elements = au.photos,
#'   type = "photo",
#'   video.id = au.names,
#'   separator = "/"
#' )
#' au.prepared$element.matrix
#' au.prepared$video.info
#'
#' # for a video
#' aus <- c(
#'   "AU1", "AU5", "AU9",
#'   "AU1", "AU2",
#'   "AU1", "AU2", "AU10",
#'   "AU1", "AU2",
#'   "AU5", "AU17", "AU18",
#'   "AU6", "AU12"
#' )
#' video.names <- c(
#'   rep("video1", 3),
#'   rep("video2", 2),
#'   rep("video3", 3),
#'   rep("video4", 2),
#'   rep("video5", 3),
#'   rep("video6", 2)
#' )
#' start.times <- c(
#'   0.1, 0.2, 0.3,
#'   0.1, 0.3,
#'   0.1, 0.4, 0.4,
#'   0.1, 0.2,
#'   0.1, 0.5, 0.6,
#'   0.1, 0.2
#' )
#' durations <- rep(0.3, times = length(start.times))
#' frame.dur <- 0.05
#' au.prepared <- prepare.netfacs(
#'   elements = aus,
#'   type = "video",
#'   video.id = video.names,
#'   start.time = start.times,
#'   duration = durations,
#'   frame.duration = frame.dur
#' )
#' head(au.prepared$element.matrix)
#' head(au.prepared$video.info)
prepare.netfacs <- function(elements, type = c("video", "photo"), video.id = NULL, start.time = NULL, duration = NULL, separator = ",", frame.duration = NULL) {

  # first, for videos
  if (type == "video") {
    # create list of all elements
    elements.list <- sort(unique(elements))
    # create empty objects to store info later
    net.set <- c()
    net.info <- c()

    # set how long one 'frame' in the video is meant to be. If not specified, use shortest duration in data
    if (is.null(frame.duration)) {
      frame.dur <- min(duration)
    }
    if (!is.null(frame.duration)) {
      frame.dur <- frame.duration
    }

    # go through each video
    for (i in 1:length(unique(video.id))) {
      # id of video
      id <- unique(video.id)[i]
      # which elements go into this video
      frames <- which(video.id == id)

      # start and end point of video
      start.vid <- min(start.time[video.id == id])
      end.vid <- start.time[max(frames)] + duration[max(frames)]

      # make sequence from start to end
      frame.starts <- seq(start.vid, end.vid - frame.dur, by = frame.dur)

      # create matrices for info and elements of the same length as the sequence
      net.matr <- data.frame(matrix(0, nrow = length(frame.starts), ncol = length(elements.list)))
      info.matr <- data.frame(matrix(0, nrow = length(frame.starts), ncol = 4))
      colnames(net.matr) <- sort(as.character(elements.list))
      colnames(info.matr) <- c("video.id", "start.sec", "end.sec", "duration")

      # set info in info matrix
      info.matr$video.id <- id
      info.matr$start.sec <- frame.starts
      info.matr$end.sec <- frame.starts + frame.dur
      info.matr$duration <- frame.dur

      # go through each specified element and add info to element matrix
      elements.vid <- elements[frames]
      start.frames <- start.time[frames]
      dur.frames <- duration[frames]
      end.frames <- start.frames + dur.frames
      for (j in 1:length(elements.vid)) {
        net.matr[which(info.matr$start.sec >= start.frames[j] & info.matr$end.sec <= end.frames[j]), elements.vid[j]] <- 1
      }

      # add to overall frames
      net.set <- rbind(net.set, net.matr)
      net.info <- rbind(net.info, info.matr)
    }
    net.list <- list(element.matrix = net.set, video.info = net.info)
  }

  # then, for photos
  if (type == "photo") {
    # create list of all elements
    elements.list <- sort(unique(unlist(strsplit(elements, split = separator, fixed = T))))
    # create empty objects to store info later
    net.set <- c()
    net.info <- c()

    # if video id is specified, keep those names; otherwise, just number
    if (is.null(video.id)) {
      video.id <- 1:length(elements)
    }
    if (!is.null(video.id)) {
      video.id <- video.id
    }

    # go through each video
    for (i in 1:length(elements)) {
      # id of video
      elements.photo <- sort(unique(unlist(strsplit(elements[i], split = separator, fixed = T))))

      net.matr <- data.frame(matrix(0, nrow = 1, ncol = length(elements.list)))
      info.matr <- data.frame(matrix(0, nrow = 1, ncol = 4))
      colnames(net.matr) <- sort(as.character(elements.list))
      colnames(info.matr) <- c("video.id", "start.sec", "end.sec", "duration")

      # set info in info matrix
      info.matr$video.id <- video.id[i]
      info.matr$start.sec <- 0
      info.matr$end.sec <- 0
      info.matr$duration <- 0

      net.matr[1, elements.photo] <- 1

      # add to overall frames
      net.set <- rbind(net.set, net.matr)
      net.info <- rbind(net.info, info.matr)
    }
    net.list <- list(element.matrix = net.set, video.info = net.info)
  }


  return(net.list)
}
