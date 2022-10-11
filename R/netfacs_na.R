#' Create probability distribution of combinations of elements in the data
#'
#' *EXPERIMENTAL*
#'
#' This is an experimental function that is capable of handling NAs in the data.
#' However, the resulting object may not work with other functions in the
#' NetFACS package. Please use \code{\link{netfacs}} instead for the most
#' reliable results.
#'
#' The \code{\link{netfacs}} function underlies most other functions in this
#' package. \cr It takes the data set and reports the observed and expected
#' probabilities that elements and combinations of elements occur in this data
#' set, and whether this differs from a null condition.
#'
#' @inheritParams netfacs
#' @details Expected values are based on bootstraps of null distribution, so the
#'   values represent distribution of element co-occurrence under null
#'   condition; or permutations of the observed distribution to test it against
#'   'random'.
#'
#'   The resulting object is the basis for most other functions in this package.
#'
#' @return Function returns a Result data frame that includes the combination
#'   name, how many elements it consisted of, how often it was observed, the
#'   probability it was observed under this condition, the expected probability
#'   under null condition (based on the permutation or bootstrap), effect size
#'   (difference between observed probability and expected probability), p-value
#'   (how many randomisations were more extreme), and for direct comparisons of
#'   contexts the specificity (probability that the condition is in fact the
#'   test condition if that combination is known) and probability increase (the
#'   factor by which the probability of the element is higher in the test than
#'   null condition)
#' @return 'event.size.information' contains information about the observed and
#'   expected size of combination or elements per event based on the
#'   randomisations
#'
#' @importFrom picante randomizeMatrix
#' @importFrom arrangements combinations
#' @importFrom parallel parLapply
#' @importFrom parallel mclapply
#' @importFrom parallel makeCluster
#' @importFrom parallel detectCores
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom Rfast rowsums rowmeans Table
#'
#'
#' @author Alex Mielke, Alan V. Rincon
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' \donttest{
#' data(emotions_set)
#' angry.face <- netfacs_na(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   null.condition = NULL,
#'   duration = NULL,
#'   ran.trials = 100,
#'   control = NULL,
#'   random.level = NULL,
#'   combination.size = 5,
#'   tail = "upper.tail",
#'   use_parallel = TRUE,
#'   n_cores = 2
#' )
#'
#' head(angry.face$result, 20)
#' angry.face$event.size.information
#' }
#' 
netfacs_na <- function(data,
                       condition = NULL,
                       test.condition = NULL,
                       null.condition = NULL,
                       duration = NULL,
                       ran.trials = 1000,
                       control = NULL,
                       random.level = NULL,
                       combination.size = 2,
                       tail = "upper.tail",
                       use_parallel = TRUE,
                       n_cores = 2) {
  
  warning("This is an experimental function that handles NAs in the data. The resulting object may not work with other functions in the NetFACS package. Please use the function 'netfacs()' for the most reliable results.", call. = FALSE, immediate. = TRUE)
  
  # validate data passed by user
  data <- validate_data(data)
  validate_condition(data, condition, test.condition, null.condition)
  
  # format data
  data <- apply(data, 2, as.numeric)
  # clean names of dataset to not include special characters or spaces
  colnames(data) <- gsub(
    colnames(data),
    pattern = "[^[:alnum:]]",
    replacement = ""
  )
  
  # elements will later be split and combined using "_", and therefore element names must not contain "_"
  if (any(grepl(pattern = "_", colnames(data)))) {
    stop("Column names of data must not contain '_'.", call. = FALSE)
  }
  
  # clean names of dataset to not include special characters or spaces
  colnames(data) <- gsub(
    colnames(data),
    pattern = "[^[:alnum:]]",
    replacement = ""
  )
  
  # determine number of cores for parallelization
  if (isTRUE(use_parallel) & is.null(n_cores)) {
    n_cores <- detectCores() - 1
  }
  if (isFALSE(use_parallel) & is.null(n_cores)) {
    n_cores <- 1
  }
  if (n_cores <= 1) {
    use_parallel <- FALSE
  }
  if (ran.trials <= 100) {
    use_parallel <- FALSE
  }
  
  # if combination.size is not determined, the maximum combination size that is considered is set to the maximum observed combination size
  if (is.null(combination.size)) {
    combination.size <- max(Rfast::rowsums(data))
  }
  
  # condition is specified ---------------------------------------------
  
  # data preparation happens for two different cases: either 'condition' is set, in which case the 'test.condition' is tested against all other cases or against one specific 'null.condition'; alternatively, if no condition is set, the probabilities are compared with random probabilities
  if (!is.null(condition)) {
    # Error messages in case test.condition is wrongly specified
    if (length(condition) != nrow(data)) {
      stop("condition vector must be the same length as nrow(data).",
           call. = FALSE
      )
    }
    if (is.null(test.condition)) {
      stop("Specify test condition.", call. = FALSE)
    }
    if (!test.condition %in% condition) {
      stop("Test condition is not part of the condition vector.",
           call. = FALSE
      )
    }
    if (!is.null(null.condition)) {
      if (!null.condition %in% condition) {
        stop("Null condition is not part of the condition vector.",
             call. = FALSE
        )
      }
    }
    
    # if random.level is not defined, each event/row is its own case, and all events are compared against each other. If random.level is defined, the randomization will select cases based on which level they belong to
    if (is.null(random.level)) {
      random.level <- 1:nrow(data)
    }
    
    # if the null condition is not determined, all cases that are not part of the test dataset are classed as null condition
    condition <- as.character(condition)
    if (is.null(null.condition)) {
      null.condition <- "all"
      condition[condition != test.condition] <- null.condition
    }
    
    # the control argument, which is entered as a list (to allow for multiple control variables) is turned into a combination of those variables. E.g., if you control for sex and social group, then each individual or is classed as sex_group
    if (length(control) > 1) {
      if (!all(lengths(control) == length(control[[1]]))) {
        stop("Control objects must be of equal lenghts.")
      }
      control.obj <- apply(do.call(cbind, control), 1, function(k) {
        k2 <- gsub(k, pattern = "[^[:alnum:]]", replacement = "")
        paste(k2, collapse = "_")
      })
    }
    if (length(control) == 1) {
      control.obj <- unlist(control[1])
    }
    if (is.null(control)) {
      control.obj <- rep(1, nrow(data))
    }
    
    # check that arguments have same number of observations
    if (!equal_observations(data, condition, random.level, control.obj)) {
      stop("data, condition, random.level, control must have the same number of observations")
    }
    
    d <-
      list(
        data = data,
        condition = condition,
        random.level = random.level,
        control = control.obj
      )
    
    # if duration is set, all variable vectors have to be multiplied accordingly
    if (!is.null(duration)) {
      if (any(is.na(duration))) stop("Duration vector must not contain NAs")
      if (length(duration != length(d$condition))) {
        stop("Duration vector must be same length as condition")
      }
      # duration is determined by the minimum value. So, if the shortest event is 0.05s, then an even of 1sec will be represented 20 times
      min.duration <- min(duration)
      duration <- round(duration / min.duration, 0)
      
      d$data <- d$data[rep(1:nrow(d$data), times = duration), , drop = FALSE]
      
      d[c("condition", "random.level", "control")] <-
        lapply(
          d[c("condition", "random.level", "control")],
          function(x) x[rep(1:length(x), times = duration)]
        )
    }
    
    # remove empty rows
    comb.size <- Rfast::rowsums(data, na.rm = T)
    if (any(comb.size == 0)) {
      d <- lapply(d, function(x) vctrs::vec_slice(x, comb.size > 0))
      NN <- sum(comb.size == 0)
      message(paste("Removing", NN, "rows with 0 active elements from data."))
    }
    
    ### turn data into test data and null data
    d.test <- lapply(d, function(x) {
      vctrs::vec_slice(x, d$condition == test.condition)
    })
    d.null <- lapply(d, function(x) {
      vctrs::vec_slice(x, d$condition == null.condition)
    })
    
    # create ratio for randomisation of null dataset: if the test dataset contains 12 males and 15 females (ratio of 1.25), then the selection of the null dataset should reflect this
    rl.test.count <- Rfast::Table(d.test$control[!duplicated(d.test$random.level)])
    rl.test.ratio <- rl.test.count / sum(rl.test.count) # ratio for test data
    # use ratio of test data as weight for bootstrapping random level of null data
    rl.null <- d.null$random.level[!duplicated(d.null$random.level)]
    ctrl.null <- d.null$control[!duplicated(d.null$random.level)]
    rl.null.weight <- rl.test.ratio[match(ctrl.null, names(rl.test.ratio))]
    
    rl.null.ratio <-
      data.frame(rl = rl.null, control = ctrl.null, weight = rl.null.weight)
    
    if (any(is.na(rl.null.ratio$weight))) {
      excluded <- rl.null.ratio[is.na(rl.null.ratio$weight), ]
      # set sample weight of missing to 0
      rl.null.ratio$weight[is.na(rl.null.ratio$weight)] <- 0
      ignored <- paste0(unique(excluded$control), collapse = ", ")
      warning("The following control values are present in null.condition but not test.condition: ", ignored, ". These observations will be ignored during the bootstrapping procedure.")
    }
    
    # create probabilities for null and test datasets
    # create a list for each event/row of the dataset, containing the names of the elements that were active
    elements.null <- get_active_elements(d.null$data)
    elements.test <- get_active_elements(d.test$data)
    
    ######## remove NAs
    elements.null <- lapply(elements.null, function(x) x[!is.na(x)])
    elements.test <- lapply(elements.test, function(x) x[!is.na(x)])
    
    # extract the probabilities for each combination of elements
    rs.null <- probability_of_combination(
      elements = elements.null,
      maxlen = combination.size
    )
    rs.test <- probability_of_combination(
      elements = elements.test,
      maxlen = combination.size
    )
    
    rs.null$combination.size <- calculate_combination_size(rs.null$combination)
    rs.test$combination.size <- calculate_combination_size(rs.test$combination)
    
    # for single units that are not active during this specific condition, add them anyways with the information that they did not show up
    rs.null <- add_inactive_single_units(rs.null,
                                         single.units = colnames(d.null$data)
    )
    rs.test <- add_inactive_single_units(rs.test,
                                         single.units = colnames(d.test$data)
    )
    
    ################## If there are NAs in the data, the following function re-calculates the number of possible events that combinations could have occurred in
    if (any(is.na(d$data))) {
      
      max_possible_count_null <- 
        count_complete_cases(rs.null$combination, d.null$data)
      
      max_possible_count_test <- 
        count_complete_cases(rs.test$combination, d.test$data)
      
      rs.null$observed.prob <- rs.null$count/max_possible_count_null
      rs.test$observed.prob <- rs.test$count/max_possible_count_test
     
    }
    # create an object with the observed event sizes
    max.event.size <- max(Rfast::rowsums(data, na.rm = TRUE), na.rm = TRUE)
    
    if (use_parallel) {
      # if on Mac or Linux, mclapply should work
      if (!(Sys.info()[["sysname"]] == "Windows") & .Platform$OS.type == "unix") {
        boot.values <-
          mclapply(1:ran.trials,
                   function(x) {
                     netfacs_bootstrap_na(
                       subject              = rl.null.ratio$rl,
                       subject.weight       = rl.null.ratio$weight,
                       null.subjects        = d.null$random.level,
                       null.elements        = elements.null,
                       test.combinations    = rs.test$combination,
                       max.combination.size = combination.size,
                       max.event.size       = max.event.size,
                       null.data            = d.null$data
                     )
                   },
                   mc.cores = n_cores
          )
      } else {
        # varlist must be a character vector of objects (not in a list)
        subject <- rl.null.ratio$rl
        subject.weight <- rl.null.ratio$weight
        null.subjects <- d.null$random.level
        test.combinations <- rs.test$combination
        d.null <- d.null$data
        
        mycluster <- parallel::makeCluster(n_cores, type = "PSOCK")
        parallel::clusterExport(
          cl = mycluster,
          varlist = c(
            "subject",
            "subject.weight",
            "null.subjects",
            "elements.null",
            "test.combinations",
            "combination.size",
            "max.event.size",
            "netfacs_bootstrap_na",
            "d.null",
            "d.test",
            "probability_of_combination",
            "possible_combinations",
            "probability_of_event_size"
          ),
          envir = environment()
        )
        doParallel::registerDoParallel(mycluster)
        boot.values <-
          parallel::parLapply(
            cl = mycluster,
            X = 1:ran.trials,
            function(x) {
              netfacs_bootstrap_na(
                subject              = subject,
                subject.weight       = subject.weight,
                null.subjects        = null.subjects,
                null.elements        = elements.null,
                test.combinations    = test.combinations,
                max.combination.size = combination.size,
                max.event.size       = max.event.size,
                null.data            = d.null
              )
            }
          )
        parallel::stopCluster(mycluster)
      }
    } else {
      # run plain loop
      boot.values <-
        lapply(
          1:ran.trials,
          function(x) {
            netfacs_bootstrap_na(
              subject              = rl.null.ratio$rl,
              subject.weight       = rl.null.ratio$weight,
              null.subjects        = d.null$random.level,
              null.elements        = elements.null,
              test.combinations    = rs.test$combination,
              max.combination.size = combination.size,
              max.event.size       = max.event.size,
              null.data            = d.null$data
            )
          }
        )
    }
    
    boot.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$combination.prob
    }))
    
    # match null count to test data
    null.count <- rs.null$count[match(rs.test$combination, rs.null$combination)]
    null.count[is.na(null.count)] <- 0
    
    res <-
      summarise_combination(
        combination = rs.test$combination,
        combination.size = rs.test$combination.size,
        observed.prob = rs.test$observed.prob,
        boot.prob = boot.prob,
        tail = tail,
        test.count = rs.test$count,
        null.count = null.count
      )
    
    ##### combination size information per event
    event.boot.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$event.size.prob
    }))
    
    event.observed.prob <-
      probability_of_event_size(elements.test, max.event.size)
    
    event.size.summary <-
      summarise_event_size(
        observed.prob = event.observed.prob,
        boot.prob = event.boot.prob
      )
    
    # collate used data
    rownames(boot.prob) <- rs.test$combination
    used.data <- list(
      data = d$data,
      condition = d$condition,
      random.level = d$random.level,
      control = d$control,
      random.prob = boot.prob
    )
  }
  
  # condition is not specified -----------------------------------------
  
  ###### the following calculations are done when there is no condition specified, meaning the observed probability across all cases is compared to a null model based on permutations maintaining the event size and element probability
  if (is.null(condition)) {
    if (!is.null(test.condition)) {
      warning("test.condition was specified without a condition vector. Ignoring test.condition.", call. = FALSE)
    }
    if (!is.null(null.condition)) {
      warning("null.condition was specified without a condition vector. Ignoring null.condition.", call. = FALSE)
    }
    
    # same as above, account for added duration data
    if (is.null(duration)) {
      min.duration <- 1
    }
    if (!is.null(duration)) {
      min.duration <- min(duration)
      duration <- round(duration / min.duration, 0)
      data <- data[rep(1:nrow(data), times = duration), , drop = FALSE]
    }
    
    d <- apply(data, 2, as.numeric)
    
    # remove empty rows
    comb.size <- Rfast::rowsums(d, na.rm = TRUE)
    if (any(comb.size == 0)) {
      d <- d[comb.size > 0, ]
      NN <- sum(comb.size == 0)
      message(paste("Removing", NN, "rows with 0 active elements from data."))
    }
    # remove empty columns
    col.size <- colSums(d, na.rm = TRUE)
    if (any(col.size == 0)) {
      d <- d[, col.size > 0]
      NN <- sum(col.size == 0)
      removed.cols <- names(col.size)[col.size == 0]
      message(
        paste("Removing", NN, "column(s) with 0 obervations from data:", 
              paste(removed.cols, collapse = ", ")
        )
      )
    }
    
    ### create probabilities for test dataset
    elements.test <- get_active_elements(d)
    ######## remove NAs
    elements.test <- lapply(lapply(elements.test, na.omit), as.character)
    
    rs.test <- probability_of_combination(
      elements = elements.test,
      maxlen = combination.size
    )
    
    rs.test$combination.size <- calculate_combination_size(rs.test$combination)
    
    # add single elements that are not represented in the test data
    rs.test <- add_inactive_single_units(rs.test, single.units = colnames(d))
    
    ################## If there are NAs in the data, the following function re-calculates the number of possible events that combinations could have occurred in
    if (any(is.na(d))) {
      
      true_count_test <- sapply(rs.test$combination, function(k){
        sum(complete.cases(d[,unlist(strsplit(
          as.character(k), "_",
          fixed = TRUE
        ), F, F)]))
      })
     
      rs.test$observed.prob <- rs.test$count/true_count_test
    }
    
    # add event size information
    max.event.size <- max(Rfast::rowsums(data, na.rm = TRUE), na.rm = TRUE)
    
    if (use_parallel) {
      # run parallel loop
      # if on Mac or Linux, mclapply should work
      if (!Sys.info()[["sysname"]] == "Windows" & .Platform$OS.type == "unix") {
        boot.values <-
          mclapply(1:ran.trials,
                   function(x) {
                     netfacs_randomize_na(
                       m = d,
                       test.combinations = rs.test$combination,
                       max.combination.size = combination.size,
                       max.event.size = max.event.size
                     )
                   },
                   mc.cores = n_cores
          )
      } else {
        test.combinations <- rs.test$combination
        mycluster <- parallel::makeCluster(spec = n_cores, type = "PSOCK")
        parallel::clusterExport(
          cl = mycluster,
          varlist = c(
            "d",
            "test.combinations",
            "combination.size",
            "max.event.size",
            "randomizeMatrix",
            "netfacs_randomize_na",
            "get_active_elements",
            "probability_of_combination",
            "calculate_combination_size",
            "possible_combinations",
            "combinations",
            "Table",
            "probability_of_event_size",
            "rowsums"
          ),
          envir = environment()
        )
        doParallel::registerDoParallel(mycluster)
        boot.values <-
          parallel::parLapply(
            cl = mycluster,
            X = 1:ran.trials,
            function(x) {
              netfacs_randomize_na(
                m = d,
                test.combinations = test.combinations,
                max.combination.size = combination.size,
                max.event.size = max.event.size
              )
            }
          )
        stopCluster(mycluster)
      }
    } else {
      # run plain loop
      boot.values <-
        lapply(
          1:ran.trials,
          function(x) {
            netfacs_randomize_na(
              m = d,
              test.combinations = rs.test$combination,
              max.combination.size = combination.size,
              max.event.size = max.event.size
            )
          }
        )
    }
    
    ##### calculate difference between expected and observed probability as z value, p value, and probability increase
    boot.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$combination.prob
    }))
    
    res <-
      summarise_combination(
        combination = rs.test$combination,
        combination.size = rs.test$combination.size,
        observed.prob = rs.test$observed.prob,
        boot.prob = boot.prob,
        tail = tail,
        test.count = rs.test$count
      )
    
    ##### combination size information per event; shuffle so that the number of time each element appears is kept constant, but number of elements per row differs
    event.boot.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$event.size.prob
    }))
    
    event.observed.prob <-
      probability_of_event_size(elements.test, max.event.size)
    
    event.size.summary <-
      summarise_event_size(
        observed.prob = event.observed.prob,
        boot.prob = event.boot.prob
      )
    
    # collate used data
    rownames(boot.prob) <- rs.test$combination
    used.data <- list(
      data = d,
      condition = NULL,
      random.level = NULL,
      control = NULL,
      random.prob = boot.prob
    )
  }
  
  # summarize results -------------------------------------------------------
  
  # order res and boot 
  used.data$random.prob <- 
    used.data$random.prob[order(res$combination.size, -res$count), ]
  res <- res[order(res$combination.size, -res$count), ]
  
  res <-
    rapply(res,
           f = round,
           classes = "numeric",
           how = "replace",
           digits = 3
    )
  event.size.summary <-
    rapply(event.size.summary,
           f = round,
           classes = "numeric",
           how = "replace",
           digits = 3
    )
  
  used.parameters <- list(
    test.condition = test.condition,
    null.condition = null.condition
  )
  
  stat_method <- ifelse(is.null(used.data$condition), "permutation", "bootstrap")
  
  out <-
    list(
      result = res,
      used.parameters = used.parameters,
      used.data = used.data,
      event.size.information = event.size.summary
    )
  
  # set class and attributes
  out <-
    structure(
      out,
      class = "netfacs",
      stat_method = stat_method,
      random_trials = ran.trials
    )
  
  return(out)
}
