#' Create probability distribution of combinations of elements in the data
#'
#' The \code{\link{netfacs}} function underlies most other functions in this package. \cr
#' It takes the data set and reports the observed and expected probabilities that elements and
#' combinations of elements occur in this data set, and whether this differs
#' from a null condition.
#'
#' @param data matrix with one column per element, and one row per event,
#'        consisting of 1 (element was active during that event) and 0 (element
#'        was not active)
#' @param condition character vector of same length as 'data' that contains
#'        information on the condition each event belongs to, so probabilities
#'        can be compared across conditions; if NULL, all events will be tested
#'        against a random null condition based on permutations
#' @param test.condition level of 'condition' that is supposed to be tested
#' @param null.condition level of 'condition' that is used to create the null
#'        distribution of values; if NULL, all levels that are not the test
#'        condition will be used
#' @param duration numeric vector that contains information on the duration of
#'        each event; if NULL, all events are assumed to have equal duration
#' @param ran.trials Number of randomisations that will be performed to find the
#'        null distribution
#' @param random.level character vector of the level on which the randomization
#'        should take place. If NULL, the randomization takes place on the event
#'        level (i.e., every row can either be selected or not); if a vector is
#'        provided, the randomization takes place on the levels of that vector
#'        rather than individual events
#' @param control list of vectors that are used as control variables. During
#'        bootstraps, the ratio of events in each level will be adapted. So, for
#'        example, if in the test distribution, there are three angry
#'        participants for each happy participant, the null distribution will
#'        maintain that ratio
#' @param combination.size if not all combinations of elements are of interest
#'        (e.g., if the question only concerns single elements or dyads of
#'        elements), this variable allows to reduce the results to those
#'        combinations, increasing speed
#' @param tail either 'upper.tail' (proportion of null probabilities
#'         that are larger than observed probabilities), or 'lower.tail' (proportion of null
#'         probabilities that are smaller than observed probabilities); default is 'upper.tail'
#' @param use_parallel logical, should the bootstrap be parallelized (default is
#'        \code{TRUE})
#' @param n_cores numeric, the number cores to be used for parallelization.
#'        Default is the number of available cores minus 1.
#'
#' @details Expected values are based on bootstraps of null distribution, so the
#' values represent distribution of element co-occurrence under null condition;
#' or permutations of the observed distribution to test it against 'random'.
#'
#' The resulting object is the basis for most other functions in this package.
#'
#' @return Function returns a Result data frame that includes the combination name, how many elements it consisted of, how often it was observed, the probability it was observed under this condition, the expected probability under null condition (based on the permutation or bootstrap), effect size (difference between observed probability and expected probability), p-value (how many randomisations were more extreme), and for direct comparisons of contexts the specificity (probability that the condition is in fact the test condition if that combination is known) and probability increase (the factor by which the probability of the element is higher in the test than null condition)
#' @return 'event.size.information' contains information about the observed and expected size of combination or elements per event based on the randomisations
#'
#' @importFrom stats sd quantile
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
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' \donttest{
#' data(emotions_set)
#' angry.face <- netfacs(
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

netfacs <- function(data,
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
  
  # fix column names of dataset to not include special characters or spaces
  colnames(data) <- gsub(
    colnames(data),
    pattern = " ",
    replacement = "",
    fixed = TRUE
  )
  colnames(data) <- gsub(
    colnames(data),
    pattern = "+",
    replacement = "",
    fixed = TRUE
  )
  colnames(data) <- gsub(
    colnames(data),
    pattern = "_",
    replacement = "",
    fixed = TRUE
  )
  colnames(data) <- gsub(
    colnames(data),
    pattern = ".",
    replacement = "",
    fixed = TRUE
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
    combination.size <- max(rowsums(data))
  }
  
  
  # null condition is specified ---------------------------------------------
  
  # data preparation happens for two different cases: either 'condition' is set, in which case the 'test.condition' is tested against all other cases or against one specific 'null.condition'; alternatively, if no condition is set, the probabilities are compared with random probabilities
  if (!is.null(condition)) {
    # Error messages in case test.condition is wrongly specified
    if (is.null(test.condition)) {
      return(print("Error: specify test condition"))
    }
    if (!test.condition %in% condition) {
      return(print("Test condition not part of the condition vector"))
    }
    
    # if random.level is not defined, each event/row is its own case, and all events are compared against each other. If random.level is defined, the randomization will select cases based on which level they belong to
    if (is.null(random.level)) {
      random.level <- 1:nrow(data)
    }
    
    # if the null condition is not determined, all cases that are not part of the test dataset are classed as null condition
    condition.x <- as.character(condition)
    if (is.null(null.condition)) {
      null.condition <- "all"
      condition.x[condition.x != test.condition] <- "all"
    }
    
    # the control argument, which is entered as a list (to allow for multiple control variables) is turned into a combination of those variables. E.g., if you control for sex and social group, then each individual or is classed as sex_group
    if (length(control) > 1) {
      control.obj <- apply(do.call(cbind, control), 1, function(k) {
        paste(k, collapse = "_")
      })
    }
    if (length(control) == 1) {
      control.obj <- unlist(control[1])
    }
    if (is.null(control)) {
      control.obj <- rep(1, nrow(data))
    }
    
    # if duration is set, all variable vectors have to be multiplied accordingly
    if (is.null(duration)) {
      min.duration <- 1
    }
    if (!is.null(duration)) {
      min.duration <- min(duration)
      duration <- round(duration / min.duration, 0) # duration is determined by the minimum value. So, if the shortest event is 0.05s, then an even of 1sec will be represented 20 times
      data <- data[rep(1:nrow(data), times = duration), ]
      
      condition.x <-
        condition.x[rep(1:length(condition.x), times = duration)]
      control.obj <-
        control.obj[rep(1:length(control.obj), times = duration)]
      random.level <-
        random.level[rep(1:length(random.level), times = duration)]
    }
    
    # remove empty rows
    data <- apply(data, 2, as.numeric)
    comb.size <- rowsums(data)
    data <- data[comb.size > 0, ]
    condition.x <- condition.x[comb.size > 0 ]
    condition <- condition[comb.size > 0 ]
    duration <- duration[comb.size > 0 ]
    random.level <- random.level[comb.size > 0 ]
    control.obj <- control.obj[comb.size > 0 ]
    
    ### turn data into test data and null data
    data.test <- data[condition.x == test.condition, ]
    data.null <- data[condition.x == null.condition, ]
    
    # turn control variable into test and null control variable
    test.control <- control.obj[condition.x == test.condition]
    null.control <- control.obj[condition.x == null.condition]
    
    # turn random level variable into test and null control variable
    random.level.null <- random.level[condition.x == null.condition]
    random.level.test <- random.level[condition.x == test.condition]
    
    # create ratio for randomisation of null dataset: if the test dataset contains 12 males and 15 females (ratio of 1.25), then the selection of the null dataset should reflect this
    rl.test.ratio <- Table(test.control[!duplicated(random.level.test)])
    rl.test.ratio <- rl.test.ratio / sum(rl.test.ratio) # ratio for test data
    
    rl.null.ratio <- data.frame(
      subj = random.level.null[!duplicated(random.level.null)],
      control = null.control[!duplicated(random.level.null)]
    )
    rl.null.ratio$llh <- as.numeric(rl.test.ratio[match(rl.null.ratio$control,
                                                        names(rl.test.ratio))])
    
    
    # create probabilities for null and test datasets
    # create a list for each event/row of the dataset, containing the names of the elements that were active
    elements.null <- lapply(1:nrow(data.null), function(x) {
      xx <- colnames(data.null)[data.null[x, ] == 1]
    })
    elements.test <- lapply(1:nrow(data.test), function(x) {
      xx <- colnames(data.test)[data.test[x, ] == 1]
    })
    
    # extract the probabilities for each combination of elements
    rs.null <- calculate_prob_of_comb(elements = elements.null, 
                                      maxlen = combination.size) 
    rs.test <- calculate_prob_of_comb(elements = elements.test, 
                                      maxlen = combination.size) 
    
    rs.null$combination.size <- calculate_combination_size(rs.null$combination)
    rs.test$combination.size <- calculate_combination_size(rs.test$combination)
    
    # for single units that are not active during this specific condition, add them anyways with the information that they did not show up
    rs.null <- add_inactive_single_units(rs.null, 
                                         single.units = colnames(data.null))
    rs.test <- add_inactive_single_units(rs.test, 
                                         single.units = colnames(data.test))
    
    # create an object with the observed event sizes
    event.sizes <- Table(sapply(elements.test, FUN = length))
    max.event.size <- max(rowsums(data))
    
    # function for the bootstrap, to be used in parallel lapply, or
    # in standard lapply loop
    # creates the probabilities of each element combination for random
    # selections of the null data set
    boot_foo <- function(x) {
      # fun must have only one argument that is NOT used to run in loop
      # make sure that necessary objects (i.e. rl.null.ratio, random.level.null, elements.null, combination.size, max.event.size) are present in the environment
      # sample elements from the null condition
      # keep relative number of observations per subject constant
      subj.boot <- sample(
        x = rl.null.ratio$subj,
        replace = TRUE,
        prob = rl.null.ratio$llh
      )
      elements.boot <-
        elements.null[random.level.null %in% unique(subj.boot)]
      
      # calculate probabilities for random dataset
      rs.boot <-
        calculate_prob_of_comb(elements = elements.boot,
                               maxlen = combination.size)
      
      # order to match real data
      boot.prob <-
        rs.boot$observed.prob[match(rs.test$combination, rs.boot$combination)]
      boot.prob[is.na(boot.prob)] <- 0
      
      # create information for event sizes (how many AUs active at once) as well
      event.sizes.boot <-
        Table(sapply(elements.boot, FUN = length))
      
      # calculate probability for an event of that size (N AU active) to occur
      xx <- data.frame(
        combination.size = 0:max.event.size,
        observed.prob = 0
      )
      xx$observed.prob[match(names(event.sizes.boot), xx$combination.size)] <-
        event.sizes.boot / sum(event.sizes.boot, na.rm = TRUE)
      return(
        list(
          boot.prob = boot.prob,
          boot.event.sizes.prob = xx$observed.prob
        )
      )
    }
    
    if (use_parallel) {
      # run parallel loop
      # which type of parallel depends on parallel_safe
      # if on Mac or Linux, mclapply should work
      if (!(Sys.info()[["sysname"]] == "Windows") &
          .Platform$OS.type == "unix") {
        boot.values <- mclapply(1:ran.trials,
                                FUN = boot_foo,
                                mc.cores = n_cores
        )
      } else {
        # create cluster for parallelization
        mycluster <- makeCluster(n_cores, type = "PSOCK")
        # export the relevant information to each core
        clusterExport(
          cl = mycluster,
          c(
            "elements.null",
            "max.event.size",
            "rs.test",
            "rl.null.ratio",
            "combination.size",
            "random.level.null",
            "calculate_prob_of_comb",
            "Table"
          ),
          envir = environment()
        )
        registerDoParallel(mycluster)
        # run parallel loop
        boot.values <-
          parLapply(
            cl = mycluster,
            X = 1:ran.trials,
            fun = boot_foo
          )
        # stop cluster
        stopCluster(mycluster)
      }
    } else {
      # run plain loop
      boot.values <- lapply(1:ran.trials, FUN = boot_foo)
    }
    
    ##### take the calculated probabilities for the randomisations and use them to return p values, z values, specificity, and increase
    
    boot.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$boot.prob
    }))
    
    rs.test <- calc_effect_pval_pincrease(rs.test = rs.test, 
                                          boot.prob = boot.prob, 
                                          tail = tail, 
                                          duration = duration,
                                          min.duration = min.duration)
    
    ### for specificity, divide how often the combination occurs in the test condition by the total count (test + null condition)
    null.count <- rs.null$count[match(rs.test$combination, rs.null$combination)]
    null.count[is.na(null.count)] <- 0
    rs.test$specificity <- rs.test$count / (rs.test$count + null.count)
    
    rs.test <- rs.test[, c(
      "combination",
      "combination.size",
      "count",
      "observed.prob",
      "expected.prob",
      "effect.size",
      "pvalue",
      "specificity",
      "prob.increase"
    )]
    
    ##### combination size information per event
    event.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$boot.event.sizes.prob
    }))
    
    t.event.size <- calculate_event_size_prob(event.prob, event.sizes)
  }
  
  
  # null condition is not specified -----------------------------------------
  
  ###### the following calculations are done when there is no condition specified, meaning the observed probability across all cases is compared to a null model based on permutations maintaining the event size and element probability
  if (is.null(condition)) {
    # same as above, account for added duration data
    if (is.null(duration)) {
      min.duration <- 1
    }
    if (!is.null(duration)) {
      min.duration <- min(duration)
      duration <- round(duration / min.duration, 0)
      data <- data[rep(1:nrow(data), times = duration), ]
    }
    
    condition.x <- rep("all", nrow(data))
    data <- apply(data, 2, as.numeric)
    
    # remove empty rows
    comb.size <- rowsums(data)
    data <- data[comb.size > 0, ]
    condition.x <- condition.x[comb.size > 0 ]
    
    ### create probabilities for test dataset
    elements.test <- lapply(1:nrow(data), function(x) {
      xx <- colnames(data)[data[x, ] == 1]
    })
    
    rs.test <- calculate_prob_of_comb(elements = elements.test, 
                                      maxlen = combination.size)
    
    rs.test$combination.size <- calculate_combination_size(rs.test$combination)
    
    # add single elements that are not represented in the test data
    rs.test <- add_inactive_single_units(rs.test, single.units = colnames(data))
    
    data <- as.matrix(data)
    # add event size information
    event.sizes <- Table(sapply(elements.test, FUN = length))
    max.event.size <- max(rowsums(data))
    
    # randomization function to be used either in parallel or plain loop
    randomize_foo <- function(x) {
      # fun must have only one argument that is NOT used to run in loop
      # make sure that necessary objects (i.e. d, rs.test, combination.size) are present in the environment
      # To establish the expected probabilities for the elements, the data is compared to permutations that keep the number of elements per row constant but allow for the element probability to vary
      xx <-
        randomizeMatrix(
          samp = data,
          null.model = "richness",
          iterations = 100
        )
      
      elements.boot <- lapply(1:nrow(xx), function(x) {
        xx <- colnames(data)[xx[x, ] == 1]
      })
      
      # create probabilities for only the single element combinations
      rs.boot.1 <- calculate_prob_of_comb(elements = elements.boot, 
                                          maxlen = 1)
      
      # for all other combinations, do permutations that keep the number of cases per row and per column the same
      xx <-
        randomizeMatrix(
          samp = data,
          null.model = "independentswap",
          iterations = 100
        )
      
      elements.boot <- lapply(1:nrow(xx), function(x) {
        xx <- colnames(data)[xx[x, ] == 1]
      })
      
      # create overall random dataset
      rs.boot.all <- calculate_prob_of_comb(elements = elements.boot, 
                                            maxlen = combination.size)
      
      comb.size <- calculate_combination_size(rs.boot.all$combination)
      
      rs.boot.all <- rs.boot.all[comb.size > 1, ]
      
      # combine the datasets
      rs.boot <- rbind(rs.boot.1, rs.boot.all)
      
      boot.prob <- 
        rs.boot$observed.prob[match(rs.test$combination, rs.boot$combination)]
      boot.prob[is.na(boot.prob)] <- 0
      
      list(boot.prob = boot.prob)
    }
    
    if (use_parallel) {
      # run parallel loop
      # which type of parallel depends on parallel_safe
      # if on Mac or Linux, mclapply should work
      if (!Sys.info()[["sysname"]] == "Windows" &
          .Platform$OS.type == "unix") {
        boot.values <- mclapply(1:ran.trials,
                                FUN = randomize_foo,
                                mc.cores = n_cores
        )
      } else {
        # create cluster for parallelization
        mycluster <- makeCluster(spec = n_cores, type = "PSOCK")
        # export the relevant information to each core
        clusterExport(
          cl = mycluster,
          c(
            "data",
            "rs.test",
            "combination.size",
            "calculate_prob_of_comb",
            "calculate_combination_size",
            "randomizeMatrix"
          ),
          envir = environment()
        )
        registerDoParallel(mycluster)
        # run parallel loop
        boot.values <-
          parLapply(
            cl = mycluster,
            X = 1:ran.trials,
            fun = randomize_foo
          )
        # stop cluster
        stopCluster(mycluster)
      }
    } else {
      # run plain loop
      boot.values <- lapply(1:ran.trials, FUN = randomize_foo)
    }
    
    ##### calculate difference between expected and observed probability as z value, p value, and probability increase
    boot.prob <- do.call(cbind, lapply(boot.values, function(x) {
      x$boot.prob
    }))
    
    rs.test <- calc_effect_pval_pincrease(rs.test = rs.test, 
                                          boot.prob = boot.prob, 
                                          tail = tail, 
                                          duration = duration)
    
    ##### combination size information per event; shuffle so that the number of time each element appears is kept constant, but number of elements per row differs
    event.prob <- do.call(cbind, lapply(1:ran.trials, function(x) {
      xx <- randomizeMatrix(
        samp = as.matrix(data),
        null.model = "frequency",
        iterations = 100
      )
      event.sizes.boot <- 
        Table(rowsums(xx)[rowsums(xx) <= max.event.size]) / nrow(xx)
      event.sizes.boot <- event.sizes.boot[!is.na(event.sizes.boot)]
      
      cs <- data.frame(
        combination.size = 0:max.event.size,
        observed.prob = 0
      )
      cs$observed.prob[match(names(event.sizes.boot), cs$combination.size)] <- 
        event.sizes.boot
      return(cs$observed.prob)
    }))
    
    t.event.size <- calculate_event_size_prob(event.prob, event.sizes)
  }
  
  # summarize results -------------------------------------------------------
  
  # summaries for the object
  rs.test <-
    rapply(rs.test,
           f = round,
           classes = "numeric",
           how = "replace",
           digits = 2)
  t.event.size <-
    rapply(t.event.size,
           f = round,
           classes = "numeric",
           how = "replace",
           digits = 2)
  
  used.parameters <- list(
    test.condition = test.condition,
    null.condition = null.condition
  )
  used.data <- list(
    data = data,
    condition = condition,
    random.level = random.level,
    control = control,
    random.prob = boot.prob
  )
  
  list(
    result = rs.test,
    used.parameters = used.parameters,
    used.data = used.data,
    event.size.information = t.event.size
  )
}


# helpers -----------------------------------------------------------------

calculate_combination_size <- function(x) {
  # X: a vector of AU combinations, sep by _
  unlist(
    lapply(
      strsplit(
        as.character(x), "_",
        fixed = TRUE
      ),
      length
    ), 
    FALSE, FALSE)
}

add_inactive_single_units <- function(d, single.units) {
  # d: data
  # single.units: single inactive AUs
  
  innactive.single.units <- 
    single.units[!single.units %in% d$combination]
  
  if (length(innactive.single.units) > 0) {
    res <- data.frame(
      combination = innactive.single.units,
      observed.prob = 0,
      count = 0,
      combination.size = 1
    )
    return(rbind(res, d))
  }
  d
}

calculate_event_size_prob <- function(event.prob, event.sizes) {
  # event prob: a matrix
  # event.sizes: a named vector
  t.event.size <- data.frame(
    combination.size = 0:(nrow(event.prob) - 1),
    observed.prob = 0,
    expected.prob = 0
  )
  t.event.size$observed.prob[match(names(event.sizes), 
                                   t.event.size$combination.size)] <- 
    event.sizes / sum(event.sizes)
  t.event.size$expected.prob <- rowmeans(event.prob)
  
  # effect size here is the difference between the observed and expected values
  t.event.size$effect.size <- 
    t.event.size$observed.prob - t.event.size$expected.prob
  
  
  t.event.size$pvalue <- as.numeric(lapply(1:nrow(event.prob), function(z) {
    # p-value: two-sided, testing how many of the permutation results for that element are more extreme than the observed value
    xx <- min(
      mean(
        event.prob[z, ] >= t.event.size$observed.prob[z],
        na.rm = TRUE
      ),
      mean(
        event.prob[z, ] <= t.event.size$observed.prob[z],
        na.rm = TRUE
      )
    ) # take the smaller of the two p-values (number of permutations with larger and smaller values)
    return(xx)
  }))
  t.event.size
}

calc_effect_pval_pincrease <- function(rs.test, boot.prob, tail, duration, 
                                       min.duration) {
  rs.test$expected.prob <- rowmeans(boot.prob)
  
  # effect size here is the difference between the observed and expected values
  rs.test$effect.size <- rs.test$observed.prob - rs.test$expected.prob
  
  # # p-values
  if (tail == "upper.tail") {
    rs.test$pvalue <- rowmeans(boot.prob >= rs.test$observed.prob)
  }
  if (tail == "lower.tail") {
    rs.test$pvalue <- rowmeans(boot.prob <= rs.test$observed.prob)
  }
  
  if (!is.null(duration)) {
    rs.test$count <- rs.test$count * min.duration
  }
  
  # create probability increase by comparing the observed probability with the
  # mean probability of the randomisation process
  rs.test$prob.increase <- rs.test$observed.prob / rowmeans(boot.prob)
  rs.test$prob.increase[is.infinite(rs.test$prob.increase)] <- NA
  
  rs.test <- rs.test[, c(
    "combination",
    "combination.size",
    "count",
    "observed.prob",
    "expected.prob",
    "effect.size",
    "pvalue",
    "prob.increase"
  )]
  boot.prob <-
    boot.prob[order(rs.test$combination.size, -1 * as.numeric(rs.test$count)), ]
  rs.test <-
    rs.test[order(rs.test$combination.size, -1 * as.numeric(rs.test$count)), ]
  rs.test <- rs.test[rs.test$combination!='NA' & !is.na(rs.test$combination),]
  
  rs.test
}
