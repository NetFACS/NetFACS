test_that("single element probabilities are calculated correctly", {
  skip_on_cran()
  # condition is specified
  res.boot <- netfacs(d.sim.with.context,
                      condition = rownames(d.sim.with.context),
                      test.condition = "a",
                      combination.size = 2,
                      ran.trials = 3)
  # condition is not specified
  res.rand <- netfacs(d.sim.no.context,
                      condition = NULL,
                      test.condition = NULL,
                      combination.size = 2,
                      ran.trials = 3)
  # how much deviation from true value to tolerate?
  p_tol <- 0.02
  
  p2 <- res.boot$result %>% 
    dplyr::filter(combination == "2") %>% 
    dplyr::pull(observed.prob)
  p3and10 <- res.boot$result %>% 
    dplyr::filter(combination == "3_10") %>% 
    dplyr::pull(observed.prob)
  p3and10.expected <- context.def["a", "3"]*context.def["a", "10"]
  
  # res.cond.boot <- network.conditional(res.boot)
  
  # p5given2 <- 
  #   res.cond.boot$conditional.probalities %>% 
  #   filter(elementA == "5" & elementB == "2") %>% 
  #   pull(Probability_AgivenB)
  
  p4 <- res.rand$result %>% 
    dplyr::filter(combination == "4") %>% 
    dplyr::pull(observed.prob)
  p8and9 <- res.rand$result %>% 
    dplyr::filter(combination == "8_9") %>% 
    dplyr::pull(observed.prob)
  p8and9.expected <- p.no.context[["8"]]*p.no.context[["9"]]
  
  # res.cond.rand <- network.conditional(res.rand)
  # 
  # p1given6 <- 
  #   res.cond.rand$conditional.probalities %>% 
  #   filter(elementA == "1" & elementB == "6") %>% 
  #   pull(Probability_AgivenB)
  
  # single element probability
  expect_equal(p4, p.no.context[["4"]], tolerance = p_tol)
  expect_equal(p2, context.def["a", "2"], tolerance = p_tol)
  # joint probabilities
  expect_equal(p8and9, p8and9.expected, tolerance = 0.3)
  expect_equal(p3and10, p3and10.expected, tolerance = 0.3)
  # # conditional probabilities
  # expect_equal(p1given6, jp.no.context["6", "1"], tolerance = p_tol)
  # expect_equal(p5given2, joint.prob.matrix$a["2", "5"], tolerance = p_tol)
  
})
test_that("error message is given when data has NAs",{
  expect_error({
    netfacs(
      data = NA,
      condition = emotions_set[[2]]$emotion,
      test.condition = "anger",
      null.condition = NULL,
      ran.trials = 3)
  })
  expect_error({
    netfacs(data = emotions_set[[1]],
            condition = NA,
            test.condition = "anger",
            null.condition = NULL,
            ran.trials = 3)
  })
})
test_that("error message is given when data is misspecified",{
  expect_error({
    netfacs(data = matrix())
  })
  expect_error({
    netfacs(data = data.frame())
  })
  expect_error({
    netfacs(data = list())
  })
  expect_error({
    netfacs(data = data.frame(a = rep(c("0", "1"), times = 3),
                              b = rep(c("o", "1"), times = 3)))
      
  })
})
test_that("error message is given when condidions are misspecified",{
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = emotions_set[[2]]$emotion,
      test.condition = "AAA",
      null.condition = "anger",
      ran.trials = 3
    )},
    "Argument 'test.condition' is not part of the 'condition' vector."
  )
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = emotions_set[[2]]$emotion,
      test.condition = "anger",
      null.condition = "AAA",
      ran.trials = 3
    )},
    "Argument 'null.condition' is not part of the 'condition' vector."
  )
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = "b",
      test.condition = "b",
      null.condition = NULL,
      ran.trials = 3
    )},
    "Argument 'condition' must be the same length as nrow 'data'."
  )
})
test_that("warning message is given when test or null conditions are specified with a NULL condition vector",{
  expect_warning({
    netfacs(
      data = emotions_set[[1]],
      condition = NULL,
      test.condition = "b",
      null.condition = NULL,
      ran.trials = 3)
  })
  expect_warning({
    netfacs(
      data = emotions_set[[1]],
      condition = NULL,
      test.condition = NULL,
      null.condition = "a",
      ran.trials = 3)
  })
})

