test_that("condition specified calculations are correct",{
  expect_equal(netfacs(d.sim.with.context, 
                       condition = rownames(d.sim.with.context), 
                       test.condition = "a", 
                       combination.size = 2,
                       ran.trials = 500, 
                       use_parallel = FALSE,
                       n_cores = 1),
               res.single.core.with.context, 
               tolerance = 0.13)
  expect_equal(netfacs(d.sim.with.context, 
                       condition = rownames(d.sim.with.context), 
                       test.condition = "a", 
                       combination.size = 2,
                       ran.trials = 500, 
                       use_parallel = TRUE,
                       n_cores = 6),
               res.multi.core.with.context, 
               tolerance = 0.13)
})
test_that("condition NOT specified calculations are correct",{
  # single core
  expect_equal(netfacs(d.sim.no.context, 
                       ran.trials = 500,
                       combination.size = 2,
                       use_parallel = FALSE,
                       n_cores = 1),
               res.single.core.no.context, 
               tolerance = 0.11)
  # multi core
  expect_equal(netfacs(d.sim.no.context, 
                       ran.trials = 500, 
                       combination.size = 2,
                       use_parallel = TRUE,
                       n_cores = 6),
               res.multi.core.no.context, 
               tolerance = 0.11)
})
test_that("error message is given when data has NAs",{
  expect_error({
    netfacs(
      data = NA,
      condition = emotions_set[[2]]$emotion,
      test.condition = "anger",
      null.condition = NULL,
      ran.trials = 20)
  })
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = NA,
      test.condition = "anger",
      null.condition = NULL,
      ran.trials = 20)
  })
})
test_that("error message is given when condidions are not misspecified",{
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = emotions_set[[2]]$emotion,
      test.condition = "AAA",
      null.condition = "anger",
      ran.trials = 20)
  })
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = emotions_set[[2]]$emotion,
      test.condition = "anger",
      null.condition = "AAA",
      ran.trials = 20)
  })
  expect_error({
    netfacs(
      data = emotions_set[[1]],
      condition = "b",
      test.condition = "b",
      null.condition = NULL,
      ran.trials = 10)
  })
})
test_that("warning message is given when test or null conditions are specified with a NULL condition vector",{
  expect_warning({
    netfacs(
      data = emotions_set[[1]],
      condition = NULL,
      test.condition = "b",
      null.condition = NULL,
      ran.trials = 10)
  })
  expect_warning({
    netfacs(
      data = emotions_set[[1]],
      condition = NULL,
      test.condition = NULL,
      null.condition = "a",
      ran.trials = 10)
  })
})

