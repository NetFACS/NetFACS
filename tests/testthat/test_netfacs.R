test_that("single core, condition specified is correct",{
  expect_equal(netfacs(d.sim.with.context, 
                       condition = rownames(d.sim.with.context), 
                       test.condition = "a", 
                       combination.size = 2,
                       ran.trials = 500, 
                       use_parallel = FALSE,
                       n_cores = 1),
               res.single.core.with.context, 
               tolerance = 0.13)
})
test_that("multi-core, condition specified is correct",{
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
test_that("single core, condition NOT specified is correct",{
  expect_equal(netfacs(d.sim.no.context, 
                       ran.trials = 500,
                       combination.size = 2,
                       use_parallel = FALSE,
                       n_cores = 1),
               res.single.core.no.context, 
               tolerance = 0.11)
})
test_that("multi-core, condition NOT specified is correct",{
  expect_equal(netfacs(d.sim.no.context, 
                       ran.trials = 500, 
                       combination.size = 2,
                       use_parallel = TRUE,
                       n_cores = 6),
               res.multi.core.no.context, 
               tolerance = 0.11)
})
test_that("error message is given when data has NA",{
  expect_error(
    {
      netfacs(
        data = NA,
        condition = emotions_set[[2]]$emotion,
        test.condition = "anger",
        null.condition = NULL,
        ran.trials = 20
      )
    }
  )
})
test_that("error message is given when condidion has NA",{
  expect_error(
    {
      netfacs(
        data = emotions_set[[1]],
        condition = NA,
        test.condition = "anger",
        null.condition = NULL,
        ran.trials = 20
      )
    }
  )
})
test_that("error message is given when test.condidion is not part of condition",{
  expect_error(
    {
      netfacs(
        data = emotions_set[[1]],
        condition = emotions_set[[2]]$emotion,
        test.condition = "AAA",
        null.condition = "anger",
        ran.trials = 20
      )
    }
  )
})
test_that("error message is given when null.condidion is not part of condition",{
  expect_error(
    {
      netfacs(
        data = emotions_set[[1]],
        condition = emotions_set[[2]]$emotion,
        test.condition = "anger",
        null.condition = "AAA",
        ran.trials = 20
      )
    }
  )
})
