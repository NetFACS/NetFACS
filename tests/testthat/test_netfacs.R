
# dummy data
m <- matrix(
  c(0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
    1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1), 
  ncol = 5, 
  dimnames = list(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", "d", "d", "d", "d", "d", "e", "e", "e", "e", "e", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", "c", "c", "d", "d", "d", "d", "d", "d", "d", "e", "e", "e", "e", "e", "e", "e", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "e", "e", "e", "e", "e", "e", "e", "e", "e", "e", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", "c", "d", "d", "d", "d", "d", "d", "e", "e", "e", "e", "e", "e"), 
                  1:5))

test_that("single core, condition specified is correct",{
  expect_snapshot_output(
    {
      set.seed(1)
      netfacs(
        data = m,
        condition = rownames(m),
        test.condition = "b",
        ran.trials = 102,
        combination.size = 4,
        tail = "upper.tail",
        use_parallel = FALSE,
        n_cores = 1
      )
    }
  )
})
test_that("multi-core, condition specified is correct",{
  expect_snapshot_output(
    {
      set.seed(1)
      netfacs(
        data = m,
        condition = rownames(m),
        test.condition = "b",
        ran.trials = 102, # need >100 for multicore
        combination.size = 4,
        tail = "upper.tail",
        use_parallel = TRUE,
        n_cores = 2
      )
    }
  )
})
test_that("single core, condition NOT specified is correct",{
  expect_snapshot_output(
    {
      set.seed(1)
      netfacs(
        data = m,
        condition = NULL,
        ran.trials = 102,
        combination.size = 4,
        tail = "upper.tail",
        use_parallel = FALSE,
        n_cores = 1
      )
    }
  )
})
test_that("multi-core, condition NOT specified is correct",{
  expect_snapshot_output(
    {
      set.seed(1)
      netfacs(
        data = m,
        condition = NULL,
        ran.trials = 102, # need >100 for multicore
        combination.size = 4,
        tail = "upper.tail",
        use_parallel = TRUE,
        n_cores = 2
      )
    }
  )
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
