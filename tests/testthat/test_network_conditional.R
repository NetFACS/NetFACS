test_that("conditional probability calculations are correct",{
  res.net.cond.test <- network.conditional(res.single.core.no.context)
  expect_equal(res.net.cond.test$conditional.probalities, 
               res.net.cond$conditional.probalities)
})