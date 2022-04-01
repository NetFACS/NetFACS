test_that("pvalues don't change when restricting max event size", {
  set.seed(123)
  xx <- sample(0:1, size = 1000, replace = TRUE, prob = c(0.6, 0.4))
  m <- matrix(xx, ncol = 10)
  
  res1 <- probability_of_event_size(m, max.event.size = 3)
  res2 <- probability_of_event_size(m, max.event.size = 10)
  expect_equal(res1["3"], res2["3"])
})
