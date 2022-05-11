test_that("combination.size is properly filtered", {
  cs.null <- netfacs.extract(res.multi.core.with.context, 
                             combination.size = NULL)
  cs.1 <- netfacs.extract(res.multi.core.with.context, 
                          combination.size = 1)
  
  expect_equal(unique(cs.null$combination.size), 1:2)
  expect_equal(unique(cs.1$combination.size), 1)
})