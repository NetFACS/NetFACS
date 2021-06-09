
test_that("correct probabilities are calculated",{
  expect_snapshot_output(
    calculate_prob_of_comb(
      elements = list(c("c", "b", "i"), c("b", "e", "j", "g", "d", "h", "a"), c("i", "c", "b"), c("e", "j", "a", "d", "c", "i", "f", "b"), c("f", "h", "e", "j", "c", "g", "a"), c("j", "g", "b", "d", "a", "i", "c", "h"), c("b", "j", "c", "g", "f"), c("b", "d", "i", "g", "h", "a"), c("d", "e", "f"), c("j", "g", "e", "d", "b")), 
      maxlen = 2
    )
  )
})


