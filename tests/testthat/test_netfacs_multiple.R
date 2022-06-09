test_that("error message is given when 'condidion' is misspecified",{
  expect_error({
    netfacs_multiple(
      data = emotions_set[[1]],
      condition = rep(1, length.out = nrow(emotions_set[[1]]))
    )},
    "Argument 'condition' must have more than one unique value"
  )
  expect_error({
    netfacs_multiple(
      data = emotions_set[[1]],
      condition = rep(c(1,NA), length.out = nrow(emotions_set[[1]]))
    )},
    "Please remove all NAs from the data and/or condition vector."
  )
})
