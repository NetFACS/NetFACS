test_that("probability of A given B is correct", {
  skip_on_cran()
  
  insert_na <- function(x, prob_na = 0.2) {
    x[sample(c(TRUE, NA), 
             prob = c(1-prob_na, prob_na), 
             size = length(x), 
             replace = TRUE)]
  }
  res <- 
    netfacs(
      data = d.sim.no.context,
      condition = NULL,
      ran.trials = 101,
      combination.size = 2,
      tail = "upper.tail",
      use_parallel = TRUE, 
      n_cores = detectCores()-1
    )
  res.na <- 
    netfacs_na(
      data = apply(d.sim.no.context, 2, insert_na),
      condition = NULL,
      ran.trials = 101,
      combination.size = 2,
      tail = "upper.tail",
      use_parallel = TRUE, 
      n_cores = detectCores()-1
    )
  
  cp <- conditional_probabilities(res)
  cp.na <- conditional_probabilities(res.na)
  
  p_8given3.cp <- cp %>% filter(combination == "8_3") %>% pull(p_AgivenB)
  p_8given3.cp.na <- cp.na %>% filter(combination == "8_3") %>% pull(p_AgivenB)
  
  p_2given4.cp <- cp %>% filter(combination == "2_4") %>% pull(p_AgivenB)
  p_2given4.cp.na <- cp.na %>% filter(combination == "2_4") %>% pull(p_AgivenB)
  
  # norm_pmi_9and10.cp <- cp %>% filter(combination == "9_10") %>% pull(norm_pmi)
  # norm_pmi_9and10.cp.na <- cp.na %>% filter(combination == "9_10") %>% pull(norm_pmi)
  
  p_tol <- 0.2
  # conditional probabilities
  expect_equal(p_8given3.cp, 1)
  expect_equal(p_8given3.cp.na, 1)
  
  expect_equal(p_2given4.cp, jp.no.context["4", "2"], tolerance = p_tol)
  expect_equal(p_2given4.cp.na, jp.no.context["4", "2"], tolerance = p_tol)
  
  # expect_equal(norm_pmi_9and10.cp, 1)
  # expect_equal(norm_pmi_9and10.cp.na, 1)
  
})




