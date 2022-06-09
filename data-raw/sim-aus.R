# simulate FACS data for unit tests

# sim no context -------------------------------------------------------

# define the "truth"; ie data generating process
# 10 total AUs, 
# four common ones: 4, 7, 9 and to a lesser extent 6
# Other AUs are rare, 10 is never active
aus <- as.character(1:10)
p <- c(0.1, 0.1, 0.1, 0.9, 0.1, 0.51, 0.9, 0.1, 0.9, 0.0)
p <- setNames(p, aus)
n_obs <- 5000

# joint probability matrix
jp <- matrix(0, nrow = length(p), ncol = length(p), 
             dimnames = list(aus, aus))
jp["4", "2"] <- 0.9 # if 4 is active, 2 has 90% prob 
jp["6", "1"] <- 0.95 # if 6 is active, 1 has 95% prob
diag(jp) <- NA


# simulate observations for single AUs
m <- matrix(nrow = n_obs, ncol = length(p))
colnames(m) <- names(p)
# sim prob of single AUs
for(i in 1:n_obs){
  m[i, ] <- rbinom(length(p), size = 1, prob = p)
}

head(m)
d <- m

# sim joint probabilities
# i <- 1
for(i in seq_len(nrow(d))){
  obs <- d[i, ]
  active <- names(obs[obs == 1])
  inactive <- names(obs[obs == 0])
  
  # get joint prob of active and inactive AUs
  xx <- jp[active, inactive, drop = FALSE]
  res <- rbinom(length(xx), size = 1, prob = xx)
  if(is.matrix(xx) & length(res)!=0){
    res <- matrix(res, nrow = nrow(xx), dimnames = list(NULL, colnames(xx)))
    res2 <- colSums(res)
  } else{
    res2 <- setNames(res, names(xx))
  }
  # update inactive AUs based on joint prob
  if(length(res2) != 0) d[i, inactive] <- res2
}

head(d)

# for when there are multiple joint probabilities
d.sim.no.context <- apply(d, 2, function(x) ifelse(x>1, 1, x))


# sim with context --------------------------------------------------------

#define two contexts
pa <- c(0.1, 0.9, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
pb <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.9, 0.9)
context.def <- matrix(c(pa, pb), nrow = 2, byrow = TRUE, 
              dimnames = list(c("a", "b"), aus))

# joint probability matrices
jpa <- matrix(0, nrow = length(pa), ncol = length(pa), 
             dimnames = list(aus, aus))
diag(jpa) <- NA
jpb <- jpa

jpa["2", "5"] <- 0.9 # if 2 is active, 5 has 90% prob 
jpb["8", "1"] <- 0.9 # if 8 is active, 1 has 90% prob

joint.prob.matrix <- 
  list(a = jpa, 
       b = jpb)

# sample
d.sim.with.context <- sample_contexts(context.def, 
                                      jp = joint.prob.matrix,
                                      n_obs = 2500)

head(d.sim.with.context)


# run netfacs on sim data -------------------------------------------------

# these results will be used in unit tests

res.multi.core.with.context <-
  netfacs(d.sim.with.context,
          condition = rownames(d.sim.with.context),
          test.condition = "a",
          combination.size = 2,
          ran.trials = 500,
          use_parallel = TRUE,
          n_cores = detectCores()-1)

# save --------------------------------------------------------------------

p.no.context <- p
jp.no.context <- jp

# save for internal package data
usethis::use_data(d.sim.no.context,
                  p.no.context,
                  jp.no.context,
                  d.sim.with.context, 
                  context.def, 
                  joint.prob.matrix,
                  res.multi.core.with.context,
                  internal = TRUE, 
                  overwrite = TRUE)

