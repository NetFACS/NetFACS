# simulate FACS data for unit tests

# sim no context -------------------------------------------------------

# define the "truth"; ie data generating process
# 10 total AUs, 
# four common ones: 4, 7, and to a lesser extent 6
# Other AUs are rare, 10 is never active
aus <- as.character(1:10)
p <- c(0.1, 0.1, 0.1, 0.9, 0.1, 0.51, 0.9, 0.1, 0.1, 0.0)
p <- setNames(p, aus)
n_obs <- 5000

# joint probability matrix
jp <- matrix(0, nrow = length(p), ncol = length(p), 
             dimnames = list(aus, aus))
jp["4", "2"] <- 0.9 # if 4 is active, 2 has 90% prob 
jp["6", "1"] <- 0.95 # if 6 is active, 1 has 95% prob
jp["9", "10"] <- 1 # if 9 is active, 10 has 100% prob
jp["3", "8"] <- 1 # if 3 is active, 8 has 100% prob
diag(jp) <- NA

set.seed(1984)
d.sim.no.context <- sim_facs(p, jp = jp, n_obs = n_obs)

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

d.sim.with.context <- sim_facs(context.def, 
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

