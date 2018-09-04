library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25) 
# dslabs equivalent to draw random sample of 25 beads from urn of blue and red beads
# Computing estimate of std error
X_bar <- 0.48
se_hat <- sqrt(X_bar*(1 - X_bar)/25)
se_hat # approx 0.100

# Probability of being within 1% of p or population mean
pnorm(0.01/se_hat) - pnorm(-0.01/se_hat) # 0.08 or 8%

# Probability of 2 SEs from p or populaton mean
pnorm(2) - pnorm(-2) # 95%

## Monte Carlo Simulation for testing CLT on urn model
# urn model - BUT cannot run due p unknown
# B <- 10000 # beads in urn
# N <- 1000 # sample size
# X_bar <- replicate(B, {
#   X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
#   mean(X)
# })

# testing for 1 example at p = 0.45
# theory
X_bar <- 0.45
se_hat <- sqrt(X_bar*(1 - X_bar)/1000)
se_hat # approx 0.0157 or 1.6%

# monte carlo sim
B <- 10000 # beads in urn
N <- 1000 # sample size
X_bar <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-0.45, 0.45))
  mean(X)
})
sd(X_bar) # 0.0156 or 1.6%

