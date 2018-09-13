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
# plot histogram to check if distribution is normal
hist(X_bar) # appears to be normal distribution

# DC Ex1
# Write a function called `take_sample` that takes `p` and `N` as arguements and 
#returns the average value of a randomly sampled population.
take_sample <-  function(p, N) {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p)) 
  mean(X)
  }


# Use the `set.seed` function to make sure your answer matches the expected result after 
# random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly 
#selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p, N)

# DC Ex2
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result 
#after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the 
#`take_sample` function from `p` for `B` replications
errors <- replicate(B, {
  p - take_sample(p, N)
})

# Calculate the mean of the errors. Print this value to the console.
mean(errors) # -4.9e-05


# Ex8 plotting SE vs N
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N, se)


# Ex11 make a qq-plot of errors to see if they flw a normal distribution
qqnorm(errors)
qqline(errors)


# Ex12 estimating prob of x_bar > 0.5
p <- 0.45
N <- 100
X_bar <- p
se_hat <- sqrt(X_bar*(1-X_bar)/N)
se_hat
1 - pnorm((0.5-0.45)/se_hat) # 1 - prob of X_bar <= 0.5 =  0.1574393
# actually prob x-bar < 0.5 ----- 1 - ccc = prob >= 0.5

# Ex13 estimating prob of specific error size
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Calculate the probability that the error is 0.01 or larger
1 - pnorm(0.01/se_hat) + pnorm(-0.01/se_hat) # 0.8414493

# grader wants:
# # Define `N` as the number of people polled
# N <-100
# 
# # Define `X_hat` as the sample average
# X_hat <- 0.51
# 
# # Define `se_hat` as the standard error of the sample average
# se_hat <- sqrt(X_hat*(1-X_hat)/N)
# 
# # Calculate the probability that the error is 0.01 or larger
# 1 - pnorm(0.01, mean = 0, sd = se_hat) + pnorm(-0.01, mean = 0, sd = se_hat)


# 0.8414493


## Confidence Intervals
# from geom_smooth
data("nhtemp")
data.frame(year=as.numeric(time(nhtemp)), temperature=as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

# Monte Carlo simulation
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

# prob of p between 2 SE_hat
pnorm(2) - pnorm(-2) # approx 95%

# qnorm vs pnorm
qnorm(.975) #  1.959964 = z for 95% CI
qnorm(.995) # 2.575829 = Z for 99% CI
pnorm(2.575829) # 0.995
pnorm(2.575829) - pnorm(-2.575829) # 99% CI
pnorm(qnorm(0.995)) # 0.995
# pnorm(1 - qnorm(0.995)) # 0.05753257 ERROR code
pnorm(qnorm(1 - 0.995)) # 0.05

# Monte Carlo sim for CIs
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)
})
mean(inside)

# Power
# CI for spread = 2p - 1 >> 2p - 1 +/- 2SE
N <- 25
X_hat <- 0.48
(2*X_hat -1) + c(-2,2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N)) 

# P-Values - is the spread bigger than 0: Null Hypothesis = spread is 0
# for spread of 0.04 what is proability of spread being greater than 2 and
# including 0?
N <- 100
z <- sqrt(N)*0.02/0.5
1 - (pnorm(z) - pnorm(-z)) # 0.6891565 - hence cannot reject Null Hypothesis


### Statistical Models
## Poll Aggretators
# Monte Carlo simulation using 2012 election 3.9% as spread between the 2 candidates
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
# mimics actual poll sizes
p <- (d+1)/2 # proportion of people voting for 'Obama'

confidence_intervals <- sapply(Ns, function(N) {
  # X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p)) # using size N
  X <- sample(c(0,1), size=Ns, replace=TRUE, prob=c(1-p, p)) # using size Ns
  X_hat <- mean(X) # proportion of people voting for Obama in each sample
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1 # CI for spread?
})

polls <- data.frame(poll=1:ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_size=Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# aggregating the polls
sum(polls$sample_size)
d_hat <- polls %>%
  summarise(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe # 0.018
round(d_hat*100, 1)
round(moe*100, 1)

# Pollsters and multilevel models
# Poll Data and Pollster Bias
data(polls_us_election_2016)
names(polls_us_election_2016)

# Filtering in only national polls, on or after Oct 31 2016 and 
# grade A or better and ungraded
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" & 
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

d_hat <- polls %>%
  summarise(d_hat = sum(spread*samplesize) / sum(samplesize)) %>%
  .$d_hat

d_hat

p_hat <- (d_hat+1)/2

# margin of error - 'std error' on spread on all polls as 1 giant poll
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of polls
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color = "black", binwidth = .01)

# pollster table - summarise(n()) shows number of occurrences of pollster
polls %>% group_by(pollster) %>% summarise(n())

# limit to pollsters' who polled more than 6 times
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# std errors
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarise(se = 2*sqrt(p_hat*(1-p_hat)/median(samplesize)))



### Data-driven Models
# collect pollster's last result and show histogram
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
one_poll_per_pollster %>%
  ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01)

# sd for spread where poll has contnuous p
sd(one_poll_per_pollster$spread)

# confidence interval
results <- one_poll_per_pollster %>%
  summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)



### Bayesian Stats
# Monte Carlo simulation to investigate test for cystic fibrosis
prev <- 0.00025 # prevalence of disease
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
N_D <- sum(outcome == "Disease") # no. with disease
N_D
N_H <- sum(outcome == "Healthy") # no. healthy
N_H

# investigate false positives
accuracy <- 0.99
test <- vector("character", N)
test[outcome=="Disease"] <- sample(c("+", "-"), N_D, replace = TRUE, prob = c(accuracy, 1 - accuracy))
test[outcome=="Healthy"] <- sample(c("-", "+"), N_H, replace = TRUE, prob = c(accuracy, 1 - accuracy))

table(outcome, test)



### Election Forecasting
# Compute posterior distribution for spread d to report probablity > 0 given observed poll data.
mu <- 0 # expected value
tau <- 0.035 # based of past observed data
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/ (1/sigma^2 + 1/tau^2))

posterior_mean # 0.02808534
posterior_se # 0.006149604

# credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of spread being > 0
1 - pnorm(0, posterior_mean, posterior_se) # 0.9999975

# simulating 6 data points for polls with N = 2000 and d = .021
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2 # prob of clinton success over trum d = clin - trum
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

# simulating data points J polls carried out by I pollsters 
I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2 # prob of clinton success over trum d = clin - trum
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
 })

datx <- stack(as.data.frame(X))
head(datx)

ggplot(datx, aes(x = ind, y = values)) + geom_point()

# adding house effect for pollster variability 
I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2 # emulate prob of clinton success over trum d = clin - trum
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

# adding general bias b = 0.025
mu <- 0 # expected value
tau <- 0.035 # based of past observed data
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/ (1/sigma^2 + 1/tau^2))

# probability of spread being > 0
1 - pnorm(0, posterior_mean, posterior_se) # 0.8174373

## predicting the Electoral College
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarise(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

results %>% arrange(abs(avg))

str(results)
# left_join results / results_us_election
results <- left_join(results, results_us_election_2016, by = "state")
str(results)

# no polls in flwg states
results_us_election_2016 %>% filter(!state %in% results$state)

# adding std deviation to the states where reqd
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
str(results)

# Monte Carlo sim using Bayesian calculation with tau = 0.02
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2 / (sd^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1/(1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
str(results)

# plotting posterior_mean vs avg

results %>% mutate(sigma = sd/sqrt(n), 
                   B = sigma^2 / (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2))) %>%
  ggplot(aes(avg, posterior_mean, size = n)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

# repeat 10000 times and only keep Clinton electoral votes; 
# add 7 for Rhodes and DC which have no polls but clinton is usre to win
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton)) %>% 
    .$clinton + 7## 7 for Rhode Island and D.C.
})
mean(clinton_EV>269) # 0.999

# histogram
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# modifying bias to reflect larger bias at state level
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton)) %>% 
    .$clinton + 7## 7 for Rhode Island and D.C.
})
mean(clinton_EV>269) # 0.835


## Forecasting
# investigating effect of time on polls results 
# to remove pollster variability only data from only 1 pollster - ipsos - is used
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# comparing theoretical std error with data-driven sd
se <- one_pollster %>%
  summarise(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

one_pollster %>% ggplot(aes(spread)) + 
  geom_histogram(binwidth = 0.01, color = "black") # spread does not appear normally distributed

# pollster vs time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) + 
  geom_smooth(method = "loess", span = 0.1) + 
  geom_point(aes(color=pollster), show.legend = FALSE, alpha=0.6) 

# plotting candidate trendlines
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>% 
  mutate(candidate = factor(candidate, levels = c("Trump","Clinton")))%>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +  
  geom_point(show.legend = FALSE, alpha=0.4)  + 
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))

