### A3.1
library(dslabs)
data("polls_us_election_2016")
str(polls_us_election_2016)

## Ex1
polls <- filter(polls_us_election_2016, state == "U.S." & enddate >= "2016-10-31")
head(polls)
nrow(polls)

# assign sample size of 1st poll to variable N
N <- polls$samplesize[1]
N
class(N)
# % clinton supporters in rawpoll
X_hat <- polls$rawpoll_clinton[1]/100
X_hat

# se of X_hat
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# calculate 95% CI using qnorm and store in variable ci, add lower CI first
ci <- c()
ci[1] <- X_hat - qnorm(0.975) * se_hat
ci[2] <- X_hat + qnorm(0.975) * se_hat

# ci <- c((X_hat)-qnorm(0.975) * se_hat,(X_hat)+qnorm(0.975) * se_hat)
ci

## Ex2
head(polls)
library(dplyr)
library(tidyverse)
# Create a new object called `pollster_results` that contains columns for pollster name, 
# end date, X_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>% 
  mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/polls$samplesize),
         lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>%
  select(pollster,enddate, X_hat, lower, upper)

polls <- polls %>% 
  mutate(X_hat = polls$rawpoll_clinton/100, 
         se_hat = sqrt(X_hat*(1-X_hat)/polls$samplesize), 
         lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) 

pollster_results <- select(polls, pollster,enddate, X_hat, lower, upper)

## Ex3 What proportion of CI included p?
p <- 0.482

avg_hit <- pollster_results %>%
  mutate(hit = lower<=0.482 & upper>=0.482) %>%
  summarize(mean(hit))

avg_hit # mean(hit) 0.3142857



### A4.1
## Popn mean >> mean(x); popn sd >> sqrt(mean((X - mean(x))^2))
## sample mean >> mean(X); sample sd = sd
# testing sample funtion
N <- 25
x <- c(10, 20 ,30, 40, 50)
sample(x, size = N, replace = TRUE)
x

