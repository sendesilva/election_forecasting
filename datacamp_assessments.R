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

### A6.1
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that columns for the lower and upper confidence intervals.
# Select the columns indicated in the instructions.
# X_hat = estimate of proportion of Clinton supporters
cis <- polls %>% 
  mutate(X_hat = (spread + 1)/2, se = sqrt(X_hat*(1-X_hat)/samplesize), 
         lower = spread - 2*qnorm(.975)*se, upper = spread + 2*qnorm(.975)*se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
head(cis)

# Ex2
# Add the actual results to the `cis` data set 
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(cis)

# Create an object called `p_hits` that summarizes the proportion of confidence intervals 
# that contain the actual value. Print this object to the console.
p_hits <- cis %>% mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  summarise(proportion_hits = mean(hit))
p_hits


# Ex3
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>%
  select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster 
# that has more than 5 polls. (errors: want 5 or more; also wants column to be only 'n')
p_hits <- cis %>%
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  group_by(pollster) %>% 
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit), n = n(), grade = first(grade)) %>%
  arrange(desc(proportion_hits))
p_hits

# Ex4
# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- cis %>%
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))
p_hits

# Ex6
# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted 
# and actual spread and indicates if the correct winner was predicted
errors <- cis %>% 
  mutate(error = spread - actual_spread, hit = (spread > 0 & actual_spread > 0) | 
           (spread < 0 & actual_spread < 0) )

# Examine the last 6 rows of `errors`
tail(errors)

# Ex7
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls
p_hits <- errors %>% group_by(state) %>% filter(n() >= 5) %>%
  summarise(proportion_hits = mean(hit), n = n()) 


# Make a barplot of the proportion of hits for each state
library(ggplot2)
ggplot(p_hits, aes(x = state, y = proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Ex9
# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("B+", "A-", "A", "A+")) %>%
  mutate(state = reorder(state, error)) %>% ggplot(aes(x=state, y=error)) +
  geom_boxplot() +
  geom_point()


### A6.2
# Ex1 Exercise 1 - Using the t-Distribution: # We know that, with a normal distribution, 
# only 5% of values are more than 2 standard deviations away from the mean. Calculate the 
# probability of seeing t-distributed random variables being more than 2 in absolute value 
# when the degrees of freedom are 3.

qnorm(0.975) # 1.96 or approx 2 sd
qt(0.975, df=14) # 2.144
1 - pnorm(2) + pnorm(-2) # 0.0455

1- pt(2, df=3) + pt(-2, df=3) # same as for pnorm > 1 - pt + pr = 0.14

# Ex2
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50, by = 1)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df) 1 - pt(2, df) + pt(-2, df)
  
  # Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(probs, df)

# Ex3
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, size = N, replace = TRUE)
  interval <- c(mean(X) - qnorm(0.975)*sd(X)/sqrt(N), mean(X) + qnorm(0.975)*sd(X)/sqrt(N))
  between(mu,interval[1], interval[2])
})
mean(res)

# Ex.4 - repeat Ex3 using t-dist
res <- replicate(B, {
  X <- sample(x, size = N, replace = TRUE)
  interval <- c(mean(X) - qt(0.975, df = N-1)*sd(X)/sqrt(N), mean(X) + qt(0.975, df = N-1)*sd(X)/sqrt(N))
  between(mu,interval[1], interval[2])
})
mean(res)


