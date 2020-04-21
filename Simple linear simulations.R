set.seed(20) # Always set seed so random numbers can be recovered
a <- rnorm(1e4, mean = 6, sd = 20) # Generate some random numbers according to a normal distribution
plot(a) # Show a normal distribution
plot(density(a))

##### Simple distribution plot first using 'real' values
set.seed(20)
chlorophyll <- rnorm(60, mean = 7, sd = 2) # Simulating some chlorophyll values; SD and mean come from
# Assemble experiment
plot(density(chlorophyll))

##### Now including an error term
set.seed(20)
x <-rnorm(60, mean = 7, sd = 2) # Data generating process
e <- rnorm(6, sd = 2) # Error
chlor <- x + e # Add the error and the data generating process
summary(chlor)
plot(x, chlor) # Plot these data
m1 <- lm(chlor ~ x) # Model of the relationship between x and chlorophyll
summary(m1) # Can see the model predicts they are strongly related, with an estimate of nearly 1. 
abline(m1$coef[1], m1$coef[2], col = "red")

#####
set.seed(20)
x <-rnorm(60, mean = 7, sd = 2) # Data generating process
e <- rnorm(6, sd = 2) # Error
chlor <- x + e # Add the error and the data generating process
summary(chlor)
plot(x, chlor) # Plot these data
m1 <- lm(chlor ~ x) # Model of the relationship between x and chlorophyll
summary(m1) # Can see the model predicts they are strongly related, with an estimate of nearly 1. 
abline(m1$coef[1], m1$coef[2], col = "red")

##### Next, need simulate with different groups
# Source: https://aosmith.rbind.io/2018/01/09/simulate-simulate-part1/

set.seed(16)
ngroup <- 2
nrep <- 10
b0 <- 5
b1 <- -2
sd <- 2
group <- rep( c("group1", "group2"), each = nrep) # 10 group 1s, 10 group 2s
eps <- rnorm(ngroup*nrep, mean = 0, sd = sd) # Error term for each observation
growth <- b0 + b1*(group == "group2") + eps # Create the simulated response; growth is now given by a true
# group mean for group1 (b0), a mean for group2 which is 2 less than group1 (b1 = -2; -2*group 2 in 
# formula), and an error epsilon (eps).

growthfit <- lm(growth ~ group)
summary(growthfit)

# This is one model, but we want to know how it functions over the long run. So let's create (borrow) a 
# function to do this:

twogroup_fun <- function(nrep = 10, b0 = 5, b1 = -2, sigma = 2){
  ngroup = 2
  group = rep(c("group1", "group2"), each = nrep)
  eps = rnorm(ngroup*nrep, 0, sigma)
  growth = b0 + b1*(group=="group2") + eps
  growthfit = lm(growth ~ group)
  growthfit
}

# This function is using the same parameters as above. 

# Now test the function

set.seed(16)
twogroup_fun()
twogroup_fun(sigma=1) # Example of how to change an element of the function

### Now repeat the function

sims <- replicate(1e3, twogroup_fun(), simplify = F)

#####
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)
#####
tidy(growthfit) # Model coefficients from the single run
summary(growthfit)$sigma # Standard deviation only (doesn't need broom)
sims %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "groupgroup2") %>% # Filtering for group2 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = -2) # Puts a line for where the actual difference between group1 and group2 was
# specified

# When looking at this distribution, estimates generally lie close to the true value but there is some 
# uncertainty here; a small number even gave positive estimates. 

# Now let's do the same for for standard deviation (sigma)
sims %>% # The simulations
  map_dbl(~summary(.x)$sigma) %>% 
  data.frame(sigma=.) %>% 
  ggplot( aes(sigma) ) +
    geom_density(fill = "blue", alpha = .5) + 
    geom_vline( xintercept = 2)

# Again, not far off

sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  {. < 2} %>%
  mean()

# The standard deviation is underestimated a little over half the time. 


### Can I now use this framework to sam














