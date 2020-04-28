rm(list = ls())

####                          First, very simple linear models

set.seed(20) # Something to note here - through chance, the first stated mixed model fit without issue, but
# other seeds run into singular fit problems. 

b0 <- 50 # Response intercept
b1 <- 5
bt <- 1.5
nexp <- 45
ntime <- 5
stress <- rnorm(nexp, 0, 3) # Distribution of some measured stress predictor
time <- rep((1:ntime), each = nexp/ntime)
sd <- 2

obs <- rep(as.factor(1:(nexp/ntime)), ntime)
obseff <- rnorm(45, 0, sd)

df <- data.frame(stress, time, obs, obseff)
df

resp <- b0 + b1*stress + bt*time + obseff

df <- data.frame(df, resp)
df

m1 <- lm(resp ~ stress + time, data = df)
summary(m1)
# The problem with this model is that it doesn't account properly for multiple measurements of each 
# replicate unit...so...
library(lme4)

m1x <- lmer(resp ~ stress + time + (1|obs), data = df)
summary(m1x)
# We see that each observation in time has its own intercept - see below

# Plotting these:
library(sjPlot)
library(sjmisc)
# Let's show the random intercepts for the model
ranef(m1x)
# And the fixed effects (for completeness)
fixef(m1x)
# Plotting the effects
plot_model(m1x) # Plot the fixed effects
plot_model(m1x, type = "re") # Plot the random effects
plot_model(m1x, type = "pred", terms = c("time", "obs[1:9]"), pred.type = "re") # Plot each intercept 
# which all share the same global slope. Observations 5 and 7 are the most negative and overlap, as can
# be seen if the observations are plotted individually.
# This global slope value, from the fixed effects, is 1.441...(etc) for time
# We can do the same for stress:
plot_model(m1x, type = "pred", terms = c("stress", "obs[1:9]"), pred.type = "re") 
# And we see here that the global stress slope is 4.92...(etc) from the fixed effects

# Can also pull parameters out like this:
tidy(m1x, effects = "fixed")
tidy(m1x, effects = "ran_pars", scales = "vcov")


# Let's extract run multiple simulations and extract some parameters
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)
# Function to repeatedly run the simulation:
stresstempo_fun <- function(nexp = 45, ntime = 5, b0 = 50, b1 = 5, bt = 1.5){
  stress <- rnorm(nexp, 0, 3)
  time <- rep((1:ntime), each = nexp/ntime)
  sd <- 2
  obs <- rep(as.factor(1:(nexp/ntime)), ntime)
  obseff <- rnorm(nexp, 0, sd)
  df <- data.frame(stress, time, obs, obseff)
  df
  resp <- b0 + b1*stress + bt*time + obseff
  df <- data.frame(df, resp)
  df
  m1 <- lm(resp ~ stress + time, data = df)
  m1
}

sims <- replicate(1e3, stresstempo_fun(), simplify = F)

# Recover standard deviation
sims %>% 
  map_dbl(~summary(.x)$sigma) %>% 
  data.frame(sigma=.) %>% 
  ggplot( aes(sigma) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 2)

# Recover stress
sims %>%
  map_df(tidy) %>%
  filter(term == "stress") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 5)

# Recover time
sims %>%
  map_df(tidy) %>%
  filter(term == "time") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 1.5)

#  But what about the effect of sample size?
# Let's try a few sizes:

stresstempo_fun90 <- function(nexp = 90, ntime = 5, b0 = 50, b1 = 5, bt = 1.5){
  stress <- rnorm(nexp, 0, 3)
  time <- rep((1:ntime), each = nexp/ntime)
  sd <- 2
  obs <- rep(as.factor(1:(nexp/ntime)), ntime)
  obseff <- rnorm(nexp, 0, sd)
  df <- data.frame(stress, time, obs, obseff)
  df
  resp <- b0 + b1*stress + bt*time + obseff
  df <- data.frame(df, resp)
  df
  m1 <- lm(resp ~ stress + time, data = df)
  m1
}

sims90 <- replicate(1e3, stresstempo_fun90(), simplify = F)
sims90 %>%
  map_df(tidy) %>%
  filter(term == "stress") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 5)

stresstempo_fun450 <- function(nexp = 450, ntime = 5, b0 = 50, b1 = 5, bt = 1.5){
  stress <- rnorm(nexp, 0, 3)
  time <- rep((1:ntime), each = nexp/ntime)
  sd <- 2
  obs <- rep(as.factor(1:(nexp/ntime)), ntime)
  obseff <- rnorm(nexp, 0, sd)
  df <- data.frame(stress, time, obs, obseff)
  df
  resp <- b0 + b1*stress + bt*time + obseff
  df <- data.frame(df, resp)
  df
  m1 <- lm(resp ~ stress + time, data = df)
  m1
}

sims450 <- replicate(1e3, stresstempo_fun450(), simplify = F)
sims450 %>%
  map_df(tidy) %>%
  filter(term == "stress") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 5)

# Sample size increasing does help with the estimation

# Given these simulations, how often do we detect an effect?

sims %>%
  map_df(tidy) %>%
  filter(term == "stress") %>%
  pull(p.value) %>%
  {. <  0.05} %>%
  mean()

# This code is putting the parameters into a table from the model, filtering for stress, extracting the 
# p value for each model, and finding out what proportion is below an alpha of 0.05. In this case,
# 100% of the models correctly detect a significant effect (i.e. reject a null of no effect of stress on
# the response. 