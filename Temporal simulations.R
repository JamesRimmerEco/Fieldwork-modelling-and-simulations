rm(list = ls())

####                          First, very simple linear models

set.seed(20)

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

# Let's extract run multiple simulations and extract some parameters
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)
