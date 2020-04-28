rm(list = ls())

####    First, very simple linear models

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
