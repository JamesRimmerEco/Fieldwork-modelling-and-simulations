# Simple linear model with one explanatory variable

set.seed(20) # Always set seed for reproducibility
ngroup <- 5 # The number of groups (in this case, unlevelled factors)
nrep <- 10 # Number of replicates

b0 <- 6 # Intercept - the level of group1
b1 <- 1.5 # Difference between group1 and group2
b2 <- -0.5 # etc
b3 <- 1
b4 <- 0.3
sd <- 2 # Overall SD

group <- rep( c("group1", "group2", "group3", "group4", "group5"), each = nrep) # A vector of group names 
# repeated for each replicate 
eps <- rnorm(ngroup*nrep, mean = 0, sd = sd) # Error term
chlor <- b0 + b1*(group == "group2") + b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + eps # Distribution of the data

chlorophyll <- data.frame(chlor, group) # Shows the results as a dataframe
str(chlorophyll)

m1 <- lm(chlor ~ group, data = chlorophyll) # Test the effect of group on chlorophyll
summary(m1) # Are the parameters well recovered? Not perfect, but not terrible

with(chlorophyll, boxplot(chlor~group)) # Visual aid as a boxplot

chlorgroupaov <- aov(chlor ~ group, data = chlorophyll) # For comparison purposes, run as analysis of variance so Tukey works
TukeyHSD(chlorgroupaov) # Post-hoc difference among the groups

#### How well does the model perform over the long term?
# Packages 
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)
# Create a function to run the model repeatedly

fivegroupfactorfun <- function(nrep = 10, b0 = 6, b1 = -1.5, b2 = 0.5, b3 = 1, b4 = 0.3, sigma = 2){
  ngroup = 5
  group = rep(c("group1", "group2", "group3", "group4", "group5"), each = nrep)
  eps = rnorm(ngroup*nrep, 0, sigma)
  chlor = b0 + b1*(group=="group2") + b2*(group == "group3") + b3*(group == "group4") + 
    b4*(group == "group5") + eps
  chlormod = lm(chlor ~ group)
  chlormod
}

simulations <- replicate(1e3, fivegroupfactorfun(), simplify = F)

# How often do we recover the SD?

sims %>% # The simulations
  map_dbl(~summary(.x)$sigma) %>% 
  data.frame(sigma=.) %>% 
  ggplot( aes(sigma) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 2)

sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  {. < 2} %>%
  mean() # Tell us how often the SD is underestimated



