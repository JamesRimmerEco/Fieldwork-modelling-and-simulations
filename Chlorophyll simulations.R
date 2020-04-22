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
chlor <- b0 + b1*(group == "group2") +b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + eps # Distribution of the data

chlorgroup <- lm(chlor ~ group) # Linear model for the effect
summary(chlorgroup) 

chlorgroupaov <- aov(chlor ~ group) # For comparison purposes, run as analysis of variance so Tukey works
TukeyHSD(chlorgroupaov) # Post-hoc difference among the groups

