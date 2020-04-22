set.seed(20) # Always set seed for reproducibility
ngroup <- 5 # The number of groups (in this case, unlevelled factors)
nrep <- 10 # Number of replicates
b0 <- 6 # Intercept - the level of group1
b1 <- 6.5
b2 <- 5.5
b3 <- 7
b4 <- 6.3
sd <- 2
group <- rep( c("group1", "group2", "group3", "group4", "group5"), each = nrep) 
eps <- rnorm(ngroup*nrep, mean = 0, sd = sd) 
chlor <- b0 + b1*(group == "group2") +b2*(group == "group3") + b3*(group == "group4") + 
  b4*(group == "group5") + eps 

chlorgroup <- lm(chlor ~ group)
summary(chlorgroup)


chlorgroupaov <- aov(chlor ~ group)
TukeyHSD(chlorgroupaov)

