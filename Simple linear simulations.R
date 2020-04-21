set.seed(20) # Always set seed so random numbers can be recovered
a <- rnorm(1e4, mean = 6, sd = 20) # Generate some random numbers according to a normal distribution
plot(a) # Show a normal distribution
plot(density(a))

##### Simple plots first
set.seed(20)
chlorophyll <- rnorm(60, mean = 7, sd = 2) # Simulating some chlorophyll values; SD and mean come from
# Assemble experiment
plot(density(chlorophyll))

##### Now including predictors
set.seed(20)
x <-rnorm(60, mean = 7, sd = 2) # Data generating process
e <- rnorm(6, sd = 2) # Error
chlor <- x + e # Add the error and the data generating process
summary(chlor)
plot(x, chlor) # Plot these data
m1 <- lm(chlor ~ x) # Model of the relationship between x and chlorophyll
summary(m1) # Can see the model predicts they are strongly related, with an estimate of nearly 1. 
abline(m1$coef[1], m1$coef[2], col = "red")

