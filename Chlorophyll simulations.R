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

fivegroupfactorfun <- function(nrep = 10, b0 = 6, b1 = -1.5, b2 = -0.5, b3 = 1, b4 = 0.3, sigma = 2){
  ngroup = 5
  group = rep(c("group1", "group2", "group3", "group4", "group5"), each = nrep)
  eps = rnorm(ngroup*nrep, 0, sigma)
  chlor = b0 + b1*(group=="group2") + b2*(group == "group3") + b3*(group == "group4") + 
    b4*(group == "group5") + eps
  chlormod = lm(chlor ~ group)
  chlormod
}

set.seed(20)
simulations <- replicate(1e3, fivegroupfactorfun(), simplify = F)

# How often do we recover the SD?

simulations %>% # The simulations
  map_dbl(~summary(.x)$sigma) %>% 
  data.frame(sigma=.) %>% 
  ggplot( aes(sigma) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = 2)

simulations %>%
  map_dbl(~summary(.x)$sigma) %>%
  {. < 2} %>%
  mean() # Tell us how often the SD is underestimated

# And the intercept parameter

simulations %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "(Intercept)") %>% # Filtering for Intercept 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = 6)

# And one of the other groups

simulations %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "groupgroup3") %>% # Filtering for group2 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = -0.5)

#### Now simulating with a continuous, rather than categoric, predictor
set.seed(20) 
sd <- 2 # Overall standard deviation
eps <- rnorm(100, 0, sd) # Error term
nutrients <- runif(100, 0, 100) # Variation of the predictor nutrients
b0 <- 2 # Chlorophyll when nutrients are zero
b1 <- 0.2 # How much each unit of nutrients increases chlorophyll by, the slope

chlor <- b0 + b1*nutrients + eps # Generating chlorophyll values
chlorophyll <- data.frame(chlor, nutrients) # Dataframe of prediction and predictor

m2 <- lm(chlor~nutrients, data = chlorophyll) # Model these data
summary(m2) # Model outputs for the intercept and nutrients are very good

plot(chlor ~ nutrients, data = chlorophyll) # Plot the raw data
abline(m2$coef[1], m2$coef[2], col = "red") # Plot the model line


                ################################################################################
                ####        Model with both a continuous and a categorical predictor        ####
                ################################################################################
set.seed(20)

sd <- 2

nstress <- 2
nrep <- 45

eps <- rnorm(nrep, 0, sd)
glyphosate <- rep( c("Yes", "No"), each = nrep)
nutrients <- runif(nrep, 0, 100)

b0 <- 2
b1 <- 0.2
b2 <- rnorm(nrep, -10, 2) 

chlor <- b0 + b1*nutrients + b2*(glyphosate == "Yes") + eps 
chlorophyll <- data.frame(chlor, nutrients, glyphosate) # Dataframe of prediction and predictor
plot(chlorophyll)

m3 <- lm(chlor ~ nutrients + glyphosate, data = chlorophyll)
summary(m3)

          ################################################################################
          ####                            More realistic simulation                   ####
          ################################################################################

# The nutrients above weren't particularly realistic in the same way height and weight measurements
# might be (would you really get a uniform distribution of nutrients measurements in an experiment/
# fielwork?)

set.seed(20)

sd <- 2
nstress <- 2
nrep <- 50

eps <- rnorm(nrep, 0, sd)
glyphosate <- rep( c("Yes", "No"), each = nrep/2)
rETRmax <- rnorm(nrep, 5, 2)

b0 <- 2
b1 <- 1.2
b2 <- rnorm(nrep, -10, 2) 

chlor <- b0 + b1*rETRmax + b2*(glyphosate == "Yes") + eps 
chlorophyll <- data.frame(chlor, rETRmax, glyphosate) # Dataframe of prediction and predictor
plot(chlorophyll, col = chlorophyll$glyphosate)

m3 <- lm(chlor ~ rETRmax + glyphosate, data = chlorophyll)
summary(m3)

plot(chlor ~ rETRmax, data = chlorophyll, col = chlorophyll$glyphosate)
abline(m3$coef[1], m3$coef[2], col = "black")
abline(m3$coef[1]+m3$coef[3], m3$coef[2], col = "red")

coef(m3)


                    ############################################
                    ####        Multilevel simulation       ####
                    ############################################

# Start using an example for trees and plots https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/
# There are trees within plots, and plots within stands (4 plots per stand). We're not measuring 
# individual trees

set.seed(16)

nstand = 5 # Number of stands
nplot = 4 # Number of plots per stand
mu = 10 # True mean
sds = 2 # Standard deviation at the level of the stand
sd = 1 # Standard deviation at the observation level

stand <- rep(LETTERS[1:nstand], each = nplot) # Create a variable containing a unique name for each of 
# sampled stands (capital letters); each is repeated four time (nplot) because there were four plots in 
# each stand

plot <- letters[1:(nstand*nplot)] # Creating a variable of unique names for each plot; this isn't 
# strictly necessary for the modelling as there is a single value per plot, but it's good practice

standeff <- rnorm(nstand, 0, sds) # Stand-level random effects; one per stand. Each plot within the stand
# has the same stand effect (i.e. the corresponding one of the 5 values). So stand therefore has a 
# response variable which is either higher or lower than the others according to the value of this random
# effect, and this needs to be encapsulated for each plot

standeff <- rep(standeff, each = nplot) # The stand effect needs to be repeated four each plot within
# each stand

ploteff <- rnorm(nstand*nplot, 0, sd) # Each unique plot measurement has its own observation-level random
# effect, which is drawn in this case from a normal distribution. 20 draws are needed here, one for each
# plot, so 20 random observation effects

dat <- data.frame(stand, standeff, plot, ploteff) # Pull all the variables into a single dataframe; helps
# illustrate the individual stand effects, and the individual plot effects

# We now have a dataframe with effects at two levels - the plot level and the stand level (remember,
# plot > stand, with plot being an observation).

# We now need to combine this to create a simulated response variable. Let's call this resp and add it
# to the dataframe

dat$resp <- with(dat, mu + standeff + ploteff)

# Now we have 20 responses 

### Fitting the model

library(lme4)

mm1 <- lmer(resp ~ 1 + (1|stand), data = dat)
summary(mm1)

# So this model is estimating an overall mean (because there are no predictor parameters per se), and the 
# random effects. Are they close to what we defined them as?
# True mean is pretty close, as are the standard deviation of the stand effect (st dev random effect), and
# the standard deviation of the residuals (1), which represents the observational standard deviation.

# Remember, one can change these parameter values to check that the model recapatures them effectively. 


#### Now testing a simulation over the longer term, to see how the model behaves
# I've increased the number of stands and plots because lme4 was having difficulty fitting the model
# in some cases

multilevel_fun = function(nstand = 10, nplot = 5, mu = 10, sigma_s = 2, sigma = 1) {
  standeff = rep( rnorm(nstand, 0, sigma_s), each = nplot)
  stand = rep(LETTERS[1:nstand], each = nplot)
  ploteff = rnorm(nstand*nplot, 0, sigma)
  resp = mu + standeff + ploteff
  dat = data.frame(stand, resp)
  lmer(resp ~ 1 + (1|stand), data = dat)
}

simulations <- replicate(100, multilevel_fun(), simplify = F)

# Now to extract parameters of interest
library(broom)
tidy(mm1, effects = "fixed")
tidy(mm1, effects = "ran_pars", scales = "vcov")

#####         Now let's see how well we can estimate variance according to sample size       ######
# We're going to compare 5, 20 and 100 stands. The packages help with data manipulation and plotting

library(purrr)
library(dplyr)
library(ggplot2)

stand_sims = c(5, 20, 100) %>%
  set_names() %>%
  map(~replicate(1000, multilevel_fun(nstand = .x) ) ) # We're looping through a vector of the 3 stand sizes
# and fitting a model 1000 times to each sample size. There will be some warning messages, e.g. about singular 
# fit, and the process will take a while. In reality, would need to address this

# We're now going to extract the variance of the stand effect for each model using tidy
stand_vars = stand_sims %>%
  modify_depth(2, ~tidy(.x, effects = "ran_pars", scales = "vcov") ) %>%
  map_dfr(bind_rows, .id = "stand_num") %>%
  filter(group == "stand")
head(stand_vars)

# Let's order the factor levels correctly
stand_vars = mutate(stand_vars, stand_num = forcats::fct_inorder(stand_num) )

# And some clearer labels
add_prefix = function(string) {
  paste("Number stands:", string, sep = " ")
}
# Add the median of each distribution as a second vertical line 
groupmed = stand_vars %>%
  group_by(stand_num) %>%
  summarise(mvar = median(estimate) )

ggplot(stand_vars, aes(x = estimate) ) + 
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~stand_num, labeller = as_labeller(add_prefix) ) +
  geom_vline(aes(xintercept = 4, linetype = "True variance"), size = .5 ) +
  geom_vline(data = groupmed, aes(xintercept = mvar, linetype = "Median variance"),
             size = .5) +
  theme_bw() +
  scale_linetype_manual(name = "", values = c(2, 1) ) +
  theme(legend.position = "bottom",
        legend.key.width = unit(.1, "cm") ) +
  labs(x = "Estimated Variance", y = NULL)

# We can see quite clearly with 5 stand that there is a lot more uncertainty about the true variance. 


            #############################################################################
            ####        Now a mixed model with both fixed and random effects         ####
            #############################################################################

# We can have variables which are measured at either level of the model. Stand level measurements are
# shared for all plots at that stand; plot level variables could be different for every plot

set.seed(16)
nstand <- 5 # Number of stand (2nd level)
nplot <- 4 # Number of plots (1st level)
b0 <- -1 # Mean response when the elevation and slope variables are 0
b1 <- 0.005 # Change in mean response for a 1 unit change in the variable elevation
b2 <- 0.1 # Change in mean response for a 1 unit change in the variable slope
sds <- 2 # Stand standard deviation
sd <- 1 # Standard deviation of observations

stand <- rep(LETTERS[1:nstand], each = nplot) # Stand names
standeff <- rep(rnorm(nstand, 0, sds), each = nplot) # Plot names
ploteff <- rnorm(nstand*nplot, 0, sd) # Random observation effects

elevation <- rep(runif(nstand, 1000, 1500), each = nplot) # Values for elevation; 1 per stand so 5 values
# (5 stands), repeated for each of the 4 plots

slope <- runif(nstand*nplot, 2, 75) # The slope variables are unique to each plot

resp2 <- b0 + b1*elevation + b2*slope + standeff + ploteff # Calculate the 20 responses (5 stand x 
# 4 plots per stand)

# Now we can fit a mixed model for the response in which elevation and slope are fixed effects and 
# stand is a random effect. 


dat <- data.frame(resp2, stand, standeff, plot, ploteff, elevation, slope)
dat
dat2 <- data.frame(resp2, stand, elevation, slope) # Just to clarify that you don't need the random effect
# variations in the dataframe itself

mm2 <- lmer(resp2 ~ elevation + slope + (1|stand), data = dat)
mm3 <- lmer(resp2 ~ elevation + slope + (1|stand), data = dat2)










