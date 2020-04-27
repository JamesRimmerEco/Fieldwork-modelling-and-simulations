             ############################################################
             ####    Mixed effects model simulation with 2 levels    ####
             ############################################################
library(lme4)
# In this simulation, we take a single estuarine system and sample from it in 5 patches. In each patch, we
# conduct measurements of chlorophyll, nutrients and grazer biomass
# Nutrients are measured at the level of the patch only, grazer biomass at the level of the experimental
# plot
          
set.seed(20) # Set a reproducible seed
npatch <- 5 # The number of patches within the estuary
nexp <- 10 # The number of experimental units per patch

b0 <- 1.2 # Mean response when the nutrients and grazer biomass are at zero
b1 <- 0.05 # Change in chlorophyll when nutrients increase by 1 unit
b2 <- -0.01 # Change in chlorophyll when grazer biomass increases by 1 unit
sds <- 3 # Patch standard deviation
sd <- 2 # Observation standard deviation

patch <- rep(LETTERS[1:npatch], each = nexp) # Name each patch
patcheff <- rep(rnorm(npatch, 0, sds), each = nexp) # Random effects for each patch
expeff <- rnorm(npatch*nexp, 0, sd) # Random effects for each experimental unit

nutrients <- rep(runif(npatch, 0, 50), each = nexp) # Values for nutrients; 1 per patch, repeated for each
# of the 10 experiments

grazer <- runif(npatch*nexp, 0, 100)

chlorophyll <- b0 + b1*nutrients + b2*grazer + patcheff + expeff # Calculate the 50 responses (5 patches 
# x 10 experiments per patch)

chlordat <- data.frame(chlorophyll, nutrients, grazer)

cmm1 <- lmer(chlorophyll ~ nutrients + grazer + (1|patch), data = chlordat)
summary(cmm1)

# In this simulation, we take a single estuarine system and sample from it in 5 patches. In each patch, we
# conduct an experiment - glyphosate at 5 levels - and replicate it twice. So 10 observations per patch.
# This first example has one fixed effect - this 5 level factor


# Could we build in other sources of noise, such as salinity/rain etc, and use them in the data generating 
# process but not account for them as fixed effects?


                    ######################################################
                    ##   Fitting a categorical variable and covariate   ##
                    ######################################################

set.seed(20)
npatch <- 5
nexp <- 6

b0 <- 3 # Mean when other variables at zero or level 1 factor (control)
b1 <- -0.3 # Mean change in response for 1 unit change in nutrients
b2 <- -0.05# Difference between control and level 1
b3 <- -0.1 # Difference between control and level 2
b4 <- -0.25 # Difference between control and level 3
b5 <- -2 # Difference between control and level 4

gly <- rep(c("Control", "lvl1", "lvl2", "lvl3", "lvl4", "lvl5"),  npatch)
nutrients <- rep(rnorm(npatch*nexp, 10, 1.8))

sds <- 2
sd <- 1.5

patch <- rep(as.factor(1:npatch), each = nexp) # Name the patches;  experiment within a patch share a name
exp <- as.factor(1:(npatch*nexp)) # Names for each experiment (all unique)

patcheff <- rep(rnorm(npatch, 0, sds), each = nexp) # Patch level variation
expeff <- rnorm(npatch*nexp, 0, sd) # Observation level variation

data.frame(gly, patch, exp, patcheff, expeff) # Check that the patch variation matches up with the patches
# This isn't to create a dataframe at this point, just to check the formulation is correct

resp <- b0 + b1*nutrients + b2*(gly == "levl1") + b3*(gly == "lvl2") + b4*(gly == "lvl3") + b5*(gly == "lvl4") + patcheff + expeff

dat <- data.frame(resp, gly, patch, exp, nutrients)
head(dat)
str(dat)
dat

mm2 <- lmer(resp ~ gly + nutrients + (1|patch), data = dat)
summary(mm2)

library(rstanarm)

mm3 <- stan_lmer(resp ~ gly + nutrients + (1|patch), data = dat)
summary(mm3)
plot(mm3)

#     So to summarise, this model has variation at 2 levels - the level of the patch, and the level of the
# observation. The patch level variation was set to have a standard deviation of 2 - lme4 has estimated this 
# to be 2.47, so not too bad given 5 patches. The residial standard deviation was set to 1.5, and the model
# has returned 1.4, so not bad again. Not all the glyphosate levels have been recaptured very well, 
# however - probably too few samples. 

# The function below allows repeated running of the simulation

mixedchlor_fun <- function(npatch = 5, nexp = 6, b0 = 3, b1 = -0.3, b2 = -0.05, b3 = -0.1, b4 = -0.25, b5 = -2, sds = 2, sd = 1.5){
  gly <- rep(c("Control", "lvl1", "lvl2", "lvl3", "lvl4", "lvl5"),  npatch)
  nutrients <- rep(rnorm(npatch*nexp, 0, 1.8))
  patch <- rep(as.factor(1:npatch), each = nexp)
  exp <- as.factor(1:(npatch*nexp)) 
  patcheff <- rep(rnorm(npatch, 0, sds), each = nexp)
  expeff <- rnorm(npatch*nexp, 0, sd)
  resp <- b0 + b1*nutrients + b2*(gly == "levl1") + b3*(gly == "lvl2") + b4*(gly == "lvl3") + b5*(gly == "lvl4") + patcheff + expeff
  mm2 <- lmer(resp ~ gly + nutrients + (1|patch))
  mm2
  }

# Testing we get the same result as when the previous model was built outside the function
set.seed(20)
mixedchlor_fun()

mixedsimulations <- replicate(1e3, mixedchlor_fun(), simplify = F)

# Packages to help extract parameters
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)
#####
# How often is sigma recovered?
mixedsimulations %>% # The simulations
    map_dbl(~summary(.x)$sigma) %>% 
    data.frame(sigma=.) %>% 
    ggplot( aes(sigma) ) +
    geom_density(fill = "blue", alpha = .5) + 
    geom_vline( xintercept = 1.5)

# Recovering treatment level 1 parameter
mixedsimulations 
  map_df(tidy) 
  filter(term == "glylvl1") %>%
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + 
  geom_vline( xintercept = -0.05)

# Recovering intercept parameter
mixedsimulations %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "(Intercept)") %>% # Filtering for group2 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = 3)

# Recovering nutrients parameter
mixedsimulations %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "nutrients") %>% # Filtering for group2 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = -0.3)

# Recovering the patch level standard deviation
mixedsimulations %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "sd_(Intercept).patch") %>% # Filtering for group2 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = 2)

# Recovering the observation level standard deviation
mixedsimulations %>% # The simulations
  map_df(tidy) %>% # Turns each sim object into a tibble
  filter(term == "sd_Observation.Residual") %>% # Filtering for group2 
  ggplot( aes(estimate) ) +
  geom_density(fill = "blue", alpha = .5) + # Density plot for the estimates for group 2
  geom_vline( xintercept = 1.5)

              ###########################################################
              #### Realistic data - intercepts only, no interactions ####
              ###########################################################

# In this dataframe, there are a series of measurements (weekly measurements) and responses; the response
# variables will be affected by each of the predictors, but not all predictors necessarily have much
# of an effect (consider AIC comparison with these). We want to account for:
# a) Multiple patches
# b) Repeated measurements

# The model structure should look something like this:
# resp ~ stress 1 + stress 2 + week + (1|ID) + (1|Patch)

# More sophisticated models and simulations can include random slopes and interactions, but we haven't 
# got that far yet. 

set.seed(20)

npatch <- 5 # Number of patches at the estuary
reppatch <- 2 # How many times the entire experiment is replicated per patch
nwks <- 5 # The number of time points at which the experiment is measured
nlvl1 <- 3 # Factor levels, including control, for stressor 1
nlvl2 <- 2 # Factor levels, including control, for stressor 2

# The below numbers can be tweaked; for now, they are made up but will be made to match mesocosm 
# experiments

b0 <- 10 # Mean response when other variables zero or at level 1 factor
ba1 <- -0.005 # Difference between intercept and stress 1 lvl 1
ba2 <- -0.9 # Difference between intercept and stress 1 lvl 2
bb1 <- -1.2 # Difference between intercept and stress 2 lvl 1
bt <- 0 # The effect of temperature on the response (zero means no effect)
bp <- 0 # The effect of precipitation on the response
bt1 <- 0.2 # Difference between time 1 (intercept) and time 2 
bt2 <- -0.1 # Difference between time 1 and time 3
bt3 <- -0.2 # Difference between time 1 and time 4
bt4 <- 0.5 # Difference between time 1 and time 5

stress1 <- rep(rep(rep(c("s1Control", "s1lvl1", "s1lvl2"),  (nlvl1*nlvl2)/nlvl1), npatch*reppatch), nwks)
# Looks convuluted, but the structure allows for the number of levels in the respective treatments to be 
# altered more easily.Stress 1 is repeated in batches to line up with each combination of stress 2

stress2 <- rep(rep(rep(c("s2Control", "s2lvl1"), each = (nlvl1*nlvl2)/nlvl2), npatch*reppatch), nwks)

time <- rep(rep(as.factor(1:nwks)), each = nlvl1*nlvl2*npatch*reppatch) # Assigns a time stamp for each week, repeated for 
# each time set of experiments once per observation per patch. 
length(time)

df <- data.frame(stress1, stress2, time) # Checks that there are the correct number of combinations - for the 
# default setting, this is a design with one factor at 2 levels, 1 at 3,  each repeated twice per patch, 
# measured 5 times. Therefore, 60 observations per system per week (for each response variable). 
head(df, n = 18)
sdp <- 2 # Patch level standard deviation
sd <- 1.5 # Observation level standard deviation

patch <- rep(rep(as.factor(1:npatch), each = nlvl1*nlvl2*reppatch), nwks) # Each patch has 2 runs of the 
# experiment, i.e. 12 per patch. Each patch is then repeated across time (5 weeks). 
df <- data.frame(stress1, stress2, time, patch)

exp <- as.factor(1:(npatch*reppatch*lvl1*nlvl2*nwks)) # Names for each experiment (all unique)

patcheff <- rep(rep(rnorm(npatch, 0, sdp), each = nlvl1*nlvl2*reppatch), nwks) # Patch level variation, 
# which is consistent in time 

expeff <- rep(rnorm(npatch*nlvl1*nlvl2*reppatch, 0, sd),  nwks) # Observation level variation; these are
# consistent in time (and this helps understand why you fit replicate identity as a random effect, not
# time itself)

df <- data.frame(stress1, stress2, time, patch, patcheff, expeff)







