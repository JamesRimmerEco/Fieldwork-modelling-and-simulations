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
                    #####       Fitting a categorical variable        ####
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

gly <- rep(c("Control", "lvl1", "lvl2", "lvl3", "lvl4"),  nexp)

sds <- 2
sd <- 1.5

patch <- rep(LETTERS[1:npatch], each = nexp) # Name the patches;  experiment within a patch share a name
exp <- as.factor(1:(npatch*nexp)) # Names for each experiment (all unique)

patcheff <- rep(rnorm(npatch, 0, sds), each = npatch) # Patch level variation
expeff <- rnorm(npatch*nexp, 0, sd) # Observation level variation









