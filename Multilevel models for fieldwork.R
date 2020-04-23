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
