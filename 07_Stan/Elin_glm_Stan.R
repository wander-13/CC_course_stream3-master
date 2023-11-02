# Generalised Linear Models in Stan
# By: Elin Swank
# 11/02/2023
# Coding Club Tutorial 
# Created by: Gergana and Maxwell Farrell
################################################################################

# Load libraries ----
library(rstanarm)
library(brms)  # for models
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidybayes)
library(modelr)

# Load data ----
# Remember to set your working directory to the folder
# where you saved the workshop files
toolik_richness <- read.csv("07_Stan/toolik_richness.csv")

# Inspect data
head(toolik_richness)

str(toolik_richness)

# change variable type to reflect reality
toolik_richness$Plot <- as.factor(as.character(toolik_richness$Plot))

# explore distributions
(hist <- ggplot(toolik_richness, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())
### data are right-skewed, poisson distribution may be suitable for the model

# enabe parallel computing between cores
options(mc.cores = parallel::detectCores())

# Example Stan code for model production
# Note - this code could take hours to run!
# We are running the model using the default weakly informative priors.
# More about priors coming later in the tutorial!
# stan_lm <- stan_glmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot),
#                     data = toolik_richness, family = poisson,
#                     chains = 4, cores = 4)

# Assess converge by looking at the trace plots
# plot(stan_lm, plotfun = "trace")

# Explore the summary output
# summary(stan_lm)

# check years of data
unique(toolik_richness$Year)

# for modeling purposes: so that the model doesn't estimate richness for 2,007 years
# but starts estimates at the first year of monitoring
# transform the year into a continuous variable starting at one

# Note how now we are using stan_glm because
# there are no random effects
stan_glm1 <- stan_glm(Richness ~ I(Year-2007),  # year transformation
                      data = toolik_richness, family = poisson,
                      chains = 4, cores = 4)

# Assessing model convergence
# examining trace plots
plot(stan_glm1, plotfun = "trace") # looks fine

# check summary
summary(stan_glm1)
# n_eff - effective sample size looks good >1000 
# Rhat - potential scale reduction factor (indicating convergence if = 1), greater
# than one could indicate an issue
