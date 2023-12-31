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

# posterior Predictive checks
# y_rep variable = model predictions
# generated quantities block = your posterior distributions
# compareing model predictions to raw data
pp_check(stan_glm1, plotfun = "stat", stat = "mean")
pp_check(stan_glm1, plotfun = "dens_overlay")
# predictions follow raw data overall, acceptable

# diagnostics with shinystan - works with rstan, rstanarm, and brms packages stan models
launch_shinystan(stan_glm1)

# Research question
# Has species richness changed?
(model_fit <- toolik_richness %>%
    data_grid(Year = seq_range(Year, n = 101)) %>%
    add_predicted_draws(stan_glm1) %>%
    ggplot(aes(x = Year, y = Richness)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 1/2, colour = "black") +
    geom_point(data = toolik_richness, colour = "darkseagreen4", size = 3) +
    scale_fill_brewer(palette = "Greys"))

# Proiors!!
# checking model priors
prior_summary(stan_glm1)
# rstanarm sutomatically centers and scales model variable data
# these are weakly informative priors, suitable for us

# Extract stan code from an rstanarm/brms model 
stancode <- rstan::get_stancode(stan_glm1$stanfit)
cat(stancode)

# update model with a negative binomial distribution
stan_glm2 <- update(stan_glm1, family = neg_binomial_2)

# Check convergence & priors
plot(stan_glm2, plotfun = "trace")
summary(stan_glm2)
prior_summary(stan_glm2)

# Posterior Predictive Checks
pp_check(stan_glm2, plotfun = "stat", stat = "mean")
pp_check(stan_glm2, plotfun = "dens_overlay")



# Run a stan model with the brms package
# fit a brms model
stan_glm_brms <- brm(bf(Richness ~ I(Year-2007),
                        family = brmsfamily('poisson')), data = toolik_richness,
                     iter = 1000,
                     chains = 4, cores = 4)

summary(stan_glm_brms)
plot(stan_glm_brms)

# Extract Stan code
# The code is nicely annotated, so you can read through
stancode(stan_glm_brms)
