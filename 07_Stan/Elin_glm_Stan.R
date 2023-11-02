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
