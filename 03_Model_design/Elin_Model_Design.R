# Model Design
# By: Elin Swank
# Date: 10/26/2023
Coding Club tutorial
################################################################################

# Load libraries ----
library(tidyverse)  # for data manipulation (tidyr, dplyr), visualization, (ggplot2), ...
library(lme4)  # for hierarchical models
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(MCMCglmm)  # for Bayesian models
library(MCMCvis)  # to visualise Bayesian model outputs
library(stargazer)  # for tables of model outputs

# Load data ----
# Remember to set your working directory to the folder
# where you saved the workshop files
toolik_plants <- read.csv("03_Model_design/toolik_plants.csv")

# Inspect data
head(toolik_plants)
str(toolik_plants)

# We can use mutate() from dplyr to modify columns
# and combine it with across() from dplyr to apply the same
# function (as.factor()) to the selected columns
toolik_plants <-
  toolik_plants %>%
  mutate(across(c(Site, Block, Plot), as.factor))

str(toolik_plants)

# Get the unique site names
unique(toolik_plants$Site)
length(unique(toolik_plants$Site))

# Group the dataframe by Site to see the number of blocks per site
toolik_plants %>% group_by(Site) %>%
  summarise(block.n = length(unique(Block)))

toolik_plants %>% group_by(Block) %>%
  summarise(plot.n = length(unique(Plot)))

unique(toolik_plants$Year)

length(unique(toolik_plants$Species))
unique(toolik_plants$Species)

# We use ! to say that we want to exclude
# all records that meet the criteria

# We use %in% as a shortcut - we are filtering by many criteria
# but they all refer to the same column: Species
toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))

# A much longer way to achieve the same purpose is:
# toolik_plants <- toolik_plants %>%
#  filter(Species != "Woody cover" &
#	       Species != "Tube" &
#         Species != "Hole"&
#				 Species != "Vole trail"....))
# But you can see how that involves unnecessary repetition.