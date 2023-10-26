# Model Design
# By: Elin Swank
# Date: 10/26/2023
# Coding Club tutorial
################################################################################

# Load libraries ----
library(tidyverse)  # for data manipulation (tidyr, dplyr), visualization, (ggplot2), ...
library(lme4)  # for hierarchical models
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(MCMCglmm)  # for Bayesian models
library(MCMCvis)  # to visualise Bayesian model outputs
library(stargazer)  # for tables of model outputs
library(glmmTMB)

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

length(unique(toolik_plants$Species))

# Calculate species richness
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species))) %>%
  ungroup()

(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
    geom_histogram() +
    theme_classic())
# histogram shows relative cover is left skewed

# Research Question: How has species richness changed over time at Toolik Lake?
# Richness is a function of time, richness ~ time
# Richness = Explained Variable
# Time = Explanatory Variable

# model with no random effects
plant_m <- lm(Richness ~ I(Year-2007),     # makes year 2008 = year 1, and so on...
              data = toolik_plants)
summary(plant_m)
# try without changing year
# plant_m <- lm(Richness ~ Year, data = toolik_plants)
# summary(plant_m)

plot(plant_m)

# Hierarchical models using lme4
# account for site effects
plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)

# accounting for site and block effects
plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants)
summary(plant_m_plot2)

# account for site, block and plot effects
plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)
summary(plant_m_plot3)

plot(plant_m_plot3)  # Checking residuals

# visualize plots
# Set a clean theme for the graphs
set_theme(base = theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Visualises random effects
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))

save_plot(filename = "model_re.png",
          height = 8, width = 15)  # Save the graph if you wish


# To see the estimate for our fixed effect (default): Year
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))

save_plot(filename = "model_fe.png",
          height = 8, width = 15)  # Save the graph if you wish

# check the effect of mean.temp on richness, with year as a random effect and 
# the same hierarchichal structure for site/block/plot as random effects
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)

# Visualise the fixed effect
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))
save_plot(filename = "model_temp_fe.png",
          height = 8, width = 15)

# Visualise the random effect terms
(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))
save_plot(filename = "model_temp_re.png",
          height = 8, width = 15)

# allow each plot to have it's own relationship with temperature

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year),
                   data = toolik_plants)    # model does not merge
summary(plant_m_rs)

# simplify and continue (ignores hierarchical grouping of blocks and plots, violates regression assumptions)
plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs)

(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))
save_plot(filename = "model_plant_fe.png",
          height = 8, width = 15)

(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))
save_plot(filename = "model_plant_re.png",
          height = 8, width = 15)

ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% plot()
save_plot(filename = "model_temp_richness.png",
          height = 12, width = 14)

ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re") %>% plot() +
  theme(legend.position = "bottom")
save_plot(filename = "model_temp_richness_rs_ri.png",
          height = 12, width = 14)

# Overall predictions - note that we have specified just mean temperature as a term
predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp"))

(pred_plot1 <- ggplot(predictions, aes(x, predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    scale_y_continuous(limits = c(0, 35)) +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot1, filename = "overall_predictions.png",
       height = 5, width = 5)

# Predictions for each grouping level (here plot which is a random effect)
# re stands for random effect
predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re")

(pred_plot2 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_y_continuous(limits = c(0, 35)) +
    theme(legend.position = "bottom") +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot2, filename = "ri_rs_predictions.png",
       height = 5, width = 5)

(pred_plot3 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    theme(legend.position = "bottom") +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot3, filename = "ri_rs_predictions_zoom.png",
       height = 5, width = 5)
