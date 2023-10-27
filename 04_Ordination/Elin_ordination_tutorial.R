# Ordination
# By: Elin Swank
# Date: 10/27/2023
# Coding Club Tutorial
################################################################################

# Set the working directory (if you didn`t do this already)
# setwd("your_filepath")

# Install and load the following packages
install.packages("vegan")
install.packages("ape")
install.packages("dplyr")

library(vegan)
library(ape)
library(plyr);library(dplyr)

# Load the community dataset which we`ll use in the examples today
data(varespec)

# Open the dataset and look if you can find any patterns
View(varespec)
# It is probably very difficult to see any patterns by just looking at the data frame!

# With this command, you`ll perform a NMDS and plot the results
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

PCA <- rda(varespec, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:3]) # 79%, this is ok.


# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")
plot(PCA, type = "points")

# You can extract the species and site scores on the new PC for further analyses:
sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points")) # biplot of axis 1 vs 3

# Check out the help file how to pimp your biplot further:
?biplot.rda

# You can even go beyond that, and use the ggbiplot package.
# You can install this package by running:
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
