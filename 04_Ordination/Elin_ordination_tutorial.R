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
# You can even go beyond that, and use the ggbiplot package.
# You can install this package by running:
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
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
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:3]) 


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

# First step is to calculate a distance matrix. 
# Here we use Bray-Curtis distance metric
dist <- vegdist(varespec,  method = "bray")

# PCoA is not included in vegan. 
# We will use the ape package instead
library(ape)
PCOA <- pcoa(dist)

# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?
sum(as.vector(PCOA$values$Cum_corr_eig[3])) # 57% cumulative explained variance

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, varespec)

# Extract the plot scores from first two PCoA axes (if you need them):
PCOAaxes <- PCOA$vectors[,c(1,2)]

# Compare this result with the PCA plot
par(mfrow = c(1, 2)) 
biplot.pcoa(PCOA)
plot(PCA)

# reset plot window
par(mfrow = c(1, 1)) 

# First step is to calculate a distance matrix. See PCOA for more information about the distance measures
# Here we use bray-curtis distance, which is recommended for abundance data
dist <- vegdist(varespec,  method = "bray")

# In this part, we define a function NMDS.scree() that automatically 
# performs a NMDS for 1-10 dimensions and plots the nr of dimensions vs the stress
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

# Use the function that we just defined to choose the optimal nr of dimensions
NMDS.scree(dist)

# Because the final result depends on the initial 
# random placement of the points 
# we`ll set a seed to make the results reproducible
set.seed(2)

# Here, we perform the final analysis and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# Do you know what the trymax = 100 and trace = F means?
# Let's check the results
NMDS1

# If you don`t provide a dissimilarity matrix, metaMDS automatically applies Bray-Curtis. So in our case, the results would have to be the same
# NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F)
# NMDS2
# turn off auto transform
NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = F)
NMDS2

stressplot(NMDS1)
plot(NMDS1, type = "t") # no species scores
# give metaMDS() orginal community matrix as input and specify distance matrix
NMDS3 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

# Alternatively, you can use the functions ordiplot and orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

# correlating environmental variables with our ordination axes
# Load the second dataset
data(varechem)

# The function envfit will add the environmental variables as vectors to the ordination plot
ef <- envfit(NMDS3, varechem, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value
# Plot the vectors of the significant correlations and interpret the plot
plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)

# displaying groups on ordination plots
# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Group1", 12), rep("Group2", 12))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("red", 12), rep("blue", 12))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)

# Challenge 1
# ordinaition on Dune data set
data(dune)

dunedist <- vegdist(dune,  method = "bray")

NMDS.scree(dunedist)

duneNMDS1 <- metaMDS(dunedist, k = 2, trymax = 100, trace = F)
duneNMDS1

duneNMDS2 <- metaMDS(dune, k = 2, trymax = 100, trace = F, autotransform = F)
duneNMDS2

data("dune.env")

duneNMDS3 <- metaMDS(dune, k = 2, trymax = 100, trace = F, autotransform = F, distance = "bray")

plot(duneNMDS3)

plot(duneNMDS3, display = "sites", type = "n")

points(duneNMDS3, display = "sites", col = "red", cex = 1.25)
text(duneNMDS3, display ="species")

# The function envfit will add the environmental variables as vectors to the ordination plot
efdune <- envfit(duneNMDS3, dune.env, permu = 999)
efdune

plot(duneNMDS3, type = "t", display = "sites")
plot(efdune, p.max = 0.05)

# Challenge 2
# perform classification and overlay on ordination
group = c(rep("Group1", 12), rep("Group2", 8))
colors = c(rep("red", 12), rep("blue", 8))

ordiplot(duneNMDS3, type = "n")
for(i in unique(group)) {
  ordihull(duneNMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(duneNMDS3, display = "species", col = "red", air = 0.01)
orditorp(duneNMDS3, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)
