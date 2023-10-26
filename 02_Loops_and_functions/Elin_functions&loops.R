# Functions and Loops
# By: Elin Swank
# 10/26/2023
# CC-5
###########################

library(ggplot2)
library(dplyr)

trees_bicuar <- read.csv("02_Loops_and_functions/trees_bicuar.csv")
trees_mlunguya <- read.csv("02_Loops_and_functions/trees_mlunguya.csv")

head(trees_bicuar)
str(trees_bicuar)


example.fn <- function(x, y){
  # Perform an action using x and y
  x + y
}

example.fn(x = 1, y = 2)


basal.area <- function(x){
  (pi*(x)^2)/40000
}

basal.area <- function(dbh){
  (pi*(dbh)^2)/40000
}

basal.area(dbh = trees_bicuar$diam)

basal.area <- function(...){
  (pi*c(...)^2)/40000
}

basal.area(trees_bicuar$diam, trees_mlunguya$diam)

trees_bicuar$ba <- basal.area(dbh = trees_bicuar$diam)


for(i in list){
  # PERFORM SOME ACTION
}

trees_bicuar$ba <- basal.area(trees_bicuar$diam)
trees_mlunguya$ba <- basal.area(trees_mlunguya$diam)
# above with for loop and list
trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)
for( i in 1:length(trees) ){
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)
}


trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year)
# Create an empty list
mean_ba_list <- list()

for( i in 1:length(trees_mlunguya_list) ){
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year, mean_ba)
  mean_ba_list[[i]] <- dat
}

ba.mean.year <- function(dbh, year){
  data.frame(
    mean_ba = mean(basal.area(dbh)),
    year = mean(year)
  )    
}

ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)

for( i in 1:length(trees_mlunguya_list) ){
  mean_ba_list[[i]] <- ba.mean.year(
    trees_mlunguya_list[[i]]$diam,
    trees_mlunguya_list[[i]]$year)
}
# above using lapply()
lapply(trees_mlunguya_list, function(x){ba.mean.year(dbh = x$diam, year = x$year)})


bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family)

lapply(bicuar_height_list, mean, na.rm = TRUE)

sapply(bicuar_height_list, mean, na.rm = TRUE)

# using if else statement
stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height + 1, round(height, digits = 1))
  
  lorey_height <- sum(height_adj * ba, na.rm = TRUE) / sum(ba, na.rm = TRUE)
  
  return(lorey_height)
}
# test on lapply()
trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)

lapply(trees_bicuar_list, function(x){stick.adj.lorey(height = x$height, method = x$height_method, ba = x$ba)})


diam.summ <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE, 
                     mean(dbh), 
                     NA)
  median_dbh <- ifelse(median == TRUE, 
                       median(dbh), 
                       NA)
  mean_ba <- ifelse(ba == TRUE, 
                    mean(basal.area(dbh)), 
                    NA)
  
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

diam.summ(dbh = trees_bicuar$diam, mean = TRUE, median = T)


LPI <- read.csv("02_Loops_and_functions/LPI_data_loops.csv")

# Scatteplot to examine how Griffon populations have changed between 1970 and 2014 in Croatia and Italy
vulture <- filter(LPI, Common.Name == "Griffon vulture / Eurasian griffon")
vultureITCR <- filter(vulture, Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              # Changing point size
    geom_smooth(method = lm, aes(fill = Country.list)) +                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position = c(0.9, 0.9)))               # Setting the position for the legend - 0 is left/bottom, 1 is top/right
