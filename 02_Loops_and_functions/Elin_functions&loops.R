# Functions and Loops
# By: Elin Swank
# 10/26/2023
# CC-5
###########################

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
