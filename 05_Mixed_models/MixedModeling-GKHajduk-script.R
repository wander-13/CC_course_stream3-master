######################################
#                                    #
#   Mixed effects modeling in R     #
#                                    #
######################################

## authors: Gabriela K Hajduk, based on workshop developed by Liam Bailey
## contact details: gkhajduk.github.io; email: gkhajduk@gmail.com
## date: 2017-03-09
##

###---- Explore the data -----###

## load the data and have a look at it

load("05_Mixed_modelsdragons.RData")

head(dragons)

## Let's say we want to know how the body length affects test scores.

## Have a look at the data distribution:

hist(dragons$testScore)  # seems close to normal distribution - good!

## It is good practice to  standardise your explanatory variables before proceeding - you can use scale() to do that:

dragons$bodyLength2 <- scale(dragons$bodyLength)

## Back to our question: is test score affected by body length?

###---- Fit all data in one analysis -----###

## One way to analyse this data would be to try fitting a linear model to all our data, ignoring the sites and the mountain ranges for now.

library(lme4)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## Let's plot the data with ggplot2

library(ggplot2)

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point()+
  geom_smooth(method = "lm")


### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line

plot(basic.lm, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad


## However, what about observation independence? Are our data independent?
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here

## We could also plot it colouring points by mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
    theme(legend.position = "none")

## From the above plots it looks like our mountain ranges vary both in the dragon body length and in their test scores. This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

## So what do we do?

###----- Run multiple analyses -----###


## We could run many separate analyses and fit a regression for each of the mountain ranges.

## Lets have a quick look at the data split by mountain range
## We use the facet_wrap to do that

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
    facet_wrap(~ mountainRange) +
    xlab("length") + ylab("test score")



##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different mountain ranges

## let's add mountain range as a fixed effect to our basic.lm

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

## now body length is not significant


###----- Mixed effects models -----###



##----- First mixed model -----##

### model
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

### plots
plot(mixed.lmer)  # looks alright, no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

### summary

### variance accounted for by mountain ranges



##-- implicit vs explicit nesting --##

head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

### create new "sample" variable
dragons <- within(dragons, sample <- factor(mountainRange:site))

##----- Second mixed model -----##
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  
# treats the two random effects as if they are crossed, when they are actually nested
summary(mixed.WRONG)


### model
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)
### summary
# Using the varible "sample" that we created, we are trying to account for the
# mountain range level and site level variance
# We are able to specify nesting in other ways:
# using these terms in the model arguments: 
# (1|mountainRange/site) or (1|mountainRange) + (1|mountainRange:site)
# but you would need to remember to do this for every model call to the data
# by stating the relationship explicitly in the dataframe itself you no longer 
# need to state it explicitly in the model calls as it is now defined in the data

### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), linewidth = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

##----- Model allowing random slopes as well as random intercepts -----##
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 

summary(mixed.ranslope)

### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

##----- Presenting your model... nicely!
# ggplot2 is not designed to handle mixed-effects model objects, so we must use
# ggeffects package
library(ggeffects)  # install the package first if you haven't already, then load it

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = dragons,                      # adding the raw data (scaled values)
               aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
    labs(x = "Body Length (indexed)", y = "Test Score", 
         title = "Body length does not affect intelligence in dragons") + 
    theme_minimal()
)
# to visualize the relationships at different levels of the random effects, specify
# type = "re" (indicating random effects), inside the ggpredict function, also 
# add the random effect name to the terms argument

# a quicker way to plot this would be:
# Library
library(dplyr)

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re") %>% 
  plot() +
  labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") +
  theme_classic()

# visualize mixed models, showing variation among levels of your random effects:
##-- If you have a random slope model!
# plot the departure from the overall model estimate for intercepts and slopes

library(sjPlot)

# Visualise random effects 
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))
# shows the difference between the general intercept or slope value found in the model summary
# and the estimate for this specific level of random effect

# show summary
summary(mixed.ranslope)

###----- Tables
library(stargazer)

stargazer(mixed.lmer2, type = "text", # allows you to see ouput in console
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

##----- Model selection for the keen -----##

### full model
full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE) # REML = Restricted/residual maximum likelihood

### reduced model
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)

### comparison
anova(reduced.lmer, full.lmer)  # the two models are not significantly different

