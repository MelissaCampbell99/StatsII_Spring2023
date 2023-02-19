#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd("C:/Users/le_ba/Documents/GitHub/StatsII_Spring2023/problemSets/PS02/template")

#####################
# Problem 1
#####################

# load data
 load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true")
  
ls()
readRDS(file = "climateSupport.RData")     
summary(climateSupport)      
summary(con)      
head(climateSupport) 
tail(climateSupport)
colnames(climateSupport)

#  Use ifelse() with as.logical()...
as.logical(ifelse(climateSupport$choice == "Supported", 1, 0))
as.numeric(climateSupport$choice)
class(climateSupport$sanctions)
as.numeric(climateSupport$countries)
as.numeric(climateSupport$sanctions)
class(climateSupport$countries)
class(climateSupport$sanctions)
## a) Run the logit regression
mod <- glm(choice ~ ., 
           data = climateSupport, 
           family = "binomial")


summary(mod)
mod$iter

mod_coef <- cbind(coef(summary(mod)),
                       confint(mod))

mod_coef

################
install.packages("tidyverse")
library(tidyverse)
#Plot
plot(climateSupport$countries, xlab="countries", ylab = "choice")
points(climateSupport$countries, fitted(mod), col = "green", pch=16)
points(climateSupport$countries, fitted (ols_base), col = "pink", pch = 2)
curve(predict(ols_base, data.frame(countries = x), type = "response"), col = "yellow", add = T)
curve(predict(logit_base, data.frame(countries = x), type = "response"), col = "red", add = T)



###############################################################
## Likelihood ratio test
#  Create a null model
nullMod <- glm(choice ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = climateSupport, 
               family = "binomial")
summary(nullMod)
#  Run an anova test on the model compared to the null model 
anova(nullMod, mod, test = "Chisq")
anova(nullMod, mod, test = "LRT") # LRT is equivalent

##  Extracting confidence intervals (of the coefficients)

exp(confint(mod)) # transform to odds ratio using exp()

# data.frame of confidence intervals and coefficients
confMod <- data.frame(cbind(lower = exp(confint(mod)[,1]), 
                            coefs = exp(coef(mod)), 
                            upper = exp(confint(mod)[,2])))
summary(confMod)
# Then use this to make a plot
plot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")

# Looking at this plot, which terms are significant at the 0.05 level?

## b) Factor vs numeric
#  The nsibs variable was parsed as an integer. The model.matrix() function is 
#  used under the hood by lm() and glm() to create a design matrix of the model. 
#  See the difference compared to when we input nsibs as an integer and a 
#  factor:
?model.matrix
head(climateSupport)
# Using model.matrix to check for each unique level of sanctions, and each unique level of country participation

model.matrix( ~ unique(sanctions), data = climateSupport) # I see a problem with the data here...
model.matrix( ~ unique(countries), data = climateSupport)

# As a side note, we can use unique() with model.matrix() to create a matrix 
# of different combinations of factor levels to use with predict(). Though it's
# probably not the best approach...
model.matrix( ~ as.factor(unique(sanctions)), data = climateSupport)

# A better function to help with this is expand.grid()
with(climateSupport, expand.grid(choice = unique(choice),
                             countries = unique(countries),
                             sanctions = unique(sanctions)))

# Consider for instance if we had a model just consisting of factors:
mod2 <- glm(choice ~ sanctions + countries, 
            data = climateSupport, 
            family = "binomial")
summary(mod2)
predicted_data <- with(climateSupport, expand.grid(choice = unique(choice),
                                               countries = unique(countries),
                                               sanctions = unique(sanctions)))

predicted_data <- cbind(predicted_data, predict(mod2, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

#Fill out the confidence intervals and predicted probability 
predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })
predicted_data


# As an alternative to coercing an interval variable as a factor, with one 
# level for each unique value, we can "bin" the variable into a smaller number 
# of categories using cut()
?cut
summary(climateSupport)
climateSupport$countries_cut <- cut(climateSupport$countries, 
                            breaks = c(0, 20, 80, Inf), 
                            include.lowest = TRUE,
                            labels = c("20 of 192", "80 of 192", "160 of 192"))

mod3 <- glm(choice ~., 
            data = climateSupport[,!names(climateSupport) %in% c("countries", "countries_f")], 
            family = "binomial")
summary(mod3)
summary(mod)

# Extract confidence intervals around the estimates
confMod3 <- data.frame(cbind(lower = exp(confint(mod3)[,1]), 
                             coefs = exp(coef(mod3)), 
                             upper = exp(confint(mod3)[,2])))
install.packages("rlang")
library(ggplot)
devtools::install_github("tidyverse/tidyverse")
# Plot the estimates and confidence intervals
ggplot(data = confMod3, mapping = aes(x = row.names(confMod3), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,8,1)) +
  labs(x = "Terms", y = "Coefficients")
##########################



mod4 <- glm(choice ~ sanctions + countries, 
            data = climateSupport, 
            family = "binomial")
mod4

###############################################################

£££££££££££££££££££££££££££££££££££
#  Parse column names as a vector to colClasses
climate <- read.table("climateSupport",
                         colClasses = c("choice" = "factor", 
                                        "countries" = "factor",
                                        "sanctions" = "factor",
                                        ))
summary(climate)
      
      
£££££££££££££      
      climate_data <- read.table("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true",
                              stringsAsFactors = TRUE)
summary(climate_data)
summary(data)
attach("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"") # safer and will warn about masked objects w/ same name in .GlobalEnv
ls(pos = 2)
##  also typically need to cleanup the search path:
detach("file:all.rda")

## clean up (the example):
unlink("all.rda")
# }
# NOT RUN {
# }
# NOT RUN {
con <- url(""https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"")
## print the value to see what objects were created.
print(load(con))
close(con) # url() always opens the connection
# }

con <-url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true") # Create connexion
load(con) #Load the data
# source the file which contains
# the function to be called
# assuming in same directory
source('climateSupport.RData')

# Create a vector of pies
x <- c(10,20,30,40)

# pass the x to the function in barpie.R file
bar_and_pie(x)


close(con) #close connexion
print(d) # Print the data
 
