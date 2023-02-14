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
# loading the required package
library(dgof)
library(tidyverse) 
library(ggpubr)



# set wd for current folder
setwd("C:/Users/le_ba/Documents/GitHub/StatsI_Fall2022/problemSets/PS01")



#####################
# Problem 1
#####################

set.seed(123)
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))


# sample 1
# generating a random sample from a normal distribution
x <- rnorm(1000, mean=0, sd=4)

# Create a histogram to visualise sample 1
hist(x,                                       
     xlim = c(- 10, 10),
     breaks = 100,
     main = "Visualising Sample 1")

# sample 2
# Generate 1000 observations in Cauchy distribution
x2 <- rcauchy(1000, location = -1, scale = 10)

# Create a histogram to visualise sample 2
hist(x2,                                       
     xlim = c(- 200, 200),
     breaks = 10000,
     main = "Visualising Sample 2")

# apply the ecdf function in order to calculate the ECDF values of data
ecdf(x)
ecdf(x2)

# plotting the results
plot(ecdf(x),
     col = "green")
plot(ecdf(x),
     xlim = range(c(x, x2)),
     col = "blue")
plot(ecdf(x2),
     add = TRUE,
     lty = "dashed",
     col = "red")

## Generate a normal distribution

## find the mean and standard deviation of the normal distribution x:

#find mean of sample
mean(data)
#find standard deviation of sample
sd(data)
# calculate standard error
print(sqrt(sum((x - mean(x)) ^ 2/(length(x) - 1)))
      /sqrt(length(x)))
#Perform t-test to find the p-value
t.test(x)
# run Kolmogorov-Smirnov test
ks.test(x, "pnorm")


# A Cauchy distribution has no mean, so  we can't test for a shift in mean,
# but does have a median, corresponding to the
# positions of the parameters. We need to know the CDF of the test statistic.
# With the CDF of this distribution, we can express the probability of the test
# statistics being at least as extreme as its value x for the sample:
# Left-tailed test: p-value = ecdf(x2).

ecdf(x2)

max(x2, na.rm = FALSE)
min(x2, na.rm = FALSE)

# run Kolmogorov-Smirnov test just on x2
ks.test(x2, "pnorm")
# #perform Kolmogorov-Smirnov test on both x and x2
ks.test(x, x2)




#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Preparing the Data and Design Matrix
Y <- matrix(data$y, nrow = nrow(data), ncol = ncol(data))
n <- ncol(data) 

optim(n, 200, gr = NULL, 
      method = c("BFGS"),
      lower = -Inf, upper = Inf,
      control = list(), hessian = FALSE)
optimHess(n, fn, gr = NULL, …, control = list())
## I don't know how to adapt this code

#The lm regression

model <- lm(y ~ x, data=data)
model
