##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Joint Distributions
# ToothGrowth Dataset
##########################

# working directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment1"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment1"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("PairAssignment1_DescriptiveStatistics_ToothGrowth.R")

# Correlation plots
qplot(dose, len, data=teeth)

## A higher dose (regardless of the supplement) in general leads to 
## longer teeth in the overall sample 
qplot(supp, len, data=teeth)
## Supplement OJ is more efficient (i.e. less guinea pigs with shorter teeth)

# Test of statistical significance

## Difference in means test
teeth$supp <- as.factor(teeth$supp)
temp <- teeth$supp == "OJ"
OJ <- teeth[temp,]$len
VC <- teeth[!temp,]$len #supplement 'VC'

t.test(OJ, VC)

rm(temp, OJ, VC)

## Chi square test
teeth$len2 <- cut(teeth$len, c(0,12,19,25.2,40))
teeth$len2 <- as.factor(teeth$len2)

tbl <- table(teeth$supp, teeth$len2)
chisq.test(tbl)

tbl <- table(teeth$dose, teeth$len2)
chisq.test(tbl)

