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
qplot(supp, len, data=teeth)

# Test of statistical significance
# Difference in means test
teeth$supp <- as.factor(teeth$supp)
temp <- teeth$supp == "OJ"
OJ <- teeth[temp,]$len #supplement 'OJ'
VC <- teeth[!temp,]$len #supplement 'VC'

## The 60 pigs become separeted in two groups according to the supplement they were given.
## Both groups are put into an object.

t.test(OJ, VC)

## A difference in means test is conducted to see whether the difference between the two groups is stat. sign.
## The propability that the Null is true is 6.1%. That means that the Null cannot be rejected.

rm(temp, OJ, VC)

# Chi square test
teeth$len2 <- cut(teeth$len, c(0,12,19,25.2,40))
teeth$len2 <- as.factor(teeth$len2)
## Tooth lenght is divided into four groups.

tbl <- table(teeth$dose, teeth$len2)
chisq.test(tbl)
## Chi squ test to see whether there is a statistical sign. relationship between the dose and the length.
## The result p<0.05 indicates that there is a sign. relationship between the dose and the group of length.

rm(tbl)
