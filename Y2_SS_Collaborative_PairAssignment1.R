##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# # Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Packages | Dataset | Working Directory
##########################

# Load Packages
library(ggplot2)
library(dplyr)
library(magrittr)
library(knitr)
library(MASS)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment1"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment1"),silent=TRUE)
getwd()

# Load Dataset 1
data("ToothGrowth")
names(ToothGrowth)

# Load Dataset 2
data("nasa")
names(nasa)
nasa <- data.frame(nasa)
