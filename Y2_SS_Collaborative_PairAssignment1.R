##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Authors: Daniel Limber & Lukas Müller
# Packages | Dataset | Working Directory
##########################

# load packages
library(ggplot2)
library(dplyr)
library(magrittr)
library(knitr)

# working directory
try("C:/Users/Dani/Documents/GitHub2/PairAssignment1")
try("/Users/Lukas/Documents/Git/PairAssignment1")
setwd()

# load dataset
data("ToothGrowth")

# information on dataset
?ToothGrowth
