##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# # Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
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
