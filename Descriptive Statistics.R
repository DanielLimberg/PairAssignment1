##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limber & Lukas Müller
# Descriptive Statistics: Measures of Central Tendency | Measures of Dispersion
##########################

# Dynamical Link to first R script file
source("Y2_SS_Collaborative_PairAssignment1.R")

# Summary Statistics
summary(ToothGrowth)

# Mesures of Central Tendency: Mean | Median | Histogram

## Loop for Mean of each Column
for (i in 1:length(names(ToothGrowth))) {
  ToothGrowth[, i] %>%
    mean() %>%
    round(digits = 1) %>%
    paste(names(ToothGrowth)[i], ., '\n') %>%
    cat()
}

# Measures of Dispersion: Standard Deviation | Range | IQR | Boxplots
