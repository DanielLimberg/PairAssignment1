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
teeth <- ToothGrowth[,c(2,1,3)]

# Mesures of Central Tendency: Mean | Median | Histogram

## Loop for Mean of each Variable (NA for supply(2) which is nominal)
for (i in 1:3) {
  ToothGrowth[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(ToothGrowth)[i], ., "\n") %>%
    cat()
}
 
## Loop for Median of each Variable (supply (2) is nominal)
median(ToothGrowth$len)
median(ToothGrowth$dose)

# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

## Range
range(ToothGrowth$dose)
for (i in 1:2) {
  ToothGrowth[, i] %>%
    range() %>%
    paste(names(ToothGrowth)[i], ., "\n") %>%
    cat()
}

## Quartiles
summary(ToothGrowth$len)
for (i in 1:3) {
  ToothGrowth[, i] %>%
    summary() %>%
    paste(names(ToothGrowth)[i], ., "\n") %>%
    cat()
}

## Interquartile Range
IQR(ToothGrowth$len)
for (i in 1:3) {
  ToothGrowth[, i] %>%
    IQR() %>%
    paste(names(ToothGrowth)[i], ., "\n") %>%
    cat()
}

## Boxplots
boxplot(ToothGrowth$len, main = 'Length of Teeth')
for (i in 1:3) {
  ToothGrowth[, i] %>%
    boxplot() %>%
    paste(names(ToothGrowth)[i], ., "\n") %>%
    cat()
}

## Variance: two ways to calculate
x <- ToothGrowth$len
sum( (x - mean(x) )^2 )
var(x) * (length(x) - 1)
rm(i, x)
