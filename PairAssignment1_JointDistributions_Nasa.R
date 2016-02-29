##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Joint Distributions
# Nasa Dataset
##########################

# working directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment1"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment1"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("PairAssignment1_DescriptiveStatistics_Nasa.R")

# Correlation between temperature and ozon values 
ggplot(nasa, aes(ozone, temperature)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Correlation between Temperature and Ozone Values") +
  xlab("Ozone Values in DU") +
  ylab("Temperature in Kelvin")
## Including all years (1995-2000) shows a somewhat surprising result: a negative relationship
## between ozon values and temperature. The higher the ozon value, the lower the temperature. 

## Are there differences across years? I.e. did the relationship change?
# Subset Data to 1995
subset1995 <- subset(nasa, year < 1996)
ggplot(subset1995, aes(ozone, temperature)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Correlation Temperature and Ozone Values 1995") +
  xlab("Ozone Values in DU") +
  ylab("Temperature in Kelvin")

# Subset Data to 2000
subset2000 <- subset(nasa, year > 1999)
ggplot(subset2000, aes(ozone, temperature)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Correlation Temperature and Ozone Values 2000") +
  xlab("Ozone Values in DU") +
  ylab("Temperature in Kelvin")

## The relationsship remains the same across the years 1995 and 2000. However,
## graphing only single years already indicates that the relationship is not as linear,
## as the graph with all years suggests.

# Difference in Means Test
north <- filter(nasa, !hem==0)
south <- filter(nasa, !hem==1)
N <- north$ozone
S <- south$ozone
t.test(N, S)

## The differences are statistically significant. There are generally lower ozone values in 
## Southern Central America. 
## Northern Central America: 272.0678 Southern Central America: 259.0359 

N <- north$temperature
S <- south$temperature
t.test(N, S)

## Again, the differences are statistically significant. However, the difference is not
## too substantial. 
## Northern Central America: 298.3946  Southern Central America: 297.1320 

rm(N, S)
