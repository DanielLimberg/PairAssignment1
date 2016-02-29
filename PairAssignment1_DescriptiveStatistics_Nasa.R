##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Descriptive Statistics: Measures of Central Tendency | Measures of Dispersion
# NASA Dataset
##########################

# working directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment1"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment1"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("Y2_SS_Collaborative_PairAssignment1.R")
rm(ToothGrowth)


# Summary Statistics
summary(nasa)
nasa <- nasa[,c(1,2,3,4,6,5,7,8,9,10,11)]
## The dataset is reordered. Character variables are put in front. This will make looping easier.

# Mesures of Central Tendency: Mean | Median | Histogram
# Mean ozone and temperature for northern and southern hemisphere
nasa$hem <- nasa$lat
nasa$hem[nasa$lat>0] <- 1 #northern Central America
nasa$hem[nasa$lat<0] <- 0 #southern Central America
## Dividing latitude into "North" and "South" along lat=0.

tapply(nasa$ozone, nasa$hem, mean)
tapply(nasa$temperature, nasa$hem, mean)
## Looking at the means for 'ozone' and 'temperature' for each hemisphere.
## The mean ozone value for the South is 259.0 DU. For the North it is 272.1 DU.
## The mean temperature for the South is 297.1 Kelvin. For the North it is 298.4.


## mean ozone and temperature for quarter year

nasa$QU <- nasa$month
nasa$QU[nasa$month==1] <- "Q1"
nasa$QU[nasa$month==2] <- "Q1"
nasa$QU[nasa$month==3] <- "Q1"
nasa$QU[nasa$month==4] <- "Q2"
nasa$QU[nasa$month==5] <- "Q2"
nasa$QU[nasa$month==6] <- "Q2"
nasa$QU[nasa$month==7] <- "Q3"
nasa$QU[nasa$month==8] <- "Q3"
nasa$QU[nasa$month==9] <- "Q3"
nasa$QU[nasa$month==10] <- "Q4"
nasa$QU[nasa$month==11] <- "Q4"
nasa$QU[nasa$month==12] <- "Q4"

north <- filter(nasa, !hem==0)
tapply(north$ozone, north$QU, mean)
south <- filter(nasa, !hem==1)
tapply(south$ozone, south$QU, mean)
## The mean ozone value is lower in the South for all quarters.

tapply(north$temperature, north$QU, mean)
tapply(south$temperature, south$QU, mean)
## The mean temperature value is higher in the South for Q1 and Q2.
## The North displays a higher mean temperature for Q3 and Q4.

rm(north, south)

# Loop for Mean of each Variable
for (i in 5:11) {
  nasa[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}
## These are the mean values: cloudhigh: 8.5; cloudlow: NA; cloudmid: 14; ozone: 264 DU;
## pressure 1000; surftemp 296.9; temperature 299.2.

# Loop for Median of each Variable
for (i in 5:11) {
  nasa[, i] %>%
    median() %>%
    round(digits = 2) %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}
## These are the mean values: cloudhigh: 8.5; cloudlow: NA; cloudmid: 14; ozone: 264 DU; 
## pressure: 1000; surftemp: 296.9; temperature 299.2.

# Distribution of Variables (Histograms)
# nasa$ozone
hist(nasa$ozone,
     main = "Ozone in the Atmosphere", 
     col = "deepskyblue2", 
     breaks = 20,
     xlab = "Ozone",
     ylab = "Frequency")
## The distribution of the ozone values is right skewed.

# nasa$temp
hist(nasa$temperature,
     main = "Global Temperature", 
     col = "firebrick1", 
     breaks = 10,
     xlab = "Temp.",
     ylab = "Frequency")
## The distribution of the temperature values is left skewed.
## High temperatures are more common in Central America.

# Compare North/South visually
par(mfrow=c(2,2))
hist(nasa$ozone[nasa$hem==1],
     main = "Ozone, N.",
     col = (c("deepskyblue2")),
     xlab = "Ozone", ylab="Frequency")
hist(nasa$ozone[nasa$hem==0],
     main = "Ozone, S.",
     col = (c("deepskyblue2")),
     xlab = "Ozone.", ylab="Frequency")
hist(nasa$temperature[nasa$hem==1],
     main = "Temp., N.",
     col = (c("firebrick1")),
     xlab = "Temp.", ylab="Frequency")
hist(nasa$temperature[nasa$hem==0],
     main = "Temp., S.",
     col = (c("firebrick1")),
     xlab = "Temp.", ylab="Frequency")
par(mfrow=c(1,1))
## For North and South ozone is right skewed.
## For North and South temperature is left skewed.


# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

# Loop for Range
for (i in 5:11) {
  nasa[, i] %>%
    range() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}
## The range (min to max value) for following variables: cloudlow: NA/cloudlow: NA;
## cloudhigh: 0/cloudhigh: 62.5; cloudmid: 0/cloudmid: 83.5; ozone: 232/ozone: 390;
## pressure: 615/pressure: 1000; surftemp: 266/surftemp: 314.9; temperature: 269.1/temperature: 310.

# Loop for Quartiles
for (i in 5:11) {
  nasa[, i] %>%
    summary() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}
## The values show the mean of the quartiles.


# Loop for Interquartile Range IQR, except variables w/ NA's
for (i in 6:11) {
  nasa[, i] %>%
    IQR() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}
## The values show the range of each quartile of each variable.

# Loop for Standard Deviation
for (i in 5:11) {
  nasa[, i] %>%
    sd() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}
## The standard deviations are between 4.7 ('surftemp') and 19 ('ozone').
## No standard deviation for 'cloudlow': NA.

# Boxplots 
par(mfcol = c(1, 2))
boxplot(nasa$ozone,
        main="Ozone",
        col = (c("deepskyblue2")),
        ylab="DU of Ozone")
boxplot(nasa$temperature,
        main="Temperature",
        col = (c("firebrick1")),
        ylab="Temparature in Kelvin")
par(mfcol = c(1, 1))
## Boxplots show three measures of central tendency: mean, range, interquartile range.

# Step by step calculation of the Variance
x <- nasa$temperature
sum(x - mean(x) )
## Sum of Differences: substracting the means from the values. The sum is -2.622187e-10.

a <- sum((x - mean(x))^2)
table(a)
## Sum of Squares: summing up the suares of the differences. The sum is 926855.96.

b <- (length(x) - 1)
table(b)
## Degrees of Freedom: number of observations minus 1. 41472-1=41471.

c <- (a/b)
table(c)
## Variance: s^2. Dividing the Sum of Squares by Degrees of Freedom. The result is 22.35.

var(nasa$temperature)
## var = 22.35.

rm(a, b, c, i, x)
## Remove all the objects from the environment.
