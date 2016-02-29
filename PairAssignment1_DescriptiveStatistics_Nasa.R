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

# Mesures of Central Tendency: Mean | Median | Histogram

## mean ozone and temperature for northern and southern hemisphere
nasa$hem <- nasa$lat
nasa$hem[nasa$lat>0] <- 1 #northern hemisphere
nasa$hem[nasa$lat<0] <- 0 #southern hemisphere

tapply(nasa$ozone, nasa$hem, mean)
tapply(nasa$temperature, nasa$hem, mean)

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
tapply(south$temperature, south$QU, mean)
rm(north, south)

## Loop for Mean of each Variable
for (i in 5:11) {
  nasa[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Loop for Median of each Variable
for (i in 5:11) {
  nasa[, i] %>%
    median() %>%
    round(digits = 2) %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Distribution of Variables (Histograms)

### nasa$ozone
hist(nasa$ozone,
     main = "Ozone in the Atmosphere", 
     col = "deepskyblue2", 
     breaks = 20,
     xlab = "Ozone",
     ylab = "Frequency")

### nasa$temp
hist(nasa$temperature,
     main = "Global Temperature", 
     col = "firebrick1", 
     breaks = 10,
     xlab = "Temp.",
     ylab = "Frequency")

## compare hemispheres visually
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


# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

## Loop for Range
for (i in 5:11) {
  nasa[, i] %>%
    range() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Loop for Quartiles
for (i in 5:11) {
  nasa[, i] %>%
    summary() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Loop for Interquartile Range IQR, except variables w/ NA's
for (i in 6:11) {
  nasa[, i] %>%
    IQR() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Loop for Standard Deviation
for (i in 5:11) {
  nasa[, i] %>%
    sd() %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Boxplots 

par(mfcol = c(1, 2))
boxplot(nasa$ozone,
        main="Ozone", ylab="DU of Ozone")
boxplot(nasa$temperature,
        main="Temperature", ylab="Temparature in Kelvin")



## Variance: Sum of Differences
x <- nasa$temperature
sum(x - mean(x) )

## Variance: Sum of Squares
a <- sum((x - mean(x))^2)
table(a)

## Variance: Degrees of Freedom
b <- (length(x) - 1)
table(b)

## Variance: s^2
c <- (a/b)
table(c)
var(nasa$temperature)

rm(a, b, c, i, x)
