##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Descriptive Statistics: Measures of Central Tendency | Measures of Dispersion
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

# Mesures of Central Tendency: Mean | Median | Histogram

## Loop for Mean of each Variable except supply (nominal)
for (i in 5:11) {
  nasa[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

##mean ozone for northern and southern hemisphere
nasa$hem <- nasa$lat
nasa$hem[nasa$lat>0] <- 1 #northern hemisphere
nasa$hem[nasa$lat<0] <- 0 #southern hemisphere

tapply(nasa$ozone, nasa$hem, mean)

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

rm(i)

## Distribution of Variables (Histograms)

### nasa$ozone
hist(nasa$ozone,
     main="Ozone in the Atmosphere", 
     col="blue", 
     breaks = 20,
     xlab = "Ozone",
     ylab = "Frequency")

### nasa$temp
hist(nasa$temperature,
     main="Global Temperature", 
     col="red", 
     breaks = 10,
     xlab = "Temp.",
     ylab = "Frequency")

## compare supplements visually
par(mfrow=c(1,2))
hist(nasa$ozone[nasa$hem==1],
        col=(c("mediumspringgreen")),
        xlab="Ozone", ylab="Frequency")
hist(nasa$temperature[nasa$hem==0],
     col=(c("firebrick1")),
     xlab="Temp.", ylab="Frequency")
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

## Loop for Interquartile Range IQR
for (i in 5:11) {
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

boxplot(len~supp, data=nasa, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of nasa", xlab="Supplement")

boxplot(len~dose, data=nasa, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of nasa", xlab="Dose in mg/days")

boxplot(len~supp*dose, data=nasa, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of nasa", xlab="Supplement")


## Variance: Sum of Differences
x <- nasa$len
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
var(nasa$len)

rm(a, b, c, i, x)
