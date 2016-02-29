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

##mean ozone of nasa for 
tapply(nasa$len, nasa$supp, mean)

## Loop for Median of each Variable
median(nasa$len)
median(nasa$dose)

## Loop for Median of each Variable except supply (nominal)
for (i in 5:11) {
  nasa[, i] %>%
    median() %>%
    round(digits = 2) %>%
    paste(names(nasa)[i], ., "\n") %>%
    cat()
}

## Distribution of Variables (Histograms)

### Length of nasa
hist(nasa$len,
     main="Length of nasa", 
     col="blue", 
     breaks = 20,
     xlab = "Length in mm",
     ylab = "Number of Guinea Pigs")

### Dose in mg/days
hist(nasa$dose,
     main="Dose", 
     col="red", 
     breaks = 10,
     xlab = "Dose in mg/days",
     ylab = "Number of Guinea Pigs")

### Frequency of Supplement given to Guinea Pigs
plot(nasa$supp, xlab = "Supplement")

## compare supplements visually
par(mfrow=c(1,2))
hist(nasa$len[nasa$supp=="VC"],
        col=(c("firebrick1")),
        xlab="Length of nasa", ylab="Frequency")
hist(nasa$len[nasa$supp=="OJ"],
     col=(c("mediumspringgreen")),
     xlab="Length of nasa", ylab="Frequency")
par(mfrow=c(1,1))

# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

## Loop for Range
for (i in 5:11) {
  nasa[, i] %>%
    range() %>%
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

## Loop for Quartiles
summary(nasa$len)
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
