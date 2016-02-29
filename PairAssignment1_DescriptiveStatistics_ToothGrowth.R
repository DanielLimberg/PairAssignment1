##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Descriptive Statistics: Measures of Central Tendency | Measures of Dispersion
# ToothGrowth Dataset
##########################

# working directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment1"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment1"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("Y2_SS_Collaborative_PairAssignment1.R")

# Summary Statistics
summary(ToothGrowth)
teeth <- ToothGrowth[,c(2,1,3)]
rm(ToothGrowth)
# Set new order to simplify loops in the follwing

# Mesures of Central Tendency: Mean | Median | Histogram

## Loop for Mean of each Variable except supply (nominal)
for (i in 2:3) {
  teeth[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Mean length of teeth for 'OJ' and 'VC' seperately
tapply(teeth$len, teeth$supp, mean)

## Loop for Median of each Variable
median(teeth$len)
median(teeth$dose)

## Loop for Median of each Variable except supply (nominal)
for (i in 2:3) {
  teeth[, i] %>%
    median() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

# Distribution of Variables (Histograms)

# Length of Teeth
hist(teeth$len,
     main="Length of Teeth", 
     col="blue", 
     breaks = 20,
     xlab = "Length in mm",
     ylab = "Number of Guinea Pigs")
## Shows the distribution of tooth length in mm for the number of guinea pigs. 

# Dose in mg/days
hist(teeth$dose,
     main="Dose", 
     col="red", 
     breaks = 10,
     xlab = "Dose in mg/day",
     ylab = "Number of Guinea Pigs")
## Shows how many guinea pigs received which of the three doses in mg/day

# Frequency of Supplement given to Guinea Pigs
plot(teeth$supp, xlab = "Supplement")

# Visual Comparison of Supplements
par(mfrow=c(1,2))
hist(teeth$len[teeth$supp=="VC"],
     main="Supplement VC",
     col=(c("firebrick1")),
     xlab="Length of Teeth in mm", ylab="Frequency")
hist(teeth$len[teeth$supp=="OJ"],
     main="Supplement OJ",
     col=(c("mediumspringgreen")),
     xlab="Length of Teeth in mm", ylab="Frequency")
par(mfrow=c(1,1))
## The histograms reveal, that the supplement OJ is more efficient (i.e. more longer teeth)

# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

## Loop for Range
for (i in 2:3) {
  teeth[, i] %>%
    range() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Loop for Standard Deviation
for (i in 2:3) {
  teeth[, i] %>%
    sd() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Loop for Quartiles
summary(teeth$len)
for (i in 2:3) {
  teeth[, i] %>%
    summary() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Loop for Interquartile Range IQR
for (i in 2:3) {
  teeth[, i] %>%
    IQR() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Loop for Standard Deviation
for (i in 2:3) {
  teeth[, i] %>%
    sd() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
## length: 7.64931517188761 
## dose: 0.628872185733079 

# Boxplots 

boxplot(len~supp, data=teeth, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of Teeth", xlab="Supplement")
## Combining measures of central tendency (mean, range and IQR), the boxplots also show that
## supplement OJ is more efficient

boxplot(len~dose, data=teeth, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of Teeth", xlab="Dose in mg/days")
## Regardless of the supplement: The higher the dose, the longer the teeth on average

boxplot(len~supp*dose, data=teeth, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of Teeth", xlab="Supplement")
## Combining dose and supplement visually, again we find that the supplement OJ is more efficent

## Variance: Sum of Differences
x <- teeth$len
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
var(teeth$len)

rm(a, b, c, i, x)
