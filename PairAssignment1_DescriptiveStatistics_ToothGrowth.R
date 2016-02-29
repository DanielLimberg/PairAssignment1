##########################
# CSSD Pair Assignment 1: File structure, version control, R data, descriptive statistics
# Deadline: 03/04/2016
# Starting Date: 02/20/2016 | Last edited: 02/24/2016
# Authors: Daniel Limberg & Lukas Müller
# Descriptive Statistics: Measures of Central Tendency | Measures of Dispersion
# ToothGrowth Dataset
##########################

# Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment1"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment1"),silent=TRUE)
getwd()

# Dynamical Link to first R script file
source("Y2_SS_Collaborative_PairAssignment1.R")

# Summary Statistics
summary(ToothGrowth)
## Summarizes variable: all contineous, except 'supp': character variable.
teeth <- ToothGrowth[,c(2,1,3)]
rm(ToothGrowth)
## Set new order to simplify loops in the follwing

# Mesures of Central Tendency: Mean | Median | Histogram

# Loop for Mean of each Variable except supply (nominal)
for (i in 2:3) {
  teeth[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

# Mean length of teeth for 'OJ' and 'VC' seperately
tapply(teeth$len, teeth$supp, mean)
## The mean tooth length for guinea pigs treated with orange juice is 20.66 mm. 
## For the ones given VJ supplements the mean length is 16.96 mm.

# Loop for Median of each Variable except supply (nominal)
for (i in 2:3) {
  teeth[, i] %>%
    median() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
## The mean is a measure of central tendency sensitive to outliers. The median is less so.
## Median length of teeth is 19.25 mm, the median dose is 1 mg.

# Distribution of Variables (Histograms)

# Length of Teeth
hist(teeth$len,
     main="Length of Teeth", 
     col="blue", 
     breaks = 20,
     xlab = "Length in mm",
     ylab = "Number of Guinea Pigs")
## Shows the distribution of tooth length in mm for the number of guinea pigs.
## The data for tooth length is not normally distributed.

# Dose in mg/days
hist(teeth$dose,
     main="Dose", 
     col="red", 
     breaks = 10,
     xlab = "Dose in mg/day",
     ylab = "Number of Guinea Pigs")
## Each dose has been fed to the pigs 20 times.

# Frequency of Supplement given to Guinea Pigs
plot(teeth$supp, xlab = "Supplement")
## Each treatment group consisted of 30 guinea pigs

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

# Loop for Range
for (i in 2:3) {
  teeth[, i] %>%
    range() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
## Lenght ranges from 4.2 mm to 33.9 mm, and the dose from 0.5 mg to 2 mg per day.

# Loop for Standard Deviation
for (i in 2:3) {
  teeth[, i] %>%
    sd() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
## The standard deviation for length is 7.65, and for theh dose this value is 0.63.

# Loop for Quartiles
summary(teeth$len)
for (i in 2:3) {
  teeth[, i] %>%
    summary() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
## The values for the quartiles are: 13.08, 19,25, 18,81, and 25.28.
## The first quartile for tooth length reaches from 4.2 to 13.08.

# Loop for Interquartile Range IQR
for (i in 2:3) {
  teeth[, i] %>%
    IQR() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
## The innerquartile range for the length is 12.2 mm, and for the dose 1.5 mg.

# Loop for Standard Deviation
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

# Step by step calculation of the Variance
x <- teeth$len
sum(x - mean(x) )
## Sum of Differences: substracting the means from the values. The sum is 3.73.

a <- sum((x - mean(x))^2)
table(a)
## Sum of Squares: summing up the suares of the differences. The sum is 3452.21.

b <- (length(x) - 1)
table(b)
## Degrees of Freedom: number of observations minus 1. 60-1=59.

c <- (a/b)
table(c)
## Variance: s^2. Dividing the Sum of Squares by Degrees of Freedom. The result is 58.51.

var(teeth$len)
## var = 58.51.

rm(a, b, c, i, x)
## Remove all the objects from the environment.
