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
rm(ToothGrowth)

# Mesures of Central Tendency: Mean | Median | Histogram

<<<<<<< HEAD
## Loop for Mean of each Variable (NA for supply(2) which is nominal)
for (i in 1:3) {
=======
## Loop for Mean of each Variable except supply (nominal)
for (i in 2:3) {
>>>>>>> origin/master
  teeth[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
 
<<<<<<< HEAD
## Loop for Median of each Variable (supply (2) is nominal)
median(teeth$len)
median(teeth$dose)
=======
## Loop for Median of each Variable except supply (nominal)
for (i in 2:3) {
  teeth[, i] %>%
    median() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Distribution of Variables

### Length of Teeth
hist(teeth$len,
     main="Length of Teeth", 
     col="blue", 
     breaks = 20,
     xlab = "Length in mm",
     ylab = "Number of Guinea Pigs")

### Dose in mg/days
hist(teeth$dose,
     main="Dose", 
     col="red", 
     breaks = 10,
     xlab = "Dose in mg/days",
     ylab = "Number of Guinea Pigs")

### Frequency of Supplement
plot(teeth$supp, xlab = "Supplement")


# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

## Range
for (i in 2:3) {
  teeth[, i] %>%
    range() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Quartiles
summary(teeth$len)
for (i in 2:3) {
  teeth[, i] %>%
    summary() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Interquartile Range
for (i in 2:3) {
  teeth[, i] %>%
    IQR() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Boxplots
boxplot(len~supp, data=teeth, varwidth=TRUE, notch=FALSE,
        col=(c("gold","darkgreen")),
        main="Lenght of Teeth", xlab="Dose per Suppliment")

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
