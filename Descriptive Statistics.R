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

## Loop for Mean of each Variable except supply (nominal)
for (i in 2:3) {
  teeth[, i] %>%
    mean() %>%
    round(digits = 2) %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}
 
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

?ToothGrowth 




# Measures of Dispersion: Range | IQR | Standard Deviation | Boxplots | Variance

## Range
range(teeth$dose)
for (i in 1:2) {
  teeth[, i] %>%
    range() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Quartiles
summary(teeth$len)
for (i in 1:3) {
  teeth[, i] %>%
    summary() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Interquartile Range
IQR(teeth$len)
for (i in 1:3) {
  teeth[, i] %>%
    IQR() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Boxplots
boxplot(teeth$len, main = 'Length of Teeth')
for (i in 1:3) {
  teeth[, i] %>%
    boxplot() %>%
    paste(names(teeth)[i], ., "\n") %>%
    cat()
}

## Variance: two ways to calculate
x <- teeth$len
sum( (x - mean(x) )^2 )
var(x) * (length(x) - 1)
rm(i, x)
