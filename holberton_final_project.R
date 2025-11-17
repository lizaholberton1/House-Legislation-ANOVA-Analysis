## Project:  STA 215, Fall 2024, Final Project
# Located:   sta 215
# File Name: holberton-final-project.R
# Date:      2024_11_24
# Who:       Liza Holberton

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

#load data
setwd("/Users/lizaholberton/Desktop/sta215")
raw_data_2 <- read_csv("raw_data.csv")
raw_data_2$voting_record <- as.numeric(raw_data_2$voting_record)
raw_data <- na.omit(raw_data_2)

##################################################################################
############### Table 1: descriptive statistics    ###############################   
##################################################################################

#Overall summary of the data set
summary(raw_data)

#Frequency tables for categorical variables
table(raw_data$race)
table(raw_data$gender)
table(raw_data$voting_record)

#Descriptive statistics for numerical variables
table(raw_data$bills)
mean(raw_data$bills)
sd(raw_data$bills)

table(raw_data$voting_record)
mean(raw_data$voting_record)
sd(raw_data$voting_record)

#Frequency table for region 
table(raw_data$region)

##################################################################################
####################  Table 2: Contingency Table     #############################   
##################################################################################
table(raw_data$race, raw_data$gender)
chisq.test(table(raw_data$race, raw_data$gender))

##################################################################################
####################   Figure 1: Box Plot    #######################################
##################################################################################

boxplot(bills ~ race, data = raw_data)
# One-way ANOVA 
anova <- aov(race ~ bills, data = raw_data)
summary(anova)

##################################################################################
####################   Figure 2: Scatter Plot    ################################ 
##################################################################################
hist(raw_data$bills)
raw_data$lnbills <- log(raw_data$bills + 1)
hist(raw_data$lnbills)

#Create histogram using log command
hist(raw_data$voting_record)
raw_data$voting_record <- log(raw_data$voting_record)
hist(raw_data$voting_record)

#Linear regression analysis 
plot(raw_data$lnbills, raw_data$voting_record)
meany <- mean(raw_data$voting_record, na.rm = TRUE)
meanx <- mean(raw_data$lnbills, na.rm = TRUE)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")

#t-test for coefficients of the linear regression model 
linear_relationship <- lm(voting_record ~ lnbills, data = raw_data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: Residual Plot     ###############################
##################################################################################
# Plot the residuals
plot(raw_data$lnbills, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
