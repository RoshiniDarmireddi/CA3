################### Header of the code ######################
# Title: CA 3 Data Analysis (Crime in Ireland)
#  
# Description: 
#
# <This code is for CA 3, which is data
#  analysed for Crime in Ireland. It deals with finding the correct
#  statistical method, testing hypothesis and doing
#  Power analysis on dataset. It has been cleaned combined and loaded 
#  onto the RStudio Environment.>
#
# Author: <Roshini Darmireddi>  
# Date: <09/10/2020>

library(readr)
crime.data <- read.csv("/Users/Roshini/Desktop/Data Science/CA3/crime-in-ireland/IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv")

summary(crime.data)

rm(crime.data_copy) ## This is used to remove varibles which are assigned before


# Show the total number of rows
sprintf("Number of Rows in Dataframe: %s", 
        format(nrow(crime.data),big.mark = ","))

# Display the structure of the crime data frame
str(crime.data)
# First 10 rows of the crime data frame
head(crime.data, n=10)

# Checking for missing values for the data frame
sprintf("Total number of missing values in Dataframe: %s"
        , format(sum(is.na(crime.data))))


###### Modifying data as apart of analysis #######

View(crime.data)# View the data before it is modified

# First of all we are going to create a copy of the dataset
crime.data_copy <- crime.data
str(crime.data_copy)

#Removing data from 2003 to 2008
crime.data_copy <- crime.data[ , c(-6, -7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-72,-71,-70)]
str(crime.data_copy)

crime.data_copy$'2009Total' <- sum(crime.data_copy$X2009Q1, crime.data_copy$X, crime.data_copy$X2009Q3, 
                                   crime.data_copy$X2009Q4)

crime.data_copy$'2010Total' <- sum(crime.data_copy$X2010Q1, crime.data_copy$X2010Q2, crime.data_copy$X2010Q3, 
                                   crime.data_copy$X2010Q4)

crime.data_copy$'2011Total' <- sum(crime.data_copy$X2011Q1, crime.data_copy$X2011Q2, crime.data_copy$X2011Q3, 
                                   crime.data_copy$X2011Q4)

crime.data_copy$'2012Total' <- sum(crime.data_copy$X2012Q1, crime.data_copy$X2012Q2, crime.data_copy$X2012Q3, 
                                   crime.data_copy$X2012Q4)

crime.data_copy$'2013Total' <- sum(crime.data_copy$X2013Q1, crime.data_copy$X2013Q2, crime.data_copy$X2013Q3, 
                                   crime.data_copy$X2013Q4)

crime.data_copy$'2014Total' <- sum(crime.data_copy$X2014Q1, crime.data_copy$X2014Q2, crime.data_copy$X2014Q3, 
                                   crime.data_copy$X2014Q4)

crime.data_copy$'2015Total' <- sum(crime.data_copy$X2015Q1, crime.data_copy$X2015Q2, crime.data_copy$X2015Q3, 
                                   crime.data_copy$X2015Q4)

crime.data_copy$'2016Total' <- sum(crime.data_copy$X2016Q1, crime.data_copy$X2016Q2, crime.data_copy$X2016Q3, 
                                   crime.data_copy$X2016Q4)

crime.data_copy$'2017Total' <- sum(crime.data_copy$X2017Q1, crime.data_copy$X2017Q2, crime.data_copy$X2017Q3, 
                                   crime.data_copy$X2017Q4)

crime.data_copy$'2018Total' <- sum(crime.data_copy$X2018Q1, crime.data_copy$X2018Q2, crime.data_copy$X2018Q3, 
                                   crime.data_copy$X2018Q4)

crime.data_modified <- crime.data_copy[, -c(6:45)]

str(crime.data_modified)

crimerate_yearwise <- (crime.data_modified[c(1,3,4,6,7,8,9,10,11,12,13,14,15)])
View(crimerate_yearwise)
years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
totals <- c(285007, 272705, 257470, 244567, 228619, 225775, 225017, 199076, 213742, 215176)

# Year wise total number of crimes from the data set()
plot(x=years, y=totals, col = 'blue')


#Histogram 

library(ggplot2)
#checking the number of houses registered as per year

hist(crimerate_yearwise)
# --------DESCRIPTIVE STATISTICS---------

install.packages("Hmisc") # note it is case sensitive
library(Hmisc)
# describe() function in the Hmisc package returns 
# the number of variables and observations
# the number of missing and unique values
# the mean, quantiles, and the five highest and lowest values.

describe(crime.data_modified)


# ----------VISUAL REPRESENTATION----------
# Exploring the data visually using DataExplorer Library
install.packages("DataExplorer")
library(DataExplorer)
plot_histogram(crime.data_modified)

# plot_density gives a comparision of the continuous variables
# in a density plot
plot_density(crime.data_modified)

#--------------------------------
barplot(table(crime.data_modified$OFFENCE.CODE))
plot(x=crime.data_modified$`2018Total`, y=crime.data_modified$OFFENCE)

