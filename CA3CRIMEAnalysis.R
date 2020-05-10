################### Header for the code ######################
# Title: CA 3 Data Analysis (Crime in Ireland)
#  
# Description: 
#
# <This code is for CA 3, Analysis in Ireland for violence. 
# It deals with finding suitable statistical method, testing hypothesis and performing data set Power analysis. 
#It was cleaned, combined and loaded onto RStudio .>
#
# Author: <Roshini Darmireddi>  
# Date: <09/05/2020>

################### HeaderEnds ########################

# -----LOADING DATASET and analyzing-------

# reading readr library to understand the csv file loaded as a dataset
# data.crime is a data frame
library(readr)
data.crime <- read.csv("IRELAND_CRIME_GARDA_DIVISION_wise_2003-2019.csv")

# Here the data is summarized with max, min, mean, median quartiles of coloumns

summary(data.crime)

# Show the total number of rows
sprintf("Number of Rows in Dataframe: %s", 
        format(nrow(data.crime),big.mark = ","))

# Display the structure of the crime data frame
str(data.crime)
# First 10 rows of the crime data frame
head(data.crime, n=10)

# Checking for missing values for the data frame
sprintf("Total number of missing values in Dataframe: %s"
        , format(sum(is.na(data.crime))))

###### Modifying data as apart of analysis #######

View(data.crime)# View the data before it is modified

# First of all we are going to create a copy of the dataset
data_copy <- data.crime
str(data_copy)

#Removing data from 2003 to 2008
data_copy <- data.crime[ , c(-2, -3, -4, -6, -7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-72,-71,-70)]
str(data_copy)

#Suming all years of quarterly data from 2009-2018

data_copy$'2009Total' <-data_copy$X2009Q1 + data_copy$X2009Q2 + data_copy$X2009Q3 + data_copy$X2009Q4
data_copy$'2010Total' <-data_copy$X2010Q1 + data_copy$X2010Q2 + data_copy$X2010Q3 + data_copy$X2010Q4
data_copy$'2011Total' <-data_copy$X2011Q1 + data_copy$X2011Q2 + data_copy$X2011Q3 + data_copy$X2011Q4
data_copy$'2012Total' <-data_copy$X2012Q1 + data_copy$X2012Q2 + data_copy$X2012Q3 + data_copy$X2012Q4
data_copy$'2013Total' <-data_copy$X2013Q1 + data_copy$X2013Q2 + data_copy$X2013Q3 + data_copy$X2013Q4
data_copy$'2014Total' <-data_copy$X2014Q1 + data_copy$X2014Q2 + data_copy$X2014Q3 + data_copy$X2014Q4
data_copy$'2015Total' <-data_copy$X2015Q1 + data_copy$X2015Q2 + data_copy$X2015Q3 + data_copy$X2015Q4
data_copy$'2016Total' <-data_copy$X2016Q1 + data_copy$X2016Q2 + data_copy$X2016Q3 + data_copy$X2016Q4
data_copy$'2017Total' <-data_copy$X2017Q1 + data_copy$X2017Q2 + data_copy$X2017Q3 + data_copy$X2017Q4
data_copy$'2018Total' <-data_copy$X2018Q1 + data_copy$X2018Q2 + data_copy$X2018Q3 + data_copy$X2018Q4

# Removing few more columns for simpifying the analysis part
data_modified <- data_copy[, -c(3:47)]

# We can get region wise data rowing other regions using the below code
#northern_region <- data_modified[-c(15:84),]

#rm(northern_region) #removing the dataframe work

View(data_modified) #to view data modified table

# To check the number of offences in the data modified table
table(data_modified$TYPE.OF.OFFENCE)

###----- Suming all total's of years considering Type of offences-----
library(dplyr)
data_regionwise <- data_modified%>%
  group_by(REGION,TYPE.OF.OFFENCE)%>%
  summarise(Total_2014=sum(`2014Total`),Total_2015=sum(`2015Total`),Total_2016=sum(`2016Total`),Total_2017=sum(`2017Total`),Total_2018=sum(`2018Total`))

data_regionwise <- as.data.frame(data_regionwise)

View(data_regionwise)
str(data_regionwise)


##-------------Finding data year wise --------------------
#Removing data from 2003 to 2008
crime.data_copy <- data.crime[ , -c(3:42)]
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

View(crime.data_copy)
crimerate_yearwise <- crime.data_copy[, -c(1:35)]

str(crimerate_yearwise)

View(crimerate_yearwise)
years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
numberofoffences <- c(285007, 272705, 257470, 244567, 228619, 225775, 225017, 199076, 213742, 215176)

noofoffences_yearwise<- data.frame(years, numberofoffences)

View(noofoffences_yearwise)




#-----------------Data visualization-----------------
# plot a histogram for the data
# To use histogram, use library lattice
library(ggplot2)


describe(data_regionwise)

library(DataExplorer)

plot(data_regionwise)

plot(data_regionwise$REGION)


plot_histogram(data_regionwise)

plot_density(data_regionwise)


hist(data_regionwise)
plot(x=data_regionwise$REGION, y=data_regionwise$Total_2014)
plot(x=data_regionwise$REGION, y=data_regionwise$Total_2015)
plot(x=data_regionwise$REGION, y=data_regionwise$Total_2016)
plot(x=data_regionwise$REGION, y=data_regionwise$Total_2017)
plot(x=data_regionwise$REGION, y=data_regionwise$Total_2018)


# plot a histogram for the data
# To use histogram, use library lattice
library(lattice)
# histogram is to visualize statistical data in different intervals
# Year wise total number of crimes from the data set()
plot(x=years, y=totals, col = 'blue')

#--------------------Normality test-----------------------


# normality test carried out, as it tells the data is normally distributed or not
# normality check for independent variable
normality_test_offences <- shapiro.test(noofoffences_yearwise$years)
normality_test_offences

# normality check for dependent variable
normality_test_crimerate <- shapiro.test(noofoffences_yearwise$numberofoffences)
normality_test_crimerate 

# From these both operations, p value is greater than 0.05. So we can say that 
# both are normally distributed.

# normality check using q-q plot
qqnorm(noofoffences_yearwise$numberofoffences)# Here noof offences are ploted as cirlces 

qqline(noofoffences_yearwise$numberofoffences, col = 'blue') #Q-Q best fit (the year is the fits in normal distrubtion )

# As the required operation is to determine the relation between two entities, both the entities
# are normally distributed and the data is continous data. So we use pearson test.

####-----Pearson test ----------------
test <- cor.test(noofoffences_yearwise$years, noofoffences_yearwise$numberofoffences, 
                 method = 'pearson', exact = FALSE)
test


### Pearson correlation test
 Offence <- cor.test(data_regionwise$Total_2017, data_regionwise$Total_2018, 
                method = "pearson")
 
Offence



#as the data consists of numerical and factoral data, chisquare test is used.
chisq.test(data_regionwise$TYPE.OF.OFFENCE, data_regionwise$Total_2014, correct=FALSE)
chisq.test(data_regionwise$TYPE.OF.OFFENCE, data_regionwise$Total_2015, correct=FALSE)
chisq.test(data_regionwise$TYPE.OF.OFFENCE, data_regionwise$Total_2016, correct=FALSE)
chisq.test(data_regionwise$TYPE.OF.OFFENCE, data_regionwise$Total_2017, correct=FALSE)
chisq.test(data_regionwise$TYPE.OF.OFFENCE, data_regionwise$Total_2018, correct=FALSE)

#based on result, we can clearly see p value is less than 0.05. so H0 is rejected.

####pca analysis for Yearly crime rate.
#my dataframe consists of only price and date, which are in numeric format
pca <- prcomp(noofoffences_yearwise, center = TRUE, scale. = TRUE)
summary(pca)  #pc1 and pc2. values are imporatnt for futher validation
str(pca) #standard deviation, center and scale can be seen by running the structure

#install.packages("factoextra")
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values #we can see variance of 96.38% and 3.619% for dim1 and dim2 and eigen values are displayed for both the dimensions.

library("FactoMineR")
pca2 <- PCA(noofoffences_yearwise, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) #variances are plotted with respect to dimensions.

pca_for_variables <- get_pca_var(pca)
pca_for_variables

#--------------------Power analysis-----------------------

install.packages("pwr")
library(pwr)

#calculating the effective size
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

#effective size and alpha 5% ,Power analysis is calculated. #pwr.t.test for corelation.
power_analysis <-pwr.t.test(d=0.5,n=NULL,sig.level=0.05,  power=0.95, type="one.sample",alternative="two.sided")
power_analysis
#plotting power analysis
plot(power_analysis)

#-------------------correlation---------------
install.packages("corrplot")
library(corrplot)
corrplot(corr=cor(data_copy[,3:50]),tl.col = "Blue",tl.cex = 0.3)

#correlation graph for yearwise data.
o <- cor(noofoffences_yearwise)
corrplot(o, method = "number")
