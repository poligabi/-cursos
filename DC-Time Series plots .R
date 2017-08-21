########################################
# DataCamp - Forecast in R - Section 1 #
########################################

# # Read the data from Excel into R
mydata <- read_excel("exercise1.xlsx")

#A time series (ts object) can be thought of as a vector or matrix of numbers, along with some information about 
#what times those numbers were recorded. This information is stored in a "ts" object in R.

# # Create a ts object called myts
myts <- ts(mydata[, 2:4], start = c(1981, 1), frequency = 4)
#Set the first argument to all the data in mydata except for the date column; This is not needed since the ts object 
#will store time information separately from the data. Then, set the start keyword to the form c(year, period) 
#to indicate the time of the first observation, and frequency to the number of observations in each year in the data.

#You will use the autoplot() function to produce time plots of the data. 
#In each plot, look out for outliers, seasonal patterns, and other interesting features.
#When trying any exercises, remember to load these packages yourself (forecast and ggplot2 packages).
library(forecast)
libray(ggplot2)

# # Plot the data with facetting
autoplot(myts, facets = TRUE)

# # Plot the data without facetting
autoplot(myts, facets = FALSE)

# Plot the four series
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)
autoplot(taylor)

# # Find the outlier in the `gold` series
goldoutlier <- which.max(gold)

# # Create a vector of the seasonal frequencies of gold, woolyrnq, gas, and taylor
freq <- c(1, 4, 12, 336)

#Seasonal plot is similar to a time plot except, the data are plotted against the individual “seasons” in which the data were observed.
#A subseries plot comprises mini time plots for each season. Here, the mean for each season is shown as a blue horizontal line.
#These examples will help you to visualize these plots and understand how they can be useful.
#First, you will use the a10 data from the fpp2 package. This contains monthly sales volumes for anti-diabetic drugs in Australia. 
#In the plots, can you see which month has the highest sales volume each year? What is unusual about the results in March and April 2008?

# # Load the fpp2 package
library(fpp2)

# # Create plots of the a10 data
autoplot(a10)
ggseasonplot(a10)

#An interesting variant of a season plot uses polar coordinates, where the time axis is circular rather than horizontal. 
#Try producing a polar coordinate plot for the a10 data using ggseasonplot(). Simply add a polar keyword and set it to TRUE.

# # Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = TRUE)

#You will also use the ausbeer data from the fpp2 package. This contains quarterly beer production for Australia. 
#What is happening to the beer production in Quarter 4?

# # Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start = 1992)

# # Make plots of the beer data
autoplot(beer)
ggsubseriesplot(beer)

### Trends, seasonality and cyclicity ###

Pattern     -     Description
Trend       =   A pattern exists involving a long-term increase OR decrease in the date
Seasonal    =   A periodic pattern exists due to calendar (ex: the quarter, month, or day of the week)
Cyclic      =   A pattern exists where the data exhibits rises and falls that are not of fixed period (duration usually of at least 2 years)

#seasonal pattern constant lenght vs cycliv pattern variable length
#average lenght of cycle longer than length of seasonal pattern

### Autocorrelation of non-seasonal time series ###

#Another way to look at time series data is to plot each observation against another observation that occurred some time previously. 
#For example, you could plot ytyt against yt−1yt−1. This is called a lag plot because you are plotting the time series against lags of 
#itself. The gglagplot() function produces various types of lag plots.
#The correlations associated with the lag plots form what is called the "autocorrelation function". When these correlations are plotted, 
#we get an ACF plot. The ggAcf() function produces ACF plots.
#You will use the annual oil production in Saudi Arabia from 1965-2013(measured in millions of tonnes). That has been loaded into your workspace.

# # Create an autoplot of the oil data
autoplot(oil)

# # Create a lag plot of the oil data
gglagplot(oil)

# # Create an ACF plot of the oil data
ggAcf(oil)

### Autocorrelation of seasonal and cyclic time series ###

#When data are either seasonal or cyclic, the ACF will peak around the seasonal lags or at the average cycle length.
#You will investigate this phenomenon by plotting the annual sunspot series (which follows the solar cycle of approximately 10-11 years)
#in sunspot.year and the daily traffic to the Hyndsight blog (which follows a 7-day weekly pattern) in hyndsight.

# # Plots of annual sunspot numbers
autoplot(sunspot.year)
ggAcf(sunspot.year)

# # Save the lag corresponding to maximum autocorrelation
maxlag_sunspot <- 1
#It should contain the value for maximum autocorrelation for sunspot.year. Look back at your ACF plot to visually determine 
#the lag (x) that corresponds with the maximum value for ACF (y), in this case is "1".

# # Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)

# # Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7
#in this case is "7".

### White noise ###
#Is a purely random time series, no pattern, hipotese zero
#Ljung-Box test = considers the first h autocorrelation values together.  
#A significant test (small p value) indicates the data are probably not white noise.

### Stock prices and white noise ###
#There is a well-known result in economics called the "Efficient Market Hypothesis" that states that asset prices reflect all available 
#information. A consequence of this is that the daily changes in stock prices should behave like white noise (ignoring dividends, 
#interest rates and transaction costs).  
#The consequence for forecasters is that the best forecast of the future price is the current price.
#We can test this hypothesis by looking at the goog series, which contains the closing stock price for Google over 1000 trading days 
#ending on February 13, 2017. This data has been loaded into your workspace.

# # Plot the original series
autoplot(goog)

#Using the diff() function with autoplot(), plot the daily changes in Google stock prices.
# # Plot the differenced series
autoplot(diff(goog))

#Use the ggAcf() function to check if these daily changes look like white noise.
# # ACF of the differenced series
ggAcf(diff(goog))

#Fill in the pre-written code to do a Ljung-Box test on the daily changes using 10 lags. A p-value greater than 0.05 suggests the 
#daily changes are not significantly different from white noise.
# # Ljung-Box test of the differenced series
Box.test(diff(goog), lag = 10, type = "Ljung")

