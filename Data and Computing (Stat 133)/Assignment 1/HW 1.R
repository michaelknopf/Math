# HW 1
# Do Not remove any of the comments. These are marked by #

# Name: Michael Knopf

# Load the data for this assignment into your R session 
# with the following command:

load(url("http://www.stat.berkeley.edu/users/nolan/data/stat133/SFTemps.rda"))

# Check to see that the data were loaded by running:
objects()
# This should show five variables: dates, dayOfMonth, month, temp, and year

# Use the length() function to find out how many observations there are.
length(dates)
# There are 5534 observations.

# 1. Find the average daily temperature
mean(temp, na.rm = TRUE)
# The mean is 56.95646.

# 2. Find the 10% trimmed average daily temperature
mean(temp,trim=0.1,na.rm=TRUE)
# The 10% trimmed average daily temperature is 56.88641.

# 3. Find the 50% trimmed average daily temperature
mean(temp,trim=0.5,na.rm=TRUE)
# The 50% trimmed average daily temperature is 57.

# 4. Compute the median daily temperature. How does it compare to 
# the 50% trimmed mean? Explain. Put your explanation in a comment
# that begins with three ###
median(temp,na.rm=TRUE)

### Your explanation goes here
### The median is 57.  Executing mean.default will print the source code
### for this function.  The lines

### "if (trim >= 0.5) 
### return(stats::median(x, na.rm = FALSE))"

### show that the 50% trimmed mean is simply defined to be the median.


# 5. We would like to convert the temperature from Farenheit to Celsius. 
# Below are several attempts to do so that each fail.  
# Try running each expression in R. 
# Record the error message in a comment
# Explain what it means. 
# Be sure to directly relate the wording of the error message with the problem you find in the expression.

(temp – 32)
### Error message here: Error: unexpected input in "(temp �"
### Explanation here: the token used for the minus sign in this expression is not an actual minus sign

(temp - 32)5/9
### Error message here: Error: unexpected numeric constant in "(temp - 32)5"
### Explanation here: An asterisk * is required between two expressions in order to multiply them.
### i.e. implicit multiplication does not work in R.  R is interpreting this expression as a 5
### simply written next to another expression, which has no meaning in R.

5/9(temp - 32)
### Error message here: Error: attempt to apply non-function
### Explanation here: R is interpretting this as an attempt to apply a function called "9" to the
### argument "temp - 32".  This is impossible because 9 can never be a function.

[temp - 32]5/9
### Error message here: Error: unexpected '[' in "["
### Explanation here: Square brackets are used only to access elements of lists by index.  In this
### example, they are being attempted to be used to enclose an expression, which is not recognized by R.

# 6. Provide a well-formed expression that correctly performs the 
# calculation that we want. Assign the converted values to tempC

tempC = (temp - 32)*(5/9)


# For the following questions, use one of: head(), summary(),
# class(), min(), max(), hist(), quantile() to answer the questions.

# 7. What were the warmest and coldest temperatures recorded in this time period?

max(temp, na.rm=TRUE)
### 79.6 degrees F

min(temp, na.rm=TRUE)
### 38.3 degrees F


# 8. What does the distribution of temperatures look like, i.e. 
# are there roughly as many warm as cold days, are the temps
# clustered around one value or spread evenly across the range
# of observed temperatures, etc.?

hist(temp)
quantile(temp, na.rm=TRUE)

### There are roughly as many warm as cold days.  I used the quantile function to confirm what the 
### histogram revealed: that the temps are clustered around the median, which is about the same as
### the mean.

# 9. Examine the first few values of dates. These are a special
# type of data. Confirm this with class().

head(dates)
class(dates)

### dates contains objects of the Date class.

# 10. Run the following code to make a plot. 
# (don't worry right now about what this code is doing)

plot(temp~dates, col = rainbow(12)[month], type="p", pch=19, cex = 0.3)

# Use the Zoom button in the Plots window to enlarge the plot.
# Resize the plot so that it is long and short.

# Make an observation about temperature in the Bay Area
# based on this plot (something that you couldn't see with
# the calculations so far.)

### Your answer goes here

### The temperatures are seasonal.  They fluctuate periodically around their mean.

# For the remainder of this assignment we will work with 
# one of the random number generators in R.

# 11. Use the following information about you to generate 
# some random values:  
# a.  Use your year of birth for the mean of the normal
# b.	Use the day of the month you were born for the sd of the normal curve.
# c.	Generate either 3, 4, or 5 random values, depending on the hour of the day that your section meets.
# d.	Assign the values to a variable matching your first name.
# e.	Provide the values generated

Michael = rnorm(n = 5, mean = 1990, sd = 29)
Michael

# 12. Generate a vector called "normsamps" containing 
# 1000 random samples from a normal distribution with 
# mean 1 and SD 2.

normsamps = rnorm(1000,1,2)

# 13. Calculate the mean and sd of the 1000 values.

mean(normsamps)
sd(normsamps)


### The return values from your computation go here
### mean: 1.043699,  sd: 1.980182


# 14. Use implicit coercion of logical to numeric to calculate
# the fraction of the values in normsamps that are less than 3.

length(normsamps[normsamps < 3])/length(normsamps)

### 0.838


# 15. Look up the help for rnorm. 
# You will see a few other functions listed.  
# Use one of them to figure out about what answer you 
# should expect for the previous problem.  
# That is, find the area under the normal(1, 2) curve 
# to the left of 3.  This should be the chance of getting 
# a random value less than 3.

pnorm(3,1,2)

### 0.8413447
