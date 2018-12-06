# Maximilian Kahn - Take-home Exam

#----Problem 2----

#Plotting the BMI data in a bar plot

bmi.values <- c(53,54,55,56,58,60,63,64,62,62,62) # Vector of BMI percentage values

years <- 1995:2005 # Generate "years" vector
  
# Plotting the percentage of adults with a BMI above 25 from 1995 to 2005.
BMI.plot <- barplot(bmi.values, names.arg=years, xlab="year", 
             ylab="Percentage above 25 BMI", main="BMI Percentage vs. Year",las = 1) 

#----Problem 3----

# a. Calculating mean for males and females
male.freq <- c(38,17,7,6,4,10,2,0,0) # male LRS frequencies
female.freq <- c(30,25,3,6,8,4,0,4,1) # female LRS frequencies
LRS <- c(0:8) # level of LRS

(male.mean <- sum((male.freq * LRS) / sum(male.freq))) # LRS mean for males
(female.mean <- sum((female.freq * LRS) / sum(female.freq))) # LRS mean for females

# The mean for males is approximately 1.5 while for females it is 1.7, so it is higher for females.

# c. Calculating the variance for males and females

# First we need create a function that returns a vector where the values are the LRS levels
# and the number of times they appear in the vector is their frequency.

# This function takes as input a vector of the frequencies and a vector of the magnitude
# of the frequencies. E.g. given the frequencies c(38,5,1) and the mag. of frequencies
# as c(0, 1, 2), the function returns the vector c(0, 5, 2). I used this function for 
# problem 2 on problem set 2. 
format.frequency <- function(frequencies, levels) {
  total.freq <- c() # initialize a vector, which we will store our values in.
  counter <- 0 # A counter to index the vector "levels"
  for (freq.num in frequencies) { # loop through every value in the vector "frequencies".
    counter <- counter + 1
    freq <- rep(levels[counter],freq.num) # create a vector that repeates the levels value, "freq.num" times.
    total.freq <- c(total.freq, freq) # append the freq vector to the vector we are going to eventually return.
  }
  return(total.freq) # return our vector with all values appended
}

# The sample variance is calculated as follows: take the difference between the data point and the mean, square the value. 
# Then sum the values and divide the sum by the size of the data set minus 1 since we are working with a sample.
# The function format.frequency prints out a bunch of values, ignore the values and look at the last one that is
# printed for the code below, the last number printed is the variance.
sum(male.var <- sum(((format.frequency(male.freq, LRS)) - male.mean)^2 / (sum(male.freq) - 1))) # The variance for male LRS

sum(((format.frequency(female.freq, LRS)) - male.mean)^2 / (sum(female.freq) - 1)) # The variance for female LRS

# d. Calculating the median for males and females

# Creating a function that returns the median

getmedian <- function(data) {
  data.length <- length(data) # length of data
  # Test to see if data length is even or odd
  if (data.length %% 2 == 0) { # if the length of the data is even then take the mean of the middle data
    middle1 <- data.length / 2
    middle2 <- middle1 + 1
    median <- (data[middle1] + data[middle2]) / 2
  } else { # otherwise the median is the middle value in the vector
    middle1 <- ceiling(data.length / 2) # ceiling round half the length to get the middle position in the vector
    median <- data[middle1]
  }
 return(median)
}

male.freq <- format.frequency(male.freq, LRS) # prepare a vector of the male frequencies
female.freq <- format.frequency(female.freq, LRS) # prepare a vector of the female frequencies

getmedian(male.freq) # get median of male frequencies
getmedian(female.freq) # get median of female frequencies

#----Problem 7---- 
# 
# b. Calculating the expected frequencies from the termite data

blue.workers <- c(3,37) # blue worker data
white.workers <- c(31,9) # white worker data
termites <- rbind(blue.workers, white.workers) # putting worker data into a matrix
colnames(termites) <- c("Unharmed","Immobilized")
(termites) # visualize the termite matrix

(tot.workers <- sum(termites)) # sum of all categories of termites

(worker.prob <- rowSums(termites) / tot.workers) # proportions for workers (blue or white liquid)
(outcome.prob <- colSums(termites) / tot.workers) # proportions for outcome (unharmed or immobilized)

# Finding the expected frequencies
expected <- c() # reset expected vector

for (i in worker.prob) { # returns birth probabilities
  t <- c()
  for (j in outcome.prob) { # returns mating number probabilities
    t <- c(t, i*j)
  }
  expected <- c(expected, t) # store expected value probabilties in this vector
}

(expected.values <- expected * tot.workers) # calculate expected value

(expected.termites <- matrix(expected.values, nrow = 2, ncol = 2, byrow = TRUE)) # turn vector into matrix
colnames(expected.termites) <- c("Unharmed","Immobilized") # renaming columns
rownames(expected.termites) <- c("blue.workers","white.workers") # renaming rows

(expected.termites) # visualize expected values

# d. Testing to see if we can reject null hypothesis

# Calculating the chi-square value:
  
(t <- (termites - expected.termites)^2) # temporary value - (observed - expected)^2

# diving each value by the corresponding expected value
t[1,1] <- t[1,1] / expected.termites[1,1]
t[1,2] <- t[1,2] / expected.termites[1,2]
t[2,1] <- t[2,1] / expected.termites[2,1]
t[2,2] <- t[2,2] / expected.termites[2,2]

(chi.square.value <- sum(t)) # The chi-square value!

# The chi-square value is 29.25, the df = (2-1)(2-1) = 1, this chi-square value corresponds with a 
# p-value of less than 0.001, and thus we can confidently reject the null hypothesis.

#----Problem 8----

# b. Calculating the proportion of deaths due to drunk driving at the 25th percentile

# We can find the z-score value that roughly corresponds with the 25th percentile through trial and error.
pnorm(-0.67) # Is approximately equal to 0.25. Thus, Z-score is equal to -0.67.

# Given that Z-score is -0.67. Then the proportion of deaths at the 25th percentile is:
  
(Y <- -0.67 * 0.068 + 0.569) # approximately 0.52.

#----Problem 9----

 # b. Writing our own function that plots a normal distribution with a mean and std given by the user.

# Creating the normal distribution function that returns a set of numbers conforming to the distribution
normal.dist <- function(mean, std) {
  co <- (1 / sqrt(2 * pi * std^2)) # calculate the coefficient
  range <- (-100 + mean):(100 + mean) # calculate the range of x-values to input
  norm.dist <- c() # initialize vector that will hold the distribution
  for (Y in range) {
    exponent <- -(Y - mean)^2 / (2 * std^2) # calculate the exponent for a y-value 
    val <- co * exp(1)^(exponent) # calculate a y-value
    norm.dist <- c(norm.dist, val) # append to norm.dist vector
  }
  return(norm.dist) # return normal distribution set
}

# Plot a normal distribition based on user input mean and std values
normal.plot <- function(mean, std) {
  x <- (-100 + mean):(100 + mean) # initialize x-range of values based on mean
  plot(x, normal.dist(mean, std), type="l", xlab="x value",
       ylab="Density", main="Normal Distribution") # plot the normal distribution y-values against x!
}

mean <- 25
std <- 19
normal.plot(mean, std) # example plot, mean = 25, std = 19.

# c. # Since two standard deviations away in both directions covers 95% percent of the distribution:


normal.plot(mean, std) # example plot, mean = 25, std = 19.
abline(v = (-2 * std) + mean) # plot vertical line two std away from the mean to the left
abline(v = 2 * std + mean) # plot vertical line two std away from the mean to the right
