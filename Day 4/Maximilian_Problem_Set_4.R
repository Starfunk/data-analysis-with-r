# Maximilian Kahn - R Problem Set 4

# Problem 1. 
# 
# a. 

(yes.birth <- c(81,85,61,17,5)) # gave birth vector
(no.birth <- c(6,8,0,0,0)) # did not give birth vector.
(birth.freq <- (rbind(yes.birth,no.birth))) # row bind the vectors into a matrix

(prob.birth <- rowSums(birth.freq[,]) / sum(birth.freq)) # probability of giving birth and not giving birth
(prob.mate <- colSums(birth.freq[,] / sum(birth.freq))) # probability of mating with 1, 2, 3, 4, or 5 times

expected <- c() # initialize the vector "expected", which will hold the expected values

# Calulating the expected values. We use a nested for-loop to calculate the probability of 
# having given birth AND mated once, having given birth AND mated twice, ..., did not give birth AND mated five times.
for (i in prob.birth) { # returns birth probabilities
  t <- c()
  for (j in prob.mate) { # returns mating number probabilities
    t <- c(t, i*j)
  }
  expected <- c(expected, t) # store expected value probabilties in this vector
}

(expected * sum(birth.freq)) # This prints the expected frequencies in a vector

# b. The expected frequencies do not meet the assumptions for the chi-squared contingency test
# because more than 20% of the expected values are below 5 and two are below 1.
# So we will lump the last five expected values into their own category and call it probability that
# female mated between 1 and 5 times AND did not give birth.
# 
# c. An alternative explanation is this: female prarie dogs select mates during the process of mating, if they are unsatisfied, 
# they reject their current partner and find the anothe partner. Prarie dogs that have mated multiple times have been disappointed multiple times.

# R exercise.
# d. 

prariedog.data <- read.csv(file="~/r-projects/Day 4/PrairieDogMultipleMating.csv", 
                          header=TRUE, sep=",") # loading prarie dog data

(prariedog.table <- table(prariedog.data))

chisq.test((table(prariedog.table))) # running chi-square contingency test
# warning message -> Chi-squared approximation may be incorrect. p-value = 0.9029

# e. 

# In order to run a fisher test, the data must be a 2x2 contingency table. Thus I put the number of times mated
# as a binary option, either 1 or more than 1 (2 represents all numbers more than 1).

for (i in 1:263) { # looping through the data frame
  if (prariedog.data[i,1] > 1) { # if the number of times mated is greater than 1
    prariedog.data[i,1] = 2 # set the number of times mated to 2
  }
}

(prariedog.table <- table(prariedog.data)) # turn the dataframe into a frequency table

fisher.test(table(prariedog.data)) # run the fisher test

# We get a p-value of 0.56, so we cannot reject the null hypothesis. The issue about the fisher test
# is that we've had to reduce the dimensionality of our data to either mated once or more than once. Thus
# the conclusions from such a test are less interesting than running a test that preserves the high-dimensionality
# of the data. But yes, the conclusions from the fisher test are similar to the chi-square test. Both tests do not
# suggest the null hypothesis is incorrect.

# f. 

prariedog.data <- read.csv(file="~/r-projects/Day 4/PrairieDogMultipleMating.csv", 
                           header=TRUE, sep=",") # loading prarie dog data

(prariedog.table <- table(prariedog.data)) # turn data into a table

prariedog.matrix <- as.matrix(prariedog.table)  # turn table into matrix

(prob.birth <- rowSums(prariedog.matrix[,]) / sum(prariedog.matrix)) # probability of giving birth and not giving birth
(prob.mate <- colSums(prariedog.matrix[,] / sum(prariedog.matrix))) # probability of mating with 1, 2, 3, 4, or 5 times

expected <- c() # initialize the vector "expected", which will hold the expected values

# Calulating the expected values. We use a nested for-loop to calculate the probability of 
# having given birth AND mated once, having given birth AND mated twice, ..., did not give birth AND mated five times.
for (i in prob.birth) { # returns birth probabilities
  t <- c()
  for (j in prob.mate) { # returns mating number probabilities
    t <- c(t, i*j)
  }
  expected <- c(expected, t) # store expected value probabilties in this vector
}

(expected * sum(prariedog.matrix)) # This prints the expected frequencies in a vector

# Problem 2

# a. This is an observational study

# b. 

(proportion.sick <- 1020/1272) # proportion of kids with ALL and sig. outside exposure

(proportion.notsick <- 5343/6238) # proportion of kids without ALL and sig. outside exposure

# e. This question cannot be answered without knowing the confidence intervals, which would have been calculated
# in part d. 
# 
# f. Some confounding variables could be: exposure to radiation and diet.

# Problem 3. 
widowed <- c(28, 47, 57) # the first element is the number who experience marked deterioration in health, second element
# is moderate deterioration in health, and the element is number who experienced no deterioration in health
not.widowed <- c(7, 31, 60) # same format as "widowed"

(widow.freq <- rbind(widowed, not.widowed))



(prob.widow <- rowSums(widow.freq[,]) / sum(widow.freq)) # probability of giving birth and not giving birth
(prob.deterioration <- colSums(widow.freq[,] / sum(widow.freq))) # probability of mating with 1, 2, 3, 4, or 5 times

expected <- c() # initialize the vector "expected", which will hold the expected values

# Calulating the expected values. We use a nested for-loop to calculate the probability of 
# having given birth AND mated once, having given birth AND mated twice, ..., did not give birth AND mated five times.
for (i in prob.widow) { # returns birth probabilities
  t <- c()
  for (j in prob.deterioration) { # returns mating number probabilities
    t <- c(t, i*j)
  }
  expected <- c(expected, t) # store expected value probabilties in this vector
}

(expected <- (expected * sum(widow.freq))) # This prints the expected frequencies in a vector
(expected)
(observed <- c(widowed, not.widowed)) # creating the observed value vectors

(chi.square <- (observed - expected)^2)

(chi.square.val <- c(chi.square[1] / expected[1]))

chi.square.val <- c()
length(expected)
for (i in 1:length(expected)) {
  print(expected[i])
  print(chi.square[i])
  (t <- chi.square[i] / expected[i])
  (chi.square.val <- c(chi.square.val, chi.square[i] / expected[i]))
}
(chi.square.val <- sum(chi.square.val)) # degrees of freedom (3-1)(2-1) = 2. Thus the chi-value is 5.991.
# Our value of 11.177 means our result is statistically significant! Our p-value is in between 0.01 and 0.001.
# This means there is an association between husband death and deterioration in widow health.

# R exercise.
widowhealth.data <- read.csv(file="~/r-projects/Day 4/WidowHealth.csv", 
                           header=TRUE, sep=",") # loading widow health data
# a. 

chisq.test((table(widowhealth.data))) #running chi-square test on data-set

# b. The function t() transposes the columns and rows. 
chisq.test(t((table(widowhealth.data)))) # running the test with transposed rows and columns we see that 
# there is no difference!

# Problem 4.
# 53% of the rewardless orchid are yellow. The remaining flowers are purple.
# a. The probability of drawing a purple flower is 1 - 0.53 = 0.47 = 47%

# b. The probability is simply the probability of getting either 3, 4, or 5 all yellow.

# Tests from k to n, where k is the number of successful trials we are starting from,
# n is the total number of trials, and where p is the null success rate. Below is a function that computes
# the binomial test.
binomial.prob <- function(n, k, p) {
  bino.prob <- 0 # initialize binomial probability
  for (i in k:n) {
    bino.prob <- bino.prob + choose(n, i) * (p)^i * (1 - p)^(n - i) # use binomial formula to calculate each probability and add to tot probability.
  }
  return(bino.prob) # return total probability
}
  
n <- 5
k <- 3
p <- 0.53

(prob.3.yellow <- binomial.prob(n, k, p)) # probability that at least 3 are yellow in 5 trials: approx. 56%.


# c. The expected standard deviation is given by: sqrt((p(1-p))/n)
  
(std <- sqrt( (p * (1 - p)) / n)) # the expected standard deviation

# d. The probability that no more than 150 flowers are yellow given n = 263, is 1 minus the following probability:
  
n <- 263
k <- 150
p <- 0.53

(prob.above.150 <- binomial.prob(n, k, p)) # probability that more than 150 flowers are yellow out of a pop. of 263 is approx. 11%.

(prob.below.150 <- 1 - prob.above.150) # probability that no more than 150 flowers are yellow out of a pop. of 263 is approx. 89%.

# Problem 5. 
# The mean being 0.569 means the proportion of traffic fatalities due to drunken driving is quite high. More than half of all accidents
# (looking at the mean), are due to drunk driving.

# a. This is simply the area under the normal distribution from 0.65 to the right. Since 0.65 is approximately 1 standard deviation to the right,
# and since 68% of the values lie within 1 standard deviation of the mean, then approximately 84% (50% + (68/2)%) of the values lie to the left
# of 0.65 and thus 100% - 84% = 16% of the values lie to the right of 0.65. Thus, we would expect roughly 16% of states to have more than 65% of their
# traffic fatalities from drunk driving.


# b. We know that to the left of one standard deviation from the mean is the 16th percentile. So the 25th percentile is a value between one standard deviation
# to the left of the mean. Therefore the value is between 0.569 - 0.068 = 0.501 and 0.569

# Problem 6. 
# a. Since confidence interval is based partly on sample size, if both researchers had different sample sizes, their intervals would be different.

# b. The second researcher, because their interval is larger implying their sample size was smaller.

# c. We cannot be 100% certain about our answer from part b because it is hypothetically possible that the samples taken could have just had a much higher
# standard deviation for the second researcher.

# Problem 7. 
# 
# a. Drawing a graph showing the frequency distribution. 

walk.angle <- c(-5.19, -1.2, -0.5, -0.33, -0.15, -0.15, -0.15, -0.07, 0.02, 
                0.02, 0.28, 0.37, 0.45, 1.76, 2.8)
hist(walk.angle, 
     main="Walking Angle Frequencies", 
     xlab="Angles", 
     col="grey",
     las=1,
     xlim = c(-6, 4),
     breaks=10) # plotting the angle frequencies

# From the graph, a trend in the mean angle is suggested and is near 0.

# b. The mean angle is:

(sum(walk.angle)/length(walk.angle)) # The mean angle is -0.136.

# Based on our results from the previous question, the mean angle is -0.136 and therefore the statement
# “People do not have a tendency to turn more in one direction, on average, than the other direction” is not
# justified.

# R exercises.

walking.data <- read.csv(file="~/r-projects/Day 4/WalkingInCircles.csv", 
                             header=TRUE, sep=",") # loading walking angle data
hist(walking.data[,1], 
     main="Walking Angle Frequencies", 
     xlab="Angles", 
     col="grey",
     las=1,
     xlim = c(-6, 4),
     breaks=10) # plotting the angle frequencies

# We can change the bin width by configuring breaks. A larger breaks values puts more
# bins into the histogram and also breaks up the bars.

(walking.mean <- mean(walking.data[,1])) # mean of the data

(walking.median <- median(walking.data[,1])) # median of the data

#Computing the mode of the data. First we turn our data into a frequency table.
# Then we sort the table so the value with the highest frequency is in the first element.
# Then we return the first element.

# function that computes the mode
getmode <- function(data) {
  t <- sort(table(walking.data[,1]), decreasing = TRUE) #we sort the table so the value with 
  # the highest frequency is in the first element.
  
  t.names <- names(t) # get vector of header names
  t.mode <- as.numeric(t.names[1]) # access the first element and turn it into a number
  return(t.mode) # returns the mode
}

# Since standard error is standard error over the square root of the sample size:
(walking.stderror <- sd(walking.data[,1])/sqrt(length(walking.data[,1]))) # std error of the data

