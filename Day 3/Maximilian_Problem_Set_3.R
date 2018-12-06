# Maximilian Kahn - R Problem Set 3

# Problem 1.
# 
# a. The probability of rain on a random day is (because each season is the same length):
  
(rain.prob <- (0.58 + 0.38 + 0.25 + 0.53) / 4)

# b. Finding probability it is a winter day:
(tot.prob <- 0.58 + 0.38 + 0.25 + 0.53) # Total probability proportion between seasons

(winter.prob <- 0.58 / tot.prob) # Winter proportion over total proportion gives the probability it is raining and a winter day.

# There is a 1/3 chance it is a winter day if it is raining and the day was randomly chosen.

# Problem 2.
# 
# a. The probability of possessing two C alleles,given that they are independent is:
  
(C.prob <- 0.13 * 0.13)

# b. THe probability that a randomly sampled individual is the sum of the individual probabilities:

(homozygote.prob <- 0.83^2 + 0.13^2 + 0.04^2)

# c. Probability that an individual is AS, recall that there are two combinations to account for: AS and SA:

(AS.prob <- 0.83 * 0.04 + 0.04 * 0.83)
  
# Thus, the probability is  approx 7%

# d. The probability that an individual is AS or AC is their probabilities added.

(AS.AC.prob <- AS.prob + 2 * 0.83 * 0.13)

# Thus, the probability is roughly 28%.

# Problem 3. 
# 
# a. alternative hypothesis
# 
# b. null hypothesis
# 
# c. alternative hypothesis
# 
# d. null hypothesis
# 
# e. alternative hypothesis

# Problem 4. 

# a. It it a two-sided test because the accuracy can be worse than before the experiment or better
# than before the experiment.

# b. The probability of attaining those accuracies given the null hypothesis.
# 
# c. The test statistic here is accuracy scores after 21 days. I.e. the number of subjects improving.
# d. The p-value associated with 10/12 subjects improving is 2 * (prob(10) + prob(11) + prob(12))

(nostril.p.value <- 2 * (0.016 + 0.003 + 0.0002))

# Thus, the p-value is: 0.0384

# e. We can say that it is unlikely that the null distribution is correct with a
# proportion of p = 0.5!

# Problem 5.
# 
# a. The null hypothesis is that there is no change in accuracy. The alternative hyp. is
# that there is a change. 
# 
# b. 
# 
# Problem 6.
# 
# a. Her success rate is 4/10 = 0.4, her expected success rate is 2/10 = 0.2.

# b. We carry out a binomial test to see if her success rate is reliable evidence that
# she is an ESP. We assume that she cannot do worse than random, so this is a one-tailed
# test. Where N is equal to 10 (the number of trials) and p is 0.2 (the null proportion).
# The probability is thus:

# For one-tailed binomial tests. Tests from k to n, where k is the number of successful trials,
# n is the total number of trials, and where p is the null success rate.
binomial.prob <- function(n, k, p) {
  bino.prob <- 0
  for (i in k:n) {
    bino.prob <- bino.prob + choose(n, i) * (p)^i * (1 - p)^(n - i)
  }
  return(bino.prob)
}

(binomial.prob(10, 4, 0.2)) # ESP binomial test, returns: 0.1208739

# 0.12 is greater than 0.05, so we cannot conclude that random guessing was not used.

# c. 
(binomial.prob(1000, 350, 0.2))  # ESP binomial test, returns: 1.747434e-28
# Because we are dealing with a much larger sample, the chance that she does 175% better than
# random is extremely small if she is randomly guessing. The incredibly small probability
# from the binomial test shows that getting 350 right out of 1000 is highly unlikely if
# the null hypothesis is true.


# Problem 7. Since 60/1000 is equal to 6%, the data is pretty consistent with 5%.
roadkill.data <- read.csv(file="~/r-projects/Day 3/RoadKill.csv", 
                              header=TRUE, sep=",") # loading dataset
all.swerve = roadkill.data[which(roadkill.data[,1] !="no swerve"),] # indexing rows that contain "swerve"
(swerve.prob <- length(all.swerve) / nrow(roadkill.data)) # calculating the probability of
# swerving based on the dataset. Note that we count the number of rows in swerve set using 
# length() and nrow() in the roadkill dataset.

# Creating the function
roadkill.prob <- function(roadkill.data) {
  all.swerve = roadkill.data[which(roadkill.data[,1] !="no swerve"),]
  (swerve.prob <- length(all.swerve) / nrow(roadkill.data))
  return (swerve.prob)
}

#Calling the function
roadkill.prob(roadkill.data)

# Problem 8. 
# 
# a. Null hypothesis: window angle does not affect bird mortality rates; alternatie hypo:
# window angle does affect bird mortality rates.

# b. The proportion killed by completely vertical windows is:
(30 / (30 + 15 + 8))

# c. Use a chi-squared goodness-of-fit test

# d. Carrying out a chi-square test

(expected.death <- (30 + 15 + 8) / 3)
(measured.death <- c(30, 15, 8))

chi.square <- function(measured,expected) {
  chi.square.value = sum((measured - expected)^2 / expected)
  return(chi.square.value)
}

(bird.chi.square <- chi.square(measured.death, expected.death))

pchisq(bird.chi.square, df=2, lower.tail=FALSE) # calculating p-value from chi square value, first arg
# is the number of observations, df is the degrees of freedom.

# It is very statistically unlikely  that this value would come from the 
# null distribution. Thus, this is strong evidence the window angle likely affects bird mortality.

# Problem 8 R-exercise

# Loading birdkill dataset:
birdcrash.data <- read.csv(file="~/r-projects/Day 3/BirdWindowCrash.csv", 
                          header=TRUE, sep=",") # loading dataset
View(birdcrash.data) # viewing data

vertical = birdcrash.data[which(birdcrash.data[,1] !="40 degrees" & 
                                  birdcrash.data[,1] != "20 degrees"),]

twenty.degrees = birdcrash.data[which(birdcrash.data[,1] !="40 degrees" & 
                                  birdcrash.data[,1] != "vertical"),]

forty.degrees = birdcrash.data[which(birdcrash.data[,1] !="vertical" & 
                                  birdcrash.data[,1] != "20 degrees"),]

chi.square2 <- function(measured, expected) {
  return((measured - expected)^2 / expected)
} # returns a component of the full chi-square value

# calculating expected value
expected.value <- ((length(vertical)) + (length(twenty.degrees)) + (length(forty.degrees))) / 3

chi1 <- chi.square2(length(vertical), expected.value)
chi2 <- chi.square2(length(twenty.degrees), expected.value)
chi3 <- chi.square2(length(forty.degrees), expected.value)
  

# Making dataframe, first column observed value, second column expected value
(bird.df.data <- data.frame(
  measured = c(length(vertical),length(twenty.degrees),length(forty.degrees)),
  expected = c(expected.value, expected.value, expected.value),
  chisquare = c(chi1, chi2, chi3)
))

(chisq.test(bird.df.data[ , 1])) # The chi-squared test. We get a p value of 0.0007841! 
# Which is less than 0.001, just as expected.

# Problem 9. 
# 
# a. No. Performing a chi-squared test to see if the variance is significant.

measured <- c(10, 21, 9)
expected <- c(10, 20, 10)
 
(chi.value <- chi.square(measured, expected)) # returns a chi square value of 0.15, 
# the results do not differ sigficantly!

# b. Same thing for 100 times more flowers:
measured2 <- c(1000, 2100, 900)
expected2 <- c(1000, 2000, 1000)

(chi.value <- chi.square(measured2, expected2)) # returns a chi square value of 15
# c. So while the proportions are the same, the chi squared tests are different because 
# magnitudes of the values increase (and since we are working with larger magnitudes)
# the chi square values are different. In this case, the results differ significantly
# from the null hypothesis of a ratio of 1:2:1!

# d. Redoing analysis using chisq()
p <- c(0.25, 0.5, 0.25)
chisq.test(measured, p = c(0.25, 0.5, 0.25)) # chi-square test for measured
chisq.test(measured2, p = c(0.25, 0.5, 0.25)) # chi-square test for measured2

# e. Sample size that results in a difference that is significant
# Keeping observed proportion constant!
sample.size <- 1600
chisq.test((measured/sum(measured)) * sample.size, p = c(0.25, 0.5, 0.25)) # chi-square test for measured

# It's decently close to the null-hypothesis so it might be hard to accept that
# the null hypothesis is probably wrong given our feeble human intuitions.
