# Maximilian Kahn - R Problem Set 2

# Problem 1.
# 
# a. A histogram.
# 
# b. We know the mean will probably be between 800-1100 as the highest frequencies 
# occur around these speeds. Intuitively we know that mean is the centre 
# of mass of the figure so this answer seems plausible. I estimate the mean to be 1000.
# 
# c. The range is 1500 - 400 = 1100, thus the true median is 400 + 1100/2 = 950. Rounded
# to the nearest hundred we get 900.
# 
# d. The mode is the value that is most frequent. From the graph, the bin that corresponds 
# with this max value is 1000-1100.

# e. As most of the highest frequencies are centred around the mean, we expect to
# have a standard deviation that is not very large. Using the rule that 68% of a normal distribution
# (yes, we are assuming the given dist. is approximately normal) falls within one standard dev.
# we can approximate the standard deviation as 150.

# Problem 2.
# 
# a. Calculating mean for males
male.freq <- c(38,17,7,6,4,10,2,0,0) # male LRS frequencies
female.freq <- c(30,25,3,6,8,4,0,4,1) # female LRS frequencies
LRS <- c(0:8) # level of LRS

(male.mean <- sum((male.freq * LRS) / 84)) # computing LRS mean for males
(female.mean <- sum((female.freq * LRS) / 81)) # computing LRS mean for females

str(male.mean) # print male.mean
str(female.mean) # print female.mean

# The mean for males is 1.5, the mean for females is 1.7. Thus, females have the 
# higher mean lifetime success.

# b. Some males do not breed while all female house sparrows breed.
# 
# c. Calculating variance of LRS for males and females

# Calculating the variance we get: male variance approximately equal to 3.5 and female
# variance approximately equal to 4.3.

str(male.variance)
str(female.variance)

# d. Calculate mean LRS for sparrow CSV
# Read CSV into R
sparrow.data <- read.csv(file="~/r-projects/Day 2/SparrowReproductiveSuccess.csv", 
                         header=TRUE, sep=",")
View(sparrow.data) # view sparrow data in a spreadsheet

male.LRS <- sparrow.data[1:84,2] # index all the male LRS scores
male.LRS <- sparrow.data[c(1:84),c(2)] # same thing as above but expressed explicitly
str(male.LRS)
female.LRS <- sparrow.data[85:165,2] # index all the female LRS scores
mean(male.LRS) # mean for male LRS
mean(female.LRS) # mean for female LRS

# e. Calculate variance for males and females
var(male.LRS) # variance for male LRS
var(female.LRS) # variance for female LRS

# Calculate variance for males and females for LRS >= 4 and LRS > 4

# Returns a dataframe with all male rows with LRS values >= 4
sparrow.data[which(sparrow.data[1:84 ,2] >= 4),]

# Returns a dataframe with all female rows with LRS values >= 4
# For some reason this does not return the right subset
sparrow.data[which(sparrow.data[85:165,2] >= 4),]

# Instead we try separating data set based on first column sex value
male.greateq.4 <- sparrow.data[which(sparrow.data$sex == "male" & sparrow.data$lifetimeRS >= 4),]
female.greateq.4 <- sparrow.data[which(sparrow.data$sex == "female" & sparrow.data$lifetimeRS >= 4),]

var(male.greateq.4[,2]) # variance for male (>= 4)
var(female.greateq.4[,2]) # variance for females (>= 4)

# Now variance for 4 > 
male.great.4 <- sparrow.data[which(sparrow.data$sex == "male" & sparrow.data$lifetimeRS > 4),]
female.great.4 <- sparrow.data[which(sparrow.data$sex == "female" & sparrow.data$lifetimeRS > 4),]

var(male.great.4[,2]) # variance for male (>= 4)
var(female.great.4[,2]) # variance for females (>= 4)

# Problem 3.
# 
# a. Since we are assuming the sampling was random, the standard error of the mean in men
# and women is given by std/sqrt(n)

(st.error.men <- 6.7/sqrt(4620))
(st.error.women <- 4.6/sqrt(6228))

# b. The standard deviation is better descriptor as the standard deviation is a measurement
# of the spread (i.e. variation) of the data.

# c. The standard error is a better descriptor in this case. This is because standard
# error is a measurement of the uncertainty of how far our estimate is from the true parameter.
# 
# d. Humans can have multiple relationships over the span of 5 years, thus, it is likely
# that women had more relationships than men in that time span. The data suggests that 
# engaged in relationships far less than women did. Also, men might have had relationships with
# women outside of the study. 

# Problem 4.

# a. Mean and std of beetles per flower

num.beetles <- c(51,45,61,76,11,117,7,132,52,149)

(beetle.mean <- mean(num.beetles)) # mean of beetles per flower
(std.beetle <- sd(num.beetles)) # std of beetles per flower

 # b. Calculating standard error of sample to calculate 95% CI interval

(beetle.error <- std.beetle/sqrt(length(num.beetles)))

# c. calculating 95% intervals
(low.bound <- beetle.mean - 2 * beetle.error) # lower bound
(up.bound <- beetle.mean + 2 * beetle.error) # upper bound

# d. With a larger sample, the mean will be about the same as it currently is.
# 
# e. With a larger sample, the std will be about the same as it currently is. 
# 
# f. d. With a larger sample, we would expect the standard error to be smaller since
# n would be larger (and standard error is inversely related to n).

# g. Loading corpseflower data into R

corpseflower.data <- read.csv(file="~/r-projects/Day 2/Corpseflowers.csv", 
                            header=TRUE, sep=",")
# visualizing data
View(corpseflower.data)
barplot(corpseflower.data[,1],xlab = "Days",ylab = "Number of Beetles")

num.beetles <- corpseflower.data[,1] # preprocessing dataframe for computation
(beetle.mean <- mean(num.beetles)) # mean of beetles per flower
(std.beetle <- sd(num.beetles)) # std of beetles per flower
(beetle.error <- std.beetle/sqrt(length(num.beetles))) # std error of the sample

# h. generating new dataset using rnorm
# for n = 25
(num.beetles.2 <- round(rnorm(25, mean = beetle.mean, sd = std.beetle)))
(beetle.mean.2 <- mean(num.beetles.2)) # check mean
(beetle.std.2 <- sd(num.beetles.2)) # check std
(beetle.error.2 <- beetle.std.2/sqrt(length(num.beetles.2))) # check error

# for n = 50
(num.beetles.3 <- round(rnorm(50, mean = beetle.mean, sd = std.beetle)))
(beetle.mean.3 <- mean(num.beetles.3)) # check mean
(beetle.std.3 <- sd(num.beetles.3)) # check std
(beetle.error.3 <- beetle.std.3/sqrt(length(num.beetles.3))) # check error

# for n = 100
(num.beetles.4 <- round(rnorm(100, mean = beetle.mean, sd = std.beetle)))
(beetle.mean.4 <- mean(num.beetles.4)) # check mean
(beetle.std.4 <- sd(num.beetles.4)) # check std
(beetle.error.4 <- beetle.std.4/sqrt(length(num.beetles.4))) # check error

# Problem 5.
# 
# The graph on the left shows all the data while the graph on the right hides data.
# Therefore, the graph on the left is superior.



# Problem 6.
# 
# a. According to the table, coaches receive the longest hugs while competitors
# receive the shortest hugs on average. The values represent sample estimates, because
# we can't be sure that we captured all the hugs across all athletes.

# b. Calculating standard error of the mean for each group. The standard error is 
# calculated as the standard deviation of its sampling distribution (i.e. distribution
# of sampling mean). Standard error, in the context of this question, tells us the spread
# of the sampling distribution and is a measure of how close our sample means 
# (for coach, supporter, and comp.) are to their true means.

(coach.error <- 3.96/sqrt(77)) # standard error for coach hugs
(supporter.error <- 2.76/sqrt(75)) # standard error for supporter hugs
(competitor.error <- 1.13/sqrt(33)) # standard error for competitor hugs

# c. We are assuming the true population distribution is a normal distribution and that
# our samples are random. We are also assuming the standard deviations given are for 
# the population so we calculated the actual standard error of the mean and not an estimate 
# for the standard error.

# d. Calculating the 95% confidence for the competitors grouping.

(competitor.mean <- 1.81) # mean for competitor group
(low.bound <- competitor.mean - 2 * competitor.error) # lower bound
(up.bound <- competitor.mean + 2 * competitor.error) # upper bound

# e. Yes, 2 seconds is plausible because 95% of the time the true mean falls within the interval
# 1.18 - 2.44. 
# 
# f. Which group is a 3-second mean hug duration in the population plausible?

# Calculating 95% intervals for coach
(coach.mean <- 3.77) # mean for coach group
(low.bound <- coach.mean - 2 * coach.error) # lower bound
(up.bound <- coach.mean + 2 * coach.error) # upper bound  

# Calculating 95% intervals for supporter
(supporter.mean <- 3.16) # mean for supporter group
(low.bound <- supporter.mean - 2 * supporter.error) # lower bound
(up.bound <- supporter.mean + 2 * supporter.error) # upper bound  
  
# A 3-second mean hug duration is plausible in both the coach and supporter populations