#
#
# problem set 3
#
#

# 7
# use the confidence interval of our estimate
# make use of the binom.test() function
n.succ <- 60    # number of successes
n.total <- 1000 # total number of trials
p.under.null <- n.succ/n.total
# run binomial test
binom.test(n.succ, n.total, p.under.null)
# CI is 
# 95 percent confidence interval:
#  0.04609504 0.07656049
# 0. 05 fall within, 0.02 not....

binom.test(n.succ, n.total, p.under.null)


# But we can also test it by defining specifc value as our null (p.under.null)
binom.test(n.succ, n.total, 0.02)
# p-value = 1.872e-13: proportion found is significantly different from null of 0.02
binom.test(n.succ, n.total, 0.05)
# not significantly different p-value = 0.1465



# 8 
list.files()
# load the data
bird <- read.csv("~/r-projects/Day 3/BirdWindowCrash.csv", stringsAsFactors = FALSE)
str(bird)
data.birds <- data.frame(table(bird))
colnames(data.birds) <- c("angle", "observed")

# add a new column
data.birds$expected <- sum(data.birds$observed) / 
                              length(data.birds$observed)
                           
data.birds$calc <- (data.birds$observed - data.birds$expected)^2 /
  data.birds$expected
# get the Chi scuare by taking sum of the calc column
(Chi.sq <- sum(data.birds$calc))
View(data.birds)


# g
chisq.test(data.birds$observed)



# 9 snapdragon
# d
# get the data into vectors
names <- c("red", "pink", "white")
# observed frequencies
small <- c(10, 21, 9)
names(small) <- names # add names to the vector
small.exp <- c(10,20,10)# expected frequencies
small.exp.prob <- small.exp/sum(small.exp)# turn expected in probabilities
# perform Chi^2 test
chisq.test(small, p = small.exp.prob)
# far from significant....

# e for the large sample size
# easiest way is to turn both expected and observed into probabilites
# we can use for our H0 value from e
expected.prop <- small.exp.prob
# for the observed it is
observed.prop <- small/sum(small)
# we can change the sample size and multiply sample size with observed
# probability to get 'observed'. Of course this should be a discrete nunmer
# in the real world but for now this approximation will do
sample.size <- 1600
chisq.test(observed.prop * sample.size, p = expected.prop)
# >1600 will do!