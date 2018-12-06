# MAXIMILIAN'S R LIBRARY OF USEFUL CODE. THIS IS A REFERENCE RESOURCE THAT
# DOCUMENTS ALL THE CODE I HAVE USED FOR MY DATA ANALYSIS IN R CLASS.

# 1. OBJECTS
# 2. FUNCTIONS
# 3. LOOPS
# 4. GRAPHS
# 5. SAVING DATA
# 6. READING AND WRITING WORKING DIRECTORIES

# 1. ------------------------------OBJECTS--------------------------------------

# THIS IS AN OVERVIEW OF BASIC DATA STRUCTURES IN R. BASIC USAGE OF VECTORS
# LISTS, MATRICES, AND DATAFRAMES IS COVERED.

# VECTORS:
# The most basic building block in R is the vector. We can create a simple 
# vector as follows:
vec <- c(1,2,3,4) # A simple vector 

# How to index vectors
vec[3] # index the vector and return the third element, 3.

# LISTS:
# A list acts as a general container that can hold any data structure. Useful in 
# funtions that need to return multiple types!

(ex.list <- list(1, "hello world", TRUE, 1+4i)) # An example list with 
# multiple types.

# How to index lists
ex.list[3] # index the list and return the third element, TRUE.

# MATRICES: 
# A way to store higher dimensional data, basically a multi-dimensional vector.
(mat <- matrix(1:6, nrow = 2, ncol = 3)) # A simple matrix, the first arg 
# specifies the values of the elements

# Another way to construct matrices is to bind vectors together.
x <- 1:11 # first vector
y <- 10:20 # second vector
(rbind(x,y)) # row bind the vectors, or
(cbind(x,y)) # column bind the vectors. If the vectors are not the same length, 
# the shorter one will repeat.

# How to index matrices
mat[1,2] # index first the row and then the column

# DATAFRAMES: 
# Dataframes are the next step from matrices. Dataframes are useful because R 
# has built-in methods to compute useful properties of dataframes.

#A simple dataframe: The variable names becomes the names of the columns, and 
# the vectors become columns.
(max.data <- data.frame(
  days = c("Monday","Tueday","Wednesday","Thursday","Friday"),
  food = c("Pasta","Burger","Lasagna","Tofu","Rice"),
  calories = c(455,344,709,450,600)
))

# How to index dataframes
max.data[ , 1:2] # returns a new dataframe with only the first columns and all 
#the rows.
max.data[1:2, 1] ## returns a new dataframe with only the first column and only 
#the first two rows.

# How to return a specific part of a dataframe using which()!
# This example returns the max dataframe without the row containing Monday.
(max.data.no.mon = max.data[which(max.data[,1] !="Monday"),])

# 2. -----------------------------FUNCTIONS-------------------------------------

# THIS IS A COLLECTION OF FUNCTIONS I HAVE FOUND TO BE USEFUL WHILE PROGRAMMING
# IN R. AT THE BOTTOM OF THIS SECTION ARE FUNCTIONS THAT I HAVE MADE FOR 
# VARIOUS PROGRAMMING TASKS.

vec <- c(1,2,3,4,5,6,7) # test vector we will use for our functions
mat <- rbind(1:3,7:9) # test matrix we will use for our functions

# LENGTH(): Length returns the number of elements a vector/list/matrix possesses

length(vec) # length of the vector

# SUM(): sums all the elements in a vector/matrix
sum(mat) # sum of the vector

# DIM(): returns the dimensions of a matrix
dim(mat) # returns the dimensions of our matrix. Row first and then columns.

# SEQ()
# A function that initializes a vector based on a sequential pattern
# For example, the first element is 4.5, the final element is 3.0 and each 
# element in between has a difference of -0.5 with the previous element.
(seq(from = 4.5, to = 3.0, by = -0.5))

# REP()
# A function that initializes a vector based on a repetitive pattern
(rep(c(0, 0, 7), times = 4)) # this command creates a vector with the vector 
# c(0,0,7) repeated 4 times.

# CHOOSE(): returns n choose k
choose(5, 2) # returns 5 choose 2

# FACTORIAL(): returns x factorial
factorial(8) # returns 8 factorial

# CONVERSION FUNCTIONS: A set of functions that convert data types:

# AS.NUMERIC()
num <- "10"
(num <- as.numeric(num)) # Convert a character into a number

# AS.CHARACTER()
num <- 29 # Initialize number 
(num <- as.character(num)) # Convert a number into a character

# AS.VECTOR()
(mat <- matrix(1:6, nrow = 2, ncol = 3))  # Initialize a matrix
(mat <- as.vector(mat)) # Squishes matrix into a vector, appends third column 
# to second column to first column.

# AS.MATRIX()
(vec <- c(1,2,3,4,5,9)) # Initialize a matrix
(mat <- as.matrix(vec)) # Convert to dataframe

# AS.DATA.FRAME()
(mat <- matrix(1:6, nrow = 2, ncol = 3)) # Initialize a matrix
(mat <- as.data.frame(mat)) # Convert to dataframe

# AS.LIST()
vec <- c(1,2,3,4,5,6,9)
(vec <- as.list(x)) # Convert vector to list

# TYPE TEST FUNCTIONS: A set of functions that test the data type

# IS.NUMERIC()
num <- 1
(is.numeric(num)) # Returns TRUE because num is numeric

# IS.CHARACTER()
num <- 1
(is.character(num)) # Returns FALSE because num is a character.

# IS.VECTOR()
num <- "Character"
(is.vector(num)) # Will return for single element vectors as well! 
# Will only return false if object is a matrix or dataframe.

# IS.MATRIX() 
vec <- c(5,5,5,5,5,5,5)
(is.matrix(vec)) # Returns FALSE because vec is a vector.

# IS.DATA.FRAME()
(mat <- matrix(1:6, nrow = 2, ncol = 3)) # Initialize a matrix
(mat <- as.data.frame(mat)) # Convert matrix into a dataframe
(is.data.frame(mat)) # Returns TRUE becomes mat is a dataframe now.

# HOMEMADE FUNCTIONS

# This function takes as input a vector of the frequencies and a vector of 
# the magnitude of the frequencies. E.g. given the frequencies c(38,5,1) and 
# the mag. of frequencies as c(0, 1, 2), the function returns the vector 
# c(0, 5, 2). I used this function for problem 2 on problem set 2. 
format.frequency <- function(frequencies, levels) {
  total.freq <- c() # initialize a vector, which we will store our values in.
  counter <- 0 # A counter to index the vector "levels"
  for (freq.num in frequencies) { # loop through every value in "frequencies"
    counter <- counter + 1
    freq <- rep(levels[counter],freq.num) # create a vector that repeates the 
    # levels value, "freq.num" times.
    total.freq <- c(total.freq, freq) # append the freq vector to the vector 
    # we are going to eventually return.
  }
  return(total.freq) # return our vector with all values appended
}
# Example frequencies and levels. Note that frequencies and levels should be 
# the same length!
frequencies <- c(38,17,7,6,4,10,2,0,0) # This was the vector for males.
levels <- c(0:8)
(format.frequency(frequencies, levels)) # We end up with the dataset we had to 
# import for the problem!

# For one-tailed binomial tests. Tests from k to n, where k is the number of 
# successful trials in is the total number of trials, and where p is the null 
# success rate. Below is a function that computes the binomial test.
binomial.prob <- function(n, k, p) {
  bino.prob <- 0 # initialize binomial probability
  for (i in k:n) {
    # use binomial formula to calculate each probability and add to
    # the total probability
    bino.prob <- bino.prob + choose(n, i) * (p)^i * (1 - p)^(n - i) 
  }
  return(bino.prob) # return total probability
}

# We need to run correlations tests for every column in our dataset against 
# every other column. This function takes in a dataframe and applies the input 
# function on every combination of 2 columns and puts the results in a matrix
# where each element is the correlation coefficient for columns i and j.
correlations <- function(dataframe) {
  # Create matrix to store corr coefficients
  size <- length(dataframe[1,]) # size is the number of columns
  (mat <- matrix(1:size^2, nrow = size, ncol = size)) # create matrix
  # Setting column and row names of matrix
  names <- colnames(dataframe) # returns a vector of column names
  colnames(mat) <- names # set matrix column names to be the same as dataframe
  rownames(mat) <- names # set matrix row names to be the same as column names
  (mat <- as.data.frame(mat)) # convert matrix to dataframe
  # This is not the most efficient way to do the loop but I think it makes the
  # most sense to do it this way. We choose the first column (i) and then 
  # compare to every column (including itself) with the index j.
  for (i in 1:size) { # for i in 1:(number of columns)
    for (j in 1:size) {
      # "complete.obs discards" the row of values if an NA value is present
      mat[i,j] <- cor(dataframe[,i],dataframe[,j], use = "complete.obs")
    }
  }
  return(mat) # return the dataframe of correlation values
}

# This function returns the median of the data set
getmedian <- function(data) {
  data.length <- length(data) # length of data
  # Test to see if data length is even or odd
  # If the length of the data is even then take the mean of the middle data
  if (data.length %% 2 == 0) { 
    middle1 <- data.length / 2
    middle2 <- middle1 + 1
    median <- (data[middle1] + data[middle2]) / 2
  } else { # otherwise the median is the middle value in the vector
    # ceiling round half the length to get the middle position in the vector
    middle1 <- ceiling(data.length / 2) 
    median <- data[middle1]
  }
  return(median)
}

# Creating the normal distribution function that returns a set of numbers 
# conforming to the distribution
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
  # plot the normal distribution y-values against x!
  plot(x, normal.dist(mean, std), type="l", xlab="x value",
       ylab="Density", main="Normal Distribution") 
}

# Testing the normal.plot functon
mean <- 25
std <- 19
normal.plot(mean, std) # example plot, mean = 25, std = 19.

# 3. -----------------------------------LOOPS-----------------------------------

# THIS SECTION OUTLINES USING FOR AND WHILE LOOPS IN R.

# FOR LOOPS: For loops take some variable and given a range or set of objects 
# will loop through the range or the set of objects.

# Looping through a range:
for (counter in 1:4) { # every loop, "counter" takes on the value in the vector
  print(counter) # prints the value of counter
}

# Looping through a set of objects:
colours <- c("RED", "BLUE", "GREEN") # initialize a vector of colours.
for (colour in colours) { 
  # colour first takes on the value "RED", then "BLUE", and then "GREEN".
  print(colour)
}

# WHILE LOOPS: Unlike for loops, while loops keep on looping until some 
# condition is satisfied. For the loop below, the program will continue 
# looping until counter = 10.
counter <- 0 # initialize counter to 0
while (counter < 10)  { # condition to be met if the loop is to exit.
  print(counter) # print the current value of the counter.
  counter <- counter + 1 # increment the value of counter by 1.
}

# APPLY(): Instead of looping through a complex data structure like a matrix or
# dataframe, apply() gives us the option to specify how we want a function to 
# be applied to a matrix or dataframe.

(apply(iris[ ,1:4], 2, sum)) # Here we specify that we want to have sum 
# applied to columns 1 to 4 and the 2 specifies we are summing columns not 
# rows. Apply returns the sums of the columns. The POWER of apply is that it is 
# able to interface with the dataframe object and make a complex computation 
# (applying a function to specific parts of the dataframe in a specific way) 
# very simple.

# 4. -------------------------------------GRAPHS--------------------------------

# THIS SECTION OUTLINES SOME USEFUL GRAPHS TO DISPLAY DATA.

# BAR GRAPH

# create the data for the plot
frequency2 = c(10,20,30,40)
groups = c("Group 1", "Group 2", "Group 3", "Group 4")
# plot the data in a bar graph, las = 1 puts group names horizontal
# while las = 2 puts group names horizontal.
# names.arg specifies that the horizontal axis use labels rather than a
# continuous number line. xlab stands for x-axis label, same for y-lab.
# main sets the title of the graph.
# col sets the colour of the bars.
a <- barplot(frequency2, names.arg=groups, xlab="Groups",
             ylab="Frequency", main="Frequency per group",las = 1,col = "black")

# SCATTER PLOT

# plot(x, y), pch determines fill, col is colour, xlim gives scale for x-axis.
max.colour <- c("red","blue") # store colours in a vector
# pch gives the shape of the plotted points. col determines the colours of the 
# plotted points, and xlim determines the x-axis range.
plot(c(1,1,2,3,4,5,6,6,7,8,7,9),c(1:12), pch = 19, col = "blue", xlim = c(0,6))
(m.t <- lm(c(1:12) ~ c(1,2,3,4,5,3,5,3,5,3,4,3)))
str(m.t)
abline(m.t) # adds linear regression line


# HISTOGRAM

# Walking angle data to plot in a histogram
walk.angle <- c(-5.19, -1.2, -0.5, -0.33, -0.15, -0.15, -0.15, -0.07, 0.02, 
                0.02, 0.28, 0.37, 0.45, 1.76, 2.8)

# An example histogram, plotting the angle frequencies from walk.angle
hist(walk.angle, 
     main="Walking Angle Frequencies", 
     xlab="Angles", 
     col="grey",
     las=1,
     xlim = c(-6, 4),
     breaks=10) 

# BOX PLOT

# Creating plant data
tree.height <- c(5,6,3,7,4,9,5,3,8,9)
bush.height <- c(2,3,5,3,2,2,3,2,4,5)
plant.mat <- cbind(tree.height, bush.height)

# Plotting plant data in a box plot!
boxplot(plant.mat, ylab="Height", xlab="Plant Type", main="Height vs. Plant",
        col="red")

# 5. ---------------------------SAVING DATA-------------------------------------

# HOW TO SAVE FILES AS CSV (COMMA-SEPARATED VALUES)
#First come up with a name for the file and save
# the name in its own variable. Then use the write.csv function. 

# Create dataframe we can then save as a CSV
(temp.data <- data.frame(
  day = c(1,2,3,4,5,6,7,8),
  temp = c(25,26,25,27,29,31,33,30)
))

write.csv(temp.data) # Saving raw data as CSV to raw data folder

# HOW TO SAVE VALUES IN A PDF
# First run the save pdf environment pdf()
pdf("pdfname")

# Then run your plots which will be saved to the pdf 
plot(temp.data[,1],temp.data[,2])

# To stop saving to the pdf, run dev.off.
dev.off()

# 6. --------------READING AND WRITING WORKING DIRECTORIES----------------------

# Reading data
data <- read.csv(file="~/r-projects/data.csv", 
                             header=TRUE, sep=",") # loading data.csv

# Getting and setting working directory
getwd() # Look up current working directory
setwd("/Users/maximiliankahn/r-projects/Weather\ Data\ Analysis") # Set new wd

# Creating new folders in working directory
output.folder.names <- c("Raw Data", "Data Plots", "Data Analysis") # names of
# folders to be created
for(i in 1:length(output.folder.names)) # loop through folder names and if
  # folder name does not exist, create folder under folder name
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])

# Storing paths in global variables
p.raw.data <- paste(getwd(), "/Raw\ Data/", sep = "")
p.data.plots <- paste(getwd(), "/Data\ Plots/", sep = "")
p.data.analysis <- paste(getwd(), "/Data\ Analysis/", sep = "")



