# Maximilian Kahn - R Problem Set 1
#
# Problem 1
#
# a. Number of sexual partners in a year: numerical/discrete
# b. Petal area of rose flowers: numerical/continuous
# c. Heart beats per minute of a Tour de France cyclist, averaged over the duration
# of the race: numerical/continuous
# d. Birth weight: numerical/continuous
# e. Stage of fruit ripeness (e.g., underripe, ripe, or overripe): categorical/ordinal
# f. Angle of flower orientation relative to position of the sun: numerical/continuous 
# g. Tree species: categorical/nominal
# h. Year of birth: numerical/discrete 
# i. Gender: categorical/nominal

# Problem 2
# 
# a. Such a decision would skew the data to people who still have a home phone -
# such people might be older since younger people tend to only have a cell phone.
# 
# b. equal chance of being selected
# 
# c. accuracy
#
# Problem 3
# 
# a. explanatory variable: possessing mutation in vkorc1 gene
#    response variable: survival
#   
# b. explanatory variable: the type of treatment
#    response variable: anxiety scores
#    
# c. explanatory variable: sensitivity to images of appetizing food
#    response variable: activity of fronto–striatal–amygdala–midbrain
#    
# d. explanatory variable: amount of endostatin dosage
#    response variable: level of growth of tumors
#
# Problem 4
# 
# a. observational study
# 
# b. experimental study
# 
# c. observational study
# 
# d. experimental study
#
# Problem 5
# 
# a. A frequency distribution table
# 
# b. One variables: number of convictions
# 
# c. 21 boys had exactly two convictions
# 
# d. 265/395
# 
# e. A bar graph is appropriate because we can compare the frequency as a function of the number of
# convictions easily. (Bar graphs do a good job displaying ordinal categorical data, which is the 
# kind of data we have).

# create the data for the plot
num.convictions = c(0:14)
frequency1 = c(265,49,21,19,10,10,2,2,4,2,1,4,3,1,2)

# plot the data in a bar graph
barplot(frequency1, names.arg=num.convictions,xlab="Number of Convictions",
        ylab="Frequency",main="Frequency of convictions")

# f. The graph follows a "hockey stick" shape. The graph is heavily skewed to the right.
# The graph is unimodal with a clear peak at 0 convictions. The mode (the most
# frequent value) is 2. I don't think there are any clear outliers in this dataset.
# Some may think that having 4 boys having 11 convictions is an outlier given that 
# only 1 boy has 10 convictions but I would not call this an outlier, I would just 
#say that this is an expected result from sampling only a few hundred individuals.

# g. There are certain aspects of the study that suggest it was not really a random
# sample. For one thing, all the schools were near the research office (they would all
# have been subject to the same local environment). Also If one individual was involved 
#in crime, it would probably be more likely that others would be as well.

# Problem 6
# 
# a. Making the plot 3D makes it really hard to match the value of each group to a frequency. 
# Having each group represented as a cone adds another layer of implied semantics that does 
# not actually exist. Why cones? What do they mean?
#   
# b. The fact that the cones have points makes it really difficult to match the tips of the cones
# to values. Also the colour scheme seems to be pretty arbitrary and communicates to the viewer
# that the creator of the graph did not think very hard about the meaning of the graph. Finally, it is
#hard to read the labels because the font is so small.

# c. Improving on the graph.

# create the data for the plot
frequency2 = c(10,20,30,40)
groups = c("Group 1", "Group 2", "Group 3", "Group 4")

# plot the data in a bar graph, las = 1 puts group names horizontal
# while las = 2 puts group names horizontal.
a <- barplot(frequency2, names.arg=groups, xlab="Groups",
        ylab="Frequency", main="Frequency per group",las = 1)


