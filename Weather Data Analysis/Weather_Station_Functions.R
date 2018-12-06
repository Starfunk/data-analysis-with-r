# ANALYZING WEATHER DATA FROM THE WEATHER CAN LIBRARY - FUNCTIONS

#----[DEFINING FUNCTIONS FOR USE IN THIS SCRIPT]----------------------------------

# All the data we use should have a length of 366, since we are looking at
# data over one year and a time interal of 1 day. This function makes sure there
# are 366 elements in the input vector. 
checklength <- function(data) {
  if (length(data) == 366) {
    print("The data is good!")
  } else {
    data.name <- deparse(substitute(data)) # get name of data object
    data.length <- as.character(length(data)) # length of data as a string
    print(paste("WARNING!", data.name, "has a length of", 
                data.length, sep = " ")) # Prints a warning if length < 366
  }
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