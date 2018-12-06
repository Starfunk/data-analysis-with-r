#
#My first R script - learning how to program in R!
#

1 + 1

store.number <- 4
store.number = 4

store.number = 8

# show what is stored in object store.number. The period is purely syntactical.
store.number

# to name functions, use camelcase, to name variables use periods.
# exploring the structure of the function 'str()'
# the smooth brackets indicate that it is a function

?str # returns a summary of the str() function

ha <- "Hello world"


str(store.number) #returns type of variable and value of variable - here it is number
str(ha) #returns type of variable and value of variable - here it is a character string

# doing arrays, c stands for concatenate
t.numbers <- c(3,5,8,9,4,3,6,7,4,3,5,6,3,2,6,7,87,5,4,3,5,6,3,6)

# indices start at 1. Return second to 4th elements:
t.numbers[2:4]

str(t.numbers)

# what is a matrix?
?matrix

# making a matrix, if you put round brackets around it, it instantly shows it in the console
#(t.matrix <- matrix(c(4,4,3,2),4,1)) # second arg: num of rows, third arg then num of columns

(t.matrix <- matrix(c(4,4,3,2),2,2))

# let's give the columns names
#colnames(t.matrix <- c("num1","num2"))

# accessing element and replace element

t.matrix[1,2] <- "gone"
t.matrix[2,2] <- NA # NA is a missing value

# remove everything
#rm(t.matrix)

str(t.matrix)

# making a dataframe
t.vector <- c(1,2,3,9)
t.char <- c("blue","red","yellow","green")
t.data.frame <- cbind.data.frame(t.char, t.vector)
str(t.data.frame)

x <- c(1,2,100,245,9,3)

x[4:6] 
x[c(5,5,5)]
x[c(1,2,3)]
x[c(4,4,6,1)]


x[length(x)]
which(x > 0)
x[x > 0]


z <- x >= 95

# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)
# Print the data frame.			
emp.data

# A simple dataframe
(max.data <- data.frame(
  days = c("Monday","Tueday","Wednesday","Thursday","Friday"),
  food = c("Pasta","Burger","Lasagna","Tofu","Rice"),
  calorie = c(455,344,709,450,600)
))
max.data$days # printing days vector
max.data$calorie # printing calorie vector



# writing a simple for loop in R, iterating over an array of years.
for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

for (num in t.numbers){
  print(paste("My number is", t.numbers))
}

sample = c(4,5,6,7,7,8,9,5,3,2,2,4,5,6)
mean(sample)

j <- seq(from = 1, to = 20, by =  3)

str(j)

#adding in sections
#-------Vector-------


sum(c(2,2,2)-c(1,1,1))
str(rep(4,3))
p = c(2,3,4)
p^2

# defining a function

fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- ((temp_F - 32) * (5 / 9)) + 273.15
  return(temp_K)
}

fahrenheit_to_kelvin(100) - 273.15

# function to frequency side project
# format.frequency <- function(frequencies.num, levels) {
#   total.freq <- c()
#   for (freq.num in frequencies.num){
#     for (level in levels) {
#       freq <- rep(level,freq.num)
#       total.freq <- c(total.freq, freq)
#       break
#     }
#   }
# }
# 
# (c <- rep.int(0, 38))
