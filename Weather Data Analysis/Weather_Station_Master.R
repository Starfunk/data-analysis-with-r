# ANALYZING WEATHER DATA FROM THE WEATHER CAN LIBRARY - MASTER

library(weathercan) # Import weathercan library

#----[SET WORKING DIRECTORY AND CREATE FILES]-----------------------------------

getwd() # Look up current working directory
setwd("/Users/maximiliankahn/r-projects/Weather\ Data\ Analysis") # Set new wd

output.folder.names <- c("Raw Data", "Data Plots", "Data Analysis")
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])

p.raw.data <- paste(getwd(), "/Raw\ Data/", sep = "")
p.data.plots <- paste(getwd(), "/Data\ Plots/", sep = "")
p.data.analysis <- paste(getwd(), "/Data\ Analysis/", sep = "")

#----[LOOK UP STATIONS AND SEE WHICH ONES ARE APPROPRIATE TO USE]---------------

View(stations) # View stations in a spreadsheet

# Set station codes and location description
vancouver.id <- 888 #VANCOUVER HARBOUR CS
squamish.id <- 336 #SQUAMISH AIRPORT
pemberton.id <- 536 #PEMBERTON AIRPORT CS
lillooet.id <- 27388 #LILLOOET

#----[SET PARAMETERS FOR DATA COLLECTION]---------------------------------------

# Set start and end dates for data analysis
date <- c("2017-01-01", "2018-01-01")

# Set interval - hour, day, or month
interval <- "day"

# Set vector of station ids we are interested in looking at
station_ids <- c(vancouver.id, squamish.id, pemberton.id, lillooet.id)

#----[DOWNLOADING RAW DATA]-----------------------------------------------------

# Downloading Vancouver weather data
vancouver <- weather_dl(station_ids = station_ids[1], start = date[1], 
                       end = date[2], interval = interval)
vancouver$max_temp
# Downloading Squamish weather data
squamish <- weather_dl(station_ids = station_ids[2], start = date[1], 
                     end = date[2], interval = interval)
# Downloading Pemberton weather data
pemberton <- weather_dl(station_ids = station_ids[3], start = date[1], 
                       end = date[2], interval = interval)
# Downloading Lillooet weather data
lillooet <- weather_dl(station_ids = station_ids[4], start = date[1], 
                       end = date[2], interval = interval)

#----[CHECKING AND VISUALIZING DATA]--------------------------------------------

# Each vector should be 366 elements long. Prints a warning if length < 366!
# Checking max temperatures
checklength(vancouver$max_temp)
checklength(squamish$max_temp)
checklength(pemberton$max_temp)
checklength(lillooet$max_temp)

# Checking total precipitation
checklength(vancouver$total_precip)
checklength(squamish$total_precip)
checklength(pemberton$total_precip)
checklength(lillooet$total_precip)

# Plotting max temp and total precipitation values for all weather stations
pdf.raw.data.name <- "/Raw\ Data\ Plots.pdf" # naming the data plot pdf
pdf(paste(p.data.plots, pdf.raw.data.name, sep = "")) # saving plots in this pdf 

# Plotting max temperatures
plot(vancouver$max_temp, pch = 19, cex = 1, las = 1)
plot(squamish$max_temp, pch = 19, cex = 1, las = 1)
plot(pemberton$max_temp, pch = 19, cex = 1, las = 1)
plot(lillooet$max_temp, pch = 19, cex = 1, las = 1)

# Plotting total precipitation
plot(vancouver$total_precip, pch = 19, cex = 1, las = 1)
plot(squamish$total_precip, pch = 19, cex = 1, las = 1)
plot(pemberton$total_precip, pch = 19, cex = 1, las = 1)
plot(lillooet$total_precip, pch = 19, cex = 1, las = 1)

dev.off() # tells R we are done saving plots to pdf

#----[SAVING RAW DATA TO RAW DATA FOLDER]---------------------------------------

#Put data into a dataframe so that we can save it later as a CSV!
weather.data.raw <- data.frame(
  van.max.temp = vancouver$max_temp,
  van.tot.precip = vancouver$total_precip,
  squam.max.temp = squamish$max_temp,
  squam.tot.precip = squamish$total_precip,
  pem.max.temp = pemberton$max_temp,
  pem.tot.precip = pemberton$total_precip
  # DO NOT USE LILLOOET DATA FOR TOTAL PRECIPITATION - IT IS ALL NA
  # lil.max.temp <- lillooet$max_temp,
  # lil.tot.precip <- lillooet$total_precip
)

View(weather.data.raw) # Visualize weather.data.raw

# Saving raw weather data to a CSV
weather.data.raw.name <- "Weather\ Data\ Raw.csv" # Naming the file
write.csv(weather.data.raw, paste(p.raw.data, weather.data.raw.name, sep = ""),
          row.names = FALSE) # Saving raw data as CSV to raw data folder

#----[RUNNING TESTS ON DATA]----------------------------------------------------

# Running correlation tests on every column combination in a dataframe. 
#See the functions section for details on the "correlations" function.
weather.corr <- correlations(weather.data.raw)

View(weather.corr) # Check out the correlations dataframe

# If the matrix looks good, save it to a CSV for later reference!
# Saving raw weather data to a CSV
weather.corr.name <- "Weather\ Correlations.csv"
write.csv(weather.corr, paste(p.data.analysis, weather.corr.name, sep = ""))

#Visualing some correlation data! Store the correlation plots in the
# pdf "correlationataplot"

pdf.correlation.data.name <- "Correlation\ Data\ Plots.pdf" # naming the pdf
pdf(paste(p.data.plots, pdf.name, sep = "")) # saving the plots in this pdf

# max temperature vs. max temperature
plot(vancouver$max_temp,squamish$max_temp, pch = 19, cex = 1, las = 1)
plot(vancouver$max_temp,pemberton$max_temp, pch = 19, cex = 1, las = 1)
plot(squamish$max_temp,pemberton$max_temp, pch = 19, cex = 1, las = 1)

# total precipitation vs. total precipitation
plot(vancouver$total_precip,squamish$total_precip, pch = 19, cex = 1, las = 1)
plot(vancouver$total_precip,pemberton$total_precip, pch = 19, cex = 1, las = 1)
plot(squamish$total_precip,pemberton$total_precip, pch = 19, cex = 1, las = 1)

# max temperature vs. total precipitation
plot(vancouver$max_temp,squamish$total_precip, pch = 19, cex = 1, las = 1)
plot(vancouver$max_temp,pemberton$total_precip, pch = 19, cex = 1, las = 1)
plot(vancouver$total_precip,squamishn$max_temp, pch = 19, cex = 1, las = 1)
plot(vancouver$total_precip,pemberton$max_temp, pch = 19, cex = 1, las = 1)
plot(squamish$max_temp,pemberton$total_precip, pch = 19, cex = 1, las = 1)
plot(squamish$total_precip,pemberton$max_temp, pch = 19, cex = 1, las = 1)
dev.off() # End saving pdf