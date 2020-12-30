# The purpose of this script is define and use event_in_interval() to make time-series data to indicate if an event occured within a specfied interval
# Input into event_in_interval() will be a file containing a "StartTime_seconds_adjusted" column (in seconds) that indicates each time the event occured
# Input files can be generated using Gen_files_for_EventInt.R

library(plyr)
library(dplyr)

#### Import files ####
# set directory
basedir <- ("~/Box/0-Bari_files/00_PennState/Bout_Project")
setwd(file.path(basedir,"Data/Split/BitesOnly/"))

# Load files
file_names = list.files(pattern="*meal*", full.names = TRUE)
files = lapply(file_names, read.csv, header = TRUE)

# Load 1 file for testing script
AUR0029.1 <- read.csv("AUR0029.1_meal_bitesonly.csv", stringsAsFactors = FALSE)

#### Define function for defining event (bite) occurance ####
# Function will: 
## (1) Calculate the total time of the recording
## (2) Take an input interval (e.g. 2 seconds) and make a new dataframe (Interval.df) that contains a row for every interval within the recording (e.g., if recording is 30s and interval is 2s, B will contain 15 rows)
## (3) For each row in Interval.df (i.e., each interval), will look to input file to see if an event occured during that time; indicate TRUE or FALSE

event_in_interval <- function(data, interval) {
  # Calculate total time of recording
  TotalTime = last(data$StartTime_seconds_adjusted) - first(data$StartTime_seconds_adjusted)
  cat("Total recording is TotalTime: ", TotalTime, "\n")
  
  Intdur = interval
  cat("Interval Duraction is: ", Intdur, "\n")
  
  # Calcaulte total number of intervals in the recording
  TotalIntervals = ceiling(TotalTime/Intdur)
  cat("Total intervals in recording: ", TotalIntervals, "\n")
  
  # Make dataframe with 1 row per interval
  Interval.df = data.frame("IntNum" = 1:TotalIntervals)
  
  # Calculate lower time threshold for each interval
  Interval.df$lowerthresh <- (Interval.df$IntNum*Intdur) - Intdur
  
  # Calculate upper time threshold for each interval
  Interval.df$upperthresh <- (Interval.df$IntNum*Intdur)
  
  # Make variable to indicate occurance of event
  Interval.df$EventOccur = NA
  
  # Make variable to indicate event count
  Interval.df$EventCount = NA

  # For each interval, Event = TRUE if there is any event in Input_clean that occured between upper and lower threshold for that interval
  Intnums = unique(Interval.df$IntNum)
  for (Int in Intnums) {
    upperthresh = (Interval.df$upperthresh[Interval.df$IntNum == Int])
    lowerthresh = (Interval.df$lowerthresh[Interval.df$IntNum == Int])
    Interval.df$EventOccur[Interval.df$IntNum == Int] = (any(data$StartTime_seconds_adjusted >= lowerthresh & data$StartTime_seconds_adjusted < upperthresh))
    eventcount = 0
    for (eventnum in 1:nrow(data)) {
      if (isTRUE(between(data$StartTime_seconds_adjusted[eventnum], lowerthresh, upperthresh))) {
        eventcount = eventcount + 1
      }
    }
    Interval.df$EventCount[Interval.df$IntNum == Int] = eventcount
  }
  return(Interval.df)
}

#### Apply function; output "EventInInt" dataframes for each meal ####


for (file in files) {
  if (nrow(file) == 0) {
    print("no data")
  }
  else if (nrow(file) > 1) {
    session <- (file$session[1])
    parid <- print(file$parid[1], max.levels = 0)
    interval = 4
    assign(paste(parid,session,"EventInInt",interval,sep = "_"), event_in_interval(file, interval))
  }
}
