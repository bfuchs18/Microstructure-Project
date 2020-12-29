# The purpose of this script is to make time-series data to indicate if an event occured within a specfied interval
# Input will be a file containing a "time" column (in UNIT?) that indicates the time at which an event occured.

# Function will: 
## (1) Calculate the total time of the recording
## (2) Take an input interval (e.g. 2 seconds) and make a new dataframe ('B') that contains a row for every interval within the recording (e.g., if recording is 30s and interval is 2s, B will contain 15 rows)
## (3) For each row in dataframe B (i.e., each interval), will look to input file to see if an event occured during that time; indicate 1 or 0

# Goal is to make a function out of this script
## Ex) Genseries(data, timeinterval)

#### Import file ####
# set basedir
basedir <- ("~/Box/0-Bari_files/00_PennState/Bout_Project")

# set directory for generated datasets
setwd(file.path(basedir,"Data/Annotation/"))

# Load dataset
Input <- read.csv("AUR0029_visit1.csv", stringsAsFactors = FALSE)

#### Clean up imported file ####

# Convert start time to seconds
#install.packages("lubridate")
library(lubridate)
Input$EventTime_seconds <- period_to_seconds(hms(Input$StartTime))

# Only keep rows with eat_f (i.e., event of interest)
Input_clean <- Input[(Input$Annotation == "eat_f"),]

# Modify EventTimes so Bite 1 occurs at time = 0
library(dplyr)
# Get time of first bite 
firstbitesec <- first(Input_clean$EventTime_seconds)
# Subtract time of first bite from all bite times -- Bite 1 now occurs at time = 0
Input_clean$EventTime_seconds_adjusted <- Input_clean$EventTime_seconds - firstbitesec


#### Calculate total duration and total number of intervals ####
TotalTime <- last(Input_clean$EventTime_seconds_adjusted) - first(Input_clean$EventTime_seconds_adjusted)

# Set interval duration {this will be an input into the function}
Intdur <- 5

# Calculate number of intervals (round up to nearest whole number)
TotalIntervals <- ceiling(TotalTime/Intdur)

#### Determine if an event occured within each interval ####

# Make a column with rows containing numbers 1 through TotalIntervals
Interval.df <- data.frame("IntNum" = 1:TotalIntervals)

# Calculate lower time threshold for each interval
Interval.df$lowerthresh <- (Interval.df$IntNum*Intdur) - Intdur

# Calculate upper time threshold for each interval
Interval.df$upperthresh <- (Interval.df$IntNum*Intdur)

# Make variable to indicate occurance of event
Interval.df$Event <- NA

Intnums <- unique(Interval.df$IntNum)
# For each interval, Event = TRUE if there is any event in Input_clean that occured between upper and lower threshold for that interval
for (Int in Intnums) {
  upperthresh <- (Interval.df$upperthresh[Interval.df$IntNum == Int])
  lowerthresh <- (Interval.df$lowerthresh[Interval.df$IntNum == Int])
  Interval.df$Event[Interval.df$IntNum == Int] <- (any(Input_clean$EventTime_seconds_adjusted >= lowerthresh & Input_clean$EventTime_seconds_adjusted < upperthresh))
}
