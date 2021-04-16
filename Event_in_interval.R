# The purpose of this script is define and use event_in_interval() to make time-series data to indicate:
    # (1) if an event occured within a specfied interval 
    # (2) how many events occured within each specific interval
    # (3) The average inter-eating-interval (IEI, time between end of chewing and start of next bite) during that interval

# Input into event_in_interval() will be a file containing a "StartTime_seconds_adjusted" column (in seconds) that indicates each time the event occured
# If IEI option is used, input file also requires column with IEI intervals (for use here, column is named InterEatInt_n)
# Input files can be generated using Gen_files_for_EventInt.R

library(plyr)
library(dplyr)
library(data.table)

#### Import files ####
# set directory
basedir <- ("~/OneDrive - The Pennsylvania State University/Bari-files-Manual-Box-Transfer/00_PennState/Bout_Project")
setwd(file.path(basedir,"Data/Split/BitesOnly/"))

# Load files
meal_file_names = list.files(pattern="*meal*", full.names = TRUE)
meal_files = lapply(meal_file_names, read.csv, header = TRUE)

snack_file_names = list.files(pattern="*snack*", full.names = TRUE)
snack_files = lapply(snack_file_names, read.csv, header = TRUE)

# Load 1 file for testing script
AUR0153.3 <- read.csv("AUR0153.3_meal_bitesonly.csv", stringsAsFactors = FALSE)

#### Define function for defining event (bite) occurance ####
# Function will: 
## (1) Calculate the total time of the recording
## (2) Take an input interval (e.g. 2 seconds) and make a new dataframe (Interval.df) that contains a row for every interval within the recording (e.g., if recording is 30s and interval is 2s, B will contain 15 rows)
## (3) For each row in Interval.df (i.e., each interval), will look to input file to see if an event occured during that time; indicate TRUE or FALSE
## (4) For each row in Interval.df (i.e., each interval), will look to input file to count how many events occured during that time; output count
## (5) For each row in Interval.df (i.e., each interval), will look to input file to average IEI over the given interval (optional use: IEI = TRUE)

event_in_interval <- function(data, unit, interval, IEI) {
  
  if (unit == "percentage") {
    cat("unit is percentage", "\n")

    # Calculate total time of recording
    TotalTime = last(data$StartTime_seconds_adjusted) - first(data$StartTime_seconds_adjusted)
    cat("Total recording is TotalTime: ", TotalTime, "\n")
    
    # Indicate total number of intervals in the recording
    TotalIntervals = interval
    cat("Total intervals in recording: ", TotalIntervals, "\n")    
    
    # Determine size of each interval
    Intdur = TotalTime/interval
    cat("Interval Duration is: ", Intdur, "\n")
  }
  
  if (unit == "time") {
    cat(" unit is time", "\n")

    # Calculate total time of recording
    TotalTime = last(data$StartTime_seconds_adjusted) - first(data$StartTime_seconds_adjusted)
    cat("Total recording is TotalTime: ", TotalTime, "\n")
    
    # Indicate interval duration
    Intdur = interval
    cat("Interval Duration is: ", Intdur, "\n")
    
    # Calcaulte total number of intervals in the recording
    TotalIntervals = ceiling(TotalTime/Intdur)
    cat("Total intervals in recording: ", TotalIntervals, "\n")
  }
  
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
  
  # For each interval, set upper and lower thresholds, indicate EventOccur = TRUE if there is any event in input data that occured between the thresholds
  Intnums = unique(Interval.df$IntNum)
  for (Int in Intnums) {
    upperthresh = (Interval.df$upperthresh[Interval.df$IntNum == Int])
    lowerthresh = (Interval.df$lowerthresh[Interval.df$IntNum == Int])
    Interval.df$EventOccur[Interval.df$IntNum == Int] = (any(data$StartTime_seconds_adjusted >= lowerthresh & data$StartTime_seconds_adjusted < upperthresh))
    eventcount = 0
    lastrow = nrow(data)
    
    if (IEI == TRUE) {
      # Make an empty list 
      IEIlist = c()
    }
    
    # Reference each event from input dataset and count how many events occured between lower and upperthresholds for each interval
    for (eventnum in 1:nrow(data)) {
       if (eventnum != lastrow) {
         # For all intervals except the last, do not include the upper boundary
          if (isTRUE(data$StartTime_seconds_adjusted[eventnum] >= lowerthresh & data$StartTime_seconds_adjusted[eventnum] < upperthresh)) {
            eventcount = eventcount + 1
            if (IEI == TRUE) {
              IEIlist <- append(IEIlist, data$InterEatInt_n[eventnum])
            }
          }
        }
      if (eventnum == lastrow) {
        # For the last interval, include the upper boundary
          if (isTRUE(data$StartTime_seconds_adjusted[eventnum] >= lowerthresh & data$StartTime_seconds_adjusted[eventnum] <= upperthresh)) {
            eventcount = eventcount + 1
            if (IEI == TRUE) {
              IEIlist <- append(IEIlist, data$InterEatInt_n[eventnum])
            }
          }
      }
    }
    
    # Add EventCount and IEI_avg to output dataframe    
    Interval.df$EventCount[Interval.df$IntNum == Int] = eventcount
  
    if (IEI == TRUE ) {
      print(IEIlist)
      if (sum(!is.na(IEIlist)) > 0) {
        Interval.df$IEI_avg[Interval.df$IntNum == Int] = mean(IEIlist, na.rm = TRUE)
      } else {
        Interval.df$IEI_avg[Interval.df$IntNum == Int] = Intdur
      }
    }
  }
  
  # Make cumulative sum variables
  Interval.df$EventCount_cumulative <- cumsum(Interval.df$EventCount)
  Interval.df$EventOccur_cumulative = cumsum(as.numeric(Interval.df$EventOccur))
  
  return(Interval.df)
}

#### Apply function; output individual or compiled dataframes ####

test <- event_in_interval(AUR0153.3, "percentage", 10, IEI = TRUE)

# Set desired intervals
intlist <- list(10)
#unit = "time"
unit = "percentage"

## Modify loop to loop through both snack and meal files
for (interval in intlist) { 
    # Make temporary empty dataframe that all data will be added to
    temp <- data.frame(matrix(ncol = 7, nrow = 0))
      #for (file in meal_files) {
      for (file in snack_files) {
        #mealtype = (file$Paradigm[1])
        if (nrow(file) == 0) {
          print("no data")
        }
        
        else if (nrow(file) > 1) {
          sesnum <- (file$session[1])
          parid <- print(file$parid[1], max.levels = 0)
          output <- event_in_interval(file, unit, interval, IEI = TRUE)
          output$ParID <- parid
          output$session <- sesnum
          output$ID <- as.character(paste(output$ParID, output$session, sep = "_"))
          
          ## Make separate dataframe for meal/participant data
          #assign(paste(parid,sesnum,"EventInInt",interval,sep = "_"), event_in_interval(file, interval))
          
          # Add participant's data to compiled dataframe
          temp <- rbind(temp, output)
        }
      }
    
    # Modify name of temporary dataframe
    #assign(paste0("Meal_Event_in_", unit, "_interval_", interval), temp)
    assign(paste0("Snack_Event_in_", unit, "_interval_", interval), temp)
    rm(temp)
}

