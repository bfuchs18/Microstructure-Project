# The purpose of this script is define and use IBI_in_interval() to make time-series data to calculate the average IBI within a specified interval

# Input into IBI_in_interval() will be a file containing a "StartTime_seconds_adjusted" column (in seconds) that indicates each time the event occured and the corresponding IBI
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
AUR0029.1 <- read.csv("AUR0029.1_meal_bitesonly.csv", stringsAsFactors = FALSE)

#### Define function for defining event (bite) occurance ####
# Function will: 
## (1) Calculate the total time of the recording
## (2) Take an input interval (e.g. 2 seconds) and make a new dataframe (Interval.df) that contains a row for every interval within the recording (e.g., if recording is 30s and interval is 2s, B will contain 15 rows)
## (3) For each row in Interval.df (i.e., each interval), will look to input file and calculated average IBI in that time frame

IBI_in_interval <- function(data, unit, interval) {

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
  
  # Make variable to indicate average IBI
  Interval.df$IBI_avg = NA
  
  # For each interval
  Intnums = unique(Interval.df$IntNum)
  for (Int in Intnums) {
    upperthresh = (Interval.df$upperthresh[Interval.df$IntNum == Int])
    lowerthresh = (Interval.df$lowerthresh[Interval.df$IntNum == Int])
    
    # Indicate if a bite occurred (this will act as a check to make sure there is an IBI average for every interval an event occured)
    Interval.df$EventOccur[Interval.df$IntNum == Int] = (any(data$StartTime_seconds_adjusted >= lowerthresh & data$StartTime_seconds_adjusted < upperthresh))
    
    lastrow = nrow(data)
    
    # Make an empty list 
    IBIlist = c()
    
    # Average IBI for every bite within interval number
    for (eventnum in 1:nrow(data)) {
       # For all eventnum except the last, exclude upper boundary
       if (eventnum != lastrow) {
        # If eventnum occured within interval, add IBI to IBIlist
        if (isTRUE(data$StartTime_seconds_adjusted[eventnum] >= lowerthresh & data$StartTime_seconds_adjusted[eventnum] < upperthresh)) {
          IBIlist <- append(IBIlist, data$IBI[eventnum])
        }
       }
      # For last eventum, include the upper boundary
      if (eventnum == lastrow) {
        # If eventnum occured within interval, add IBI to IBIlist
        if (isTRUE(data$StartTime_seconds_adjusted[eventnum] >= lowerthresh & data$StartTime_seconds_adjusted[eventnum] <= upperthresh)) {
          IBIlist <- append(IBIlist, data$IBI[eventnum])
        }
      }
    }
    
    print(IBIlist)
    Interval.df$IBI_avg[Interval.df$IntNum == Int] = mean(IBIlist, na.rm = TRUE)
  }

  return(Interval.df)
}

#### Apply function; output individual or compiled dataframes ####

test <- IBI_in_interval(AUR0029.1, "percentage", 20)



# Set desired intervals
intlist <- list(5)
#unit = "time"
unit = "percentage"

## Modify loop to loop through both snack and meal files
for (interval in intlist) { 
    # Make temporary empty dataframe that all data will be added to
    temp <- data.frame(matrix(ncol = 7, nrow = 0))
      for (file in meal_files) {
      #for (file in snack_files) {
        #mealtype = (file$Paradigm[1])
        if (nrow(file) == 0) {
          print("no data")
        }
        
        else if (nrow(file) > 1) {
          sesnum <- (file$session[1])
          parid <- print(file$parid[1], max.levels = 0)
          output <- IBI_in_interval(file, unit, interval)
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
    assign(paste0("Meal_IBI_in_", unit, "_interval_", interval), temp)
    #assign(paste0("Snack_IBI_in_", unit, "_interval_", interval), temp)
    rm(temp)
}

