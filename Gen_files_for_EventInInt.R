#install.packages("plyr")
library(plyr)
#install.packages("microbenchmark")
library(microbenchmark)
library(lubridate)

#### import data ####

#basedir <- ("~/Box/0-Bari_files/00_PennState/Bout_Project")
basedir <- ("~/OneDrive - The Pennsylvania State University/Bari-files-Manual-Box-Transfer/00_PennState/Bout_Project")
setwd(file.path(basedir,"Data/Intervals/"))

myfiles = list.files(pattern="*.csv")
list.files()
dat_csv = ldply(myfiles, read_csv)

#### set class of variables ####
dat_csv$IBI <- as.numeric(dat_csv$IBI)
dat_csv$InterEatInt_f <- as.numeric(dat_csv$InterEatInt_f)
dat_csv$InterEatInt_n <- as.numeric(dat_csv$InterEatInt_n)
dat_csv$chewtimes <- as.numeric(dat_csv$chewtimes)
dat_csv$id <- as.numeric(dat_csv$id)

#### Recode mis-coded variables ####
# Note: For AUR0076, "eat_f" is coded as "eat" ; "eat_n" is coded as "other chew"
unique(dat_csv$Annotation)
dat_csv$Annotation[dat_csv$Annotation == "eat"] <- "eat_f"
dat_csv$Annotation[dat_csv$Annotation == "drinK"] <- "drink"
dat_csv$Annotation[dat_csv$Annotation == "other chew"] <- "eat_n"

# Recode StartTimes to be in seconds
dat_csv$StartTime_seconds <- period_to_seconds(hms(dat_csv$start_time))

#### subset data ####
meals <- subset(dat_csv, Paradigm == "Meal") # This dateframe excludes drinks
snacks <- subset(dat_csv, Paradigm == "Snack") # This dateframe excludes drinks

#### Make a list of unique dataframes by parid and session ####
splitMeals <- split(meals, interaction(meals$parid, meals$session))
splitSnacks <- split(snacks, interaction(snacks$parid, snacks$session))

## example of how to access a unique column of a unique dataframe
splitMeals[["AUR0029.1"]]$parid

#### Isolate eat_f events ####
splitMeals_eatf <- lapply(splitMeals, subset, Annotation == "eat_f")
splitSnacks_eatf <- lapply(splitSnacks, subset, Annotation == "eat_f")


#### For each dataframe in the list, make a new variable EventTime_seconds_adjusted, which adjusted StartTime so that the first bite occurs at time = 0
splitMeals_eatf <- lapply(splitMeals_eatf, function(x) cbind(x, StartTime_seconds_adjusted = x$StartTime_seconds - first(x$StartTime_seconds)))

splitSnacks_eatf <- lapply(splitSnacks_eatf, function(x) cbind(x, StartTime_seconds_adjusted = x$StartTime_seconds - first(x$StartTime_seconds)))


##### Write unique csv files containing BITES ONLY (eat_f) for each individual eating event #####
setwd(file.path(basedir,"Data/Split/BitesOnly"))
sapply(names(splitMeals_eatf), 
       function (x) write.csv(splitMeals_eatf[[x]], file=paste(x, "_meal_bitesonly.csv", sep="")))
sapply(names(splitSnacks_eatf), 
       function (x) write.csv(splitSnacks_eatf[[x]], file=paste(x, "_snack_bitesonly.csv", sep="")))