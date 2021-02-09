#install.packages("plot.matrix")
library('plot.matrix')
library(plyr)
library(dplyr)
library(readr)
library(hms)
library(chron)

### This script generates matrices resulting from running Kolmogorov–Smirnov tests between each pair of meals
## (1) A test-statistic matrix with D-statistic values
## (2) A p-value matrix

#### Import data ####

basedir <- ("~/OneDrive - The Pennsylvania State University/Bari-files-Manual-Box-Transfer/00_PennState/Bout_Project")
setwd(file.path(basedir,"Data/Intervals/"))

myfiles = list.files(pattern="*.csv")
list.files()
dat_csv = ldply(myfiles, read_csv)

#### Prepare dataframes ####
# set class of variables
dat_csv$IBI <- as.numeric(dat_csv$IBI)
dat_csv$InterEatInt_f <- as.numeric(dat_csv$InterEatInt_f)
dat_csv$InterEatInt_n <- as.numeric(dat_csv$InterEatInt_n)
dat_csv$chewtimes <- as.numeric(dat_csv$chewtimes)
dat_csv$id <- as.numeric(dat_csv$id)

# Recode mis-coded variables
# Note: For AUR0076, "eat_f" is coded as "eat" ; "eat_n" is coded as "other chew"
unique(dat_csv$Annotation)
dat_csv$Annotation[dat_csv$Annotation == "eat"] <- "eat_f"
dat_csv$Annotation[dat_csv$Annotation == "drinK"] <- "drink"
dat_csv$Annotation[dat_csv$Annotation == "other chew"] <- "eat_n"

# subset data
meals <- subset(dat_csv, Paradigm == "Meal") # This dateframe excludes drinks
snacks <- subset(dat_csv, Paradigm == "Snack") # This dateframe excludes drinks

# Make full ID variables in dataframe
meals$FullID <- paste(meals$parid, meals$session, meals$Paradigm, sep="_")
snacks$FullID <- paste(snacks$parid, snacks$session, snacks$Paradigm, sep="_")

# Make a wide dataset with 1 column per meal, 1 row per bite entry. This method will pad NAs so columns are of equal length
IEI_wide_meal <- dcast(data = meals, id~FullID, value.var='InterEatInt_n')
IEI_wide_snack <- dcast(data = meals, id~FullID, value.var='InterEatInt_n')

#### Generate meal matrices ####

d.mat_meal <- matrix(, nrow = 0, ncol = 30)
p.mat_meal <- matrix(, nrow = 0, ncol = 30)

for (c in 2:31) {
  r <- IEI_wide_meal[,c]
  
  p.val <- sapply(IEI_wide_meal[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(p.value=ks$p.value)
    setNames(c(ks$p.value), c("p.value"))
  })
  
  d.val<- sapply(IEI_wide_meal[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(statistic=ks$statistic)
  })
  
  p.mat_meal <- rbind(p.mat_meal, p.val)
  d.mat_meal <- rbind(d.mat_meal, d.val)
}

#rename row and column names
rownames(p.mat_meal) <- colnames(IEI_wide_meal[2:31])
colnames(p.mat_meal) <- colnames(IEI_wide_meal[2:31])
rownames(d.mat_meal) <- colnames(IEI_wide_meal[2:31])
colnames(d.mat_meal) <- colnames(IEI_wide_meal[2:31])

#### Generate snack matrices ####
# For each meal, make 2 matrices, separating p-values and test-statistics
d.mat_snack <- matrix(, nrow = 0, ncol = 30)
p.mat_snack <- matrix(, nrow = 0, ncol = 30)

for (c in 2:31) {
  r <- IEI_wide_snack[,c]
  
  p.val <- sapply(IEI_wide_snack[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(p.value=ks$p.value)
    setNames(c(ks$p.value), c("p.value"))
  })
  
  d.val<- sapply(IEI_wide_snack[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(statistic=ks$statistic)
  })
  
  p.mat_snack <- rbind(p.mat_snack, p.val)
  d.mat_snack <- rbind(d.mat_snack, d.val)
}

#rename row and column names
rownames(p.mat_snack) <- colnames(IEI_wide_snack[2:31])
colnames(p.mat_snack) <- colnames(IEI_wide_snack[2:31])
rownames(d.mat_snack) <- colnames(IEI_wide_snack[2:31])
colnames(d.mat_snack) <- colnames(IEI_wide_snack[2:31])



#### Visualize matrices ####

## View rounded numerical matrices
round(p.mat_snack, 2)
round(d.mat_snack, 2)

## with colors

# note: with plot(as.pvalue()) the breaks are set to 0, 0.1, 0.05, 0.01, 0.001 and 1
# Significant p-values indicate that the tested pair has different distribitions 

plot(d.mat_meal, main="Meal paradigm IEI: KStest D stat matrix", axis.col=list(side=1, las=2), axis.row = list(side=2, las=1), cex.axis = .5, col=topo.colors)
plot(as.pvalue(p.mat_meal), main="Meal paradigm IEI: KStest p-value matrix", axis.col=list(side=1, las=2), axis.row = list(side=2, las=1), cex.axis = .5, digits = NA)

plot(d.mat_snack, main="Snack paradigm IEI: KStest D stat matrix", axis.col=list(side=1, las=2), axis.row = list(side=2, las=1), cex.axis = .5, col=topo.colors)
plot(as.pvalue(p.mat_snack), main="Snack paradigm IEI: KStest p-value matrix", axis.col=list(side=1, las=2), axis.row = list(side=2, las=1), cex.axis = .5, digits = NA)



