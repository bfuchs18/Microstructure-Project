### This script generates matrices resulting from running Kolmogorov–Smirnov tests between each pair of meals
## (1) A test-statistic matrix with D-statistic values
## (2) A p-value matrix
## (3) A combined matrix with test-stats and p-values

# Make full ID variable in dataframe
meals$FullID <- paste(meals$parid, meals$session, meals$Paradigm, sep="_")

# Make a wide dataset with 1 column per meal, 1 row per bite entry. This method will pad NAs so columns are of equal length
IEI_wide <- dcast(data = meals, id~FullID, value.var='InterEatInt_n')

### Make 2 matrices, separating p-values and test-statistics
d.mat <- matrix(, nrow = 0, ncol = 30)
p.mat <- matrix(, nrow = 0, ncol = 30)

for (c in 2:31) {
  r <- IEI_wide[,c]
  
  p.val <- sapply(IEI_wide[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(p.value=ks$p.value)
    setNames(c(ks$p.value), c("p.value"))
  })
  
  d.val<- sapply(IEI_wide[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(statistic=ks$statistic)
  })
  
  p.mat <- rbind(p.mat, p.val)
  d.mat <- rbind(d.mat, d.val)
}

#rename row and column names
rownames(p.mat) <- colnames(IEI_wide[2:31])
colnames(p.mat) <- colnames(IEI_wide[2:31])

rownames(d.mat) <- colnames(IEI_wide[2:31])
colnames(d.mat) <- colnames(IEI_wide[2:31])

### Make 1 matrix with p-values and test-statistics
full.mat <- matrix(, nrow = 0, ncol = 30)
for (c in 2:31) {
  r <- IEI_wide[,c]
  res <- sapply(IEI_wide[,2:31], function(y) {
    ks <- ks.test(r, y)
    c(statistic=ks$statistic, p.value=ks$p.value)
    setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
  })
  
  full.mat <- rbind(full.mat, res)
}


## View rounded matrices
round(full.mat, 2)
p.mat <- round(p.mat, 2)
d.mat <- round(d.mat, 2)

#install.packages("plot.matrix")
library('plot.matrix')
plot(d.mat, main="KS test - D stat matrix", axis.col=list(side=1, las=2), axis.row = list(side=2, las=1), cex.axis = .5, col=topo.colors)
plot(p.mat, main="KStest - P value matrix", axis.col=list(side=1, las=2), axis.row = list(side=2, las=1), cex.axis = .5)
