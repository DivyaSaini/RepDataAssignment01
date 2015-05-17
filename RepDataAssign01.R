filename <- "./data/activity.csv"
mydata <- read.csv(filename, colClasses = c("integer", "Date", "factor"))
nonNAdata <- na.omit(mydata)
rownames(nonNAdata) <- 1:nrow(nonNAdata)
dim(mydata) # [1] 17568     4
dim(nonNAdata) # [1] 15264     4
