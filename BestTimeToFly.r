#This program is to predict which is the best time to fly in a day 
library(biganalytics)
library(bigmemory)
library(ggplot2)
library(reshape2)
library(foreach)

#Writing data to big matrix
x <- read.big.matrix("/Users/vidya/Desktop/Nisha/SJSU/CMPE-239/combined.csv", header = TRUE, backingfile = "airline.bin", descriptorfile = "airline.desc", type = "integer", extraCols = "age")
dim(x)
x[1:6,1:6]

#Considering only year 2006 data
sum(x[,"Year"] == 2006)
sum(x[,"DayOfWeek"] == 6)
y <- attach.big.matrix("airline.desc")
mat1 <- big.matrix(nrow = 3, ncol = 3, type = "integer", init = 0)
mat2 <-mat1
mat2[1,1] <- 1
dayCount = integer(7)
#Splitting the days
for (i in 1:7)
dayCount[i] <- sum(x[,"DayOfWeek"] == i)

dayCount <- foreach(i = 1:7, .combine = c) %do% {sum(x[,"DayOfWeek"] == i)}
DaysName <- split(1:nrow(x), x[,"DayOfWeek"])

#Customizing the names of days
names(DaysName) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
DaysName$Mon[1:10]
DaysName$Tue[1:10]
dayCount <- foreach(dayInds = DaysName, .combine = c) %do% { length(dayInds)}

departureHours <- floor(x[, "CRSDepTime"]/100)
departureHours[departureHours == 24] <- 0
hourIndex <- split(1:length(departureHours), departureHours)

#Different Qantiles
probVal <- c(0.9,0.99,0.999,0.9999)

#Computing for every quantile sequentially
delayQuant <- foreach(hour = hourIndex, .combine = cbind) %do% { require(bigmemory) x<- attach.big.matrix("airline.desc") quantile(x[hour,"DepDelay"], probVal, na.rm = TRUE)}
delayQuant <- foreach(hour = hourIndex, .combine = cbind) %do% {
x<- attach.big.matrix("airline.desc")
quantile(x[hour,"DepDelay"], probVal, na.rm = TRUE)}
colnames(delayQuant) <- names(hourIndex)
Quant <- melt(delayQuant)
names(Quant) <- c("percentile","hour","delay")
qplot(hour,delay,data = Quant, color = percentile, geom = "line")
