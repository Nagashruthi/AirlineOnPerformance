newinp<-read.csv("F:/SJSU SE/CMPE 239/Project/239/Data/newcombined.csv")
newinp = na.omit(newinp)
newinp = newinp[, -20:-29]
str(newinp)
library(arules)
library(arulesViz)

 input_allyrs= newinp

# order the numeric data and convert to range

 input_allyrs$ArrDelay = ordered(cut(input_allyrs$ArrDelay, c(0, 25, 55, 85, Inf)), labels = c("On-Scheduled","Delayed", "Medium-Delayed", "Lot-of-Delay"))
 input_allyrs$DepDelay = ordered(cut(input_allyrs$DepDelay, c(0, 5, 23, 35, Inf)), labels = c("On-Scheduled","Delayed", "Medium-Delayed", "Lot-of-Delay"))
 input_allyrs$AirTime = ordered(cut(input_allyrs$AirTime, c(-1, 50, 100, 200, 300, Inf)), labels = c("Too-Short","Short", "Intermediate", "Long", "Too-Long"))
 input_allyrs$CRSDepTime = ordered(cut(input_allyrs$CRSDepTime, c(-1, 600, 1200, 1800, Inf)),labels = c("Night-Overnight", "Morning", "Afternoon", "Evening"))
 input_allyrs$CRSArrTime = ordered(cut(input_allyrs$CRSArrTime, c(-1, 600, 1200, 1800, 2359)),labels = c("Night-Overnight", "Morning", "Afternoon", "Evening"))
 input_allyrs$DayOfWeek = as.character(input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^1", "Sunday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^2", "Monday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^3", "Tuesday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^4", "Wednesday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^5", "Thursday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^6", "Friday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = gsub("^7", "Saturday", input_allyrs$DayOfWeek)
 input_allyrs$DayOfWeek = factor(input_allyrs$DayOfWeek)
 log = !(names(input_allyrs) %in% c("DayofMonth", "FlightNum", "Cancelled", "ActualElapsedTime","DepDelay", "UniqueCarrier"))
 input_allyrs.AR = input_allyrs[, log]

 input_allyrs.AR = na.omit(input_allyrs.AR)
 summary(input_allyrs.AR)

 input_allyrs.AR= input_allyrs.AR[,-1]


 names(input_allyrs.AR)
 
 input_allyrs.AR= input_allyrs.AR[,-2]
 names(input_allyrs.AR)
 input_allyrs.AR= input_allyrs.AR[,-3]
 input_allyrs.AR= input_allyrs.AR[,-5]
 rules <- apriori(input_allyrs.AR, parameter = list(supp = 0.001, conf = 0.8))
 options(digits=2)
 inspect(rules[1:5])
 input_allyrs.AR= input_allyrs.AR[,-5]
 rules <- apriori(input_allyrs.AR, parameter = list(supp = 0.001, conf = 0.8))
 options(digits=2)
 inspect(rules[1:5])
 summary(rules)
 rules<-sort(rules, by="confidence", decreasing=TRUE)
 inspect(rules[1:10])

 #plot graph for the above rules
plot(rules[1:10], method = "graph", control = list(type = "items"),interactive=TRUE)

#Calculate delay for fridays
 rules_delay_1 = subset(rules_delay, subset = lhs %in% "DayOfWeek=Friday")
 rules_delay_1<-sort(rules_delay_1, by="confidence", decreasing=TRUE)
 inspect(rules_delay_1[1:5])

 #plot graph for the above rules
plot(rules_delay_1, method = "graph", control = list(type = "items"))
plot(rules_delay_1)
plot(rules_delay_1,method="paracoord")
plot(rules_delay_1[1:10], method = "graph", control = list(type = "items"),interactive=TRUE)

#Arrdelay
 log = !(names(newinp) %in% c("Month","DayofMonth","DepTime","ArrTime","TailNum", "FlightNum", "LateAircraftDelay", "ActualElapsedTime","IsDepDelayed","IsArrDelayed")
+ )

 inp.AR.arr = newinp[, log]
 inp.AR.arr = na.omit(inp.AR.arr)
 names(inp.AR.arr)
 summary(inp.AR.arr)
 inp.AR.arr = inp.AR.arr[,-5]
 inp.AR.arr = inp.AR.arr[,-7]

> rules_arr <- apriori(inp.AR,parameter = list(minlen=2, supp=0.005, conf=0.1),appearance = list(rhs=c("ArrDelay=Lot-of-Delay"),default="lhs"),control = list(verbose=F))


#sort rules
rules_arr<-sort(rules_arr, by="confidence", decreasing=TRUE)
inspect(rules_arr[1:5])

#plot graph for the above rules
plot(rules_arr[1:10], method = "graph", control = list(type = "items"),interactive=TRUE)
plot(rules_arr[1:10])
plot(rules_arr[1:20],method="paracoord")

#DepDelay
 log = !(names(newinp) %in% c("Month","DayofMonth","DepTime","ArrTime","TailNum", "FlightNum", "LateAircraftDelay", "ActualElapsedTime","IsDepDelayed","IsArrDelayed","ArrDelay")
+ )
 inp.AR.dep = newinp[, log]
 inp.AR.dep = na.omit(inp.AR.dep)
 names(inp.AR.dep)
 summary(inp.AR.dep)
rules_dep <- apriori(inp.AR.dep,parameter = list(minlen=2, supp=0.005, conf=0.1),appearance = list(rhs=c("DepDelay=Lot-of-Delay"),default="lhs"),control = list(verbose=F))

#sort rules
rules_dep<-sort(rules_arr, by="confidence", decreasing=TRUE)

#plot graph for the above rules
plot(rules_dep)
plot(rules_dep[1:20])
plot(rules_dep[1:10],method="paracoord")
plot(rules_dep[1:10], method = "graph", control = list(type = "items"),interactive=TRUE)




