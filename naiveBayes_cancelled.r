library(e1071)
input <- read.csv("/Users/vidya/Desktop/Nisha/SJSU/CMPE-239/combined.csv")
names(input)

#Use only attributes Month, DayofMonth, DayOfWeek, Dest, Cancelled and remove  other attibutes using below command
input = input[,-5]
flight_iscancelled = ifelse(input$Cancelled == 1, "Yes", "No")

#Remove Cancelled column from the data frame and add flight_iscancelled to data frame
input = data.frame(input,flight_iscancelled)
model <- naiveBayes( input$flight_iscancelled~ ., data = input)

#predicting for first 100000 rows
predict(model, input[1:100000,-1])
predict(model, input[1:100000,-1], type = "raw")

#prediction for whole data set
prediction <- predict(model, input[,-1])

tab_val <- tab_val(prediction, input$flight_iscancelled)

#display confusion matrix and accuracy
tab_val    
sum(tab_val[row(tab_val)==col(tab_val)])/sum(tab_val)

#Using laplace smoothing
model <- naiveBayes(input$flight_iscancelled ~ ., data = input, laplace = 3)
prediction <- predict(model, input[,-1])
tab_val <- tab_val(pred, input$flight_iscancelled)
tab_val
sum(tab_val[row(tab_val)==col(tab_val)])/sum(tab_val)
