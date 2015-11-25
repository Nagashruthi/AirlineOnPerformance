input <- read.csv("/Users/vidya/Desktop/Nisha/SJSU/CMPE-239/combined.csv")
head(input)
names(input)
input = input[,-11]
names(input)
input = input[,-16]
input = input[,-16]
names(input)
diff_depTime = ifelse((input$DepTime - input$CRSDepTime) > 15, "Yes", "No")
input = data.frame(input,diff_depTime)
names(input)
set.seed(2)
train = sample(1:nrow(input), nrow(input)/2)
test = -train
training_data = input[train,]
test_data = input[test,]
testing_diffTime = diff_depTime[test]
tree_model = tree(diff_depTime~., training_data)
plot(tree_model)
text(tree_model,pretty = 0)
tree_pred = predict(tree_model, test_data, type = "class")
mean(tree_pred != testing_diffTime, na.rm = TRUE)
tail(input)
head(input)
