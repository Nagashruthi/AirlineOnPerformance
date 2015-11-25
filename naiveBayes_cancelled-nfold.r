# for Naive Bayes, we want to use categorial predictors where we can,
# as for them the output is more informative
m.nb = naiveBayes(input[,c(2:6,7:8)],input$flight_iscancelled)
temp = input
# add a new column that assigns each row a number from 1 to 10, cutting the data up equally
temp$fold = cut(1:nrow(temp), breaks=5, labels=F)
#here are the folds we got:
unique(temp$fold)

nb.accuracies = c()

for (i in 1:10) {
  m.nbi = naiveBayes(temp[temp$fold != i,c(2:6,7:8)], 
    temp[temp$fold != i,]$flight_iscancelled)

  predictions = predict(m.nbi, temp[temp$fold == i, c(2:6,7:8)])

  numcorrect = sum(predictions == temp[temp$fold ==
    i,]$flight_iscancelled)

  nb.accuracies = append(numcorrect / nrow(temp[temp$fold == i,]), nb.accuracies)
}

nb.accuracies
mean(nb.accuracies)
