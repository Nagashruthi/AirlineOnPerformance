# for Naive Bayes
m.nb = naiveBayes(input[,c(2:5,7:8)],input$diff_depTime)
temp = input
# add a new column that assigns each row a number from 1 to 10 for 10 fold and 1 to 5 for 5 fold, cutting the data up equally
temp$fold = cut(1:nrow(temp), breaks=5, labels=F)
#here are the folds we got:
unique(temp$fold)

nb.accuracies = c()

for (i in 1:10) {
  m.nbi = naiveBayes(temp[temp$fold != i,c(2:7,8:12)], 
    temp[temp$fold != i,]$diff_depTime)

  predictions = predict(m.nbi, temp[temp$fold == i, c(2:7,8:12)])

  numcorrect = sum(predictions == temp[temp$fold ==
    i,]$diff_depTime)

  nb.accuracies = append(numcorrect / nrow(temp[temp$fold == i,]), nb.accuracies)
}

nb.accuracies
mean(nb.accuracies)
