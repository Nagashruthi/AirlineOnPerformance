# for Naive Bayes
m.nb = naiveBayes(input[,c(2:5,6:9)],input$diff_arrTime)
temp = input
# add a new column that assigns each row a number from 1 to 10 for 10 fold and 1 to 5 for 5 fold, cutting the data up equally
temp$fold = cut(1:nrow(temp), breaks=5, labels=F)
#here are the folds we got:
unique(temp$fold)

nb.accuracies = c()

for (i in 1:10) {
  m.nbi = naiveBayes(temp[temp$fold != i,c(2:5,6:9)], 
    temp[temp$fold != i,]$diff_arrTime)

  predictions = predict(m.nbi, temp[temp$fold == i, c(2:5,6:9)])

  numcorrect = sum(predictions == temp[temp$fold ==
    i,]$diff_arrTime)

  nb.accuracies = append(numcorrect / nrow(temp[temp$fold == i,]), nb.accuracies)
}

nb.accuracies
mean(nb.accuracies)
